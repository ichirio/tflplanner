test_that("a user-code report's program reads its data and leaves content", {
  p <- new_planner()
  p <- set_ard_rows(p, "datasets", "", data.frame(dataset = "ADSL",
                                                  path = "data/adam/adsl.rds"))
  p <- add_output(p, "U-1", type = "user", data_code = "content <- adsl")
  p <- set_lf_rows(p, "figures", "U-1", data.frame(datasets = "ADSL"))
  expect_identical(report_info(p, "U-1")$type, "user")
  code <- program_code(p, "U-1")
  expect_true(any(grepl("adsl <- ", code, fixed = TRUE)))
  expect_true(any(code == "content <- adsl"))
  expect_true(any(code == "content <- .user_content(content)"))
  expect_true(any(grepl("rtf_tables(doc, content", code, fixed = TRUE)))
  expect_silent(parse(text = code))
  # no ARD unless the report says so
  expect_false(any(grepl("this report's ARD", code, fixed = TRUE)))
  p2 <- set_lf_rows(p, "figures", "U-1", data.frame(datasets = "ADSL", ard = "TRUE"))
  expect_true(.user_reads_ard(p2, "U-1"))
  # no code yet: a TODO that stops
  p3 <- add_output(p, "U-2", type = "user")
  expect_true(any(grepl("still to be written", program_code(p3, "U-2"), fixed = TRUE)))
})

test_that("the program turns a ggplot into a figure, alone or in a list", {
  e <- new.env()
  eval(parse(text = .user_content_fun), e)
  rtfplot <- function(p) structure(list(), class = "rtfplot")
  environment(e$.user_content) <- environment()
  g <- structure(list(), class = c("gg", "ggplot"))
  expect_s3_class(e$.user_content(g), "rtfplot")
  out <- e$.user_content(list(data.frame(a = 1), g))
  expect_s3_class(out[[2]], "rtfplot")
  expect_s3_class(out[[1]], "data.frame")
  expect_s3_class(e$.user_content(data.frame(a = 1)), "data.frame")
})

test_that("a figure written by hand becomes a user-code report when asked", {
  p <- add_output(new_planner(), "F-1", type = "figure",
                  data_code = "plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE))")
  p <- add_output(p, "F-2", type = "figure",
                  data_code = "content <- list(1)")
  expect_identical(.hand_figures(p), c("F-1", "F-2"))
  q <- make_user_report(p, "F-1")
  expect_identical(report_info(q, "F-1")$type, "user")
  expect_match(q$outputs$data_code[1], "content <- plot$")
  # code that leaves content is kept as it is
  q <- make_user_report(q, "F-2")
  expect_identical(q$outputs$data_code[2], "content <- list(1)")
  expect_identical(.hand_figures(q), character())
  expect_error(make_user_report(q, "F-1"), "not a figure written by hand")
})

test_that("a user-code report runs out of the app and its content is shown", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("ggplot2")
  local_home()
  s <- create_study("UC")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(as.data.frame(cards::ADSL), file.path(s$path, "data/adam/adsl.rds"))
  p <- catalog_add_files(add_standard_defaults(s$planner, "UC"), study_files(s))
  p <- add_output(p, "U-1", type = "user",
                  data_code = paste("content <- list(head(adsl[, c(\"USUBJID\", \"AGE\")], 20),",
                                    "ggplot2::ggplot(adsl, ggplot2::aes(AGE)) + ggplot2::geom_histogram(bins = 5))"))
  p <- set_lf_rows(p, "figures", "U-1", data.frame(datasets = "ADSL"))
  p <- add_output(p, "F-1", type = "figure",
                  data_code = "plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE)) + ggplot2::geom_histogram(bins = 5)")
  p <- set_lf_rows(p, "figures", "F-1", data.frame(datasets = "ADSL"))
  s$planner <- p
  save_study(s)
  res <- preview_user(s, "U-1")
  expect_null(res$error)
  parts <- .user_preview_parts(res$content)
  expect_length(parts$pages, 1L)
  expect_length(parts$figures, 1L)
  shiny::testServer(server_for("UC"), {
    rv <- session$userData$rv
    # the figure written by hand is offered, not converted
    expect_match(output$uc_offer$html, "F-1", fixed = TRUE)
    expect_identical(report_info(rv$p, "F-1")$type, "figure")
    session$setInputs(target = "U-1", nav = "make", step = "content", content_nav = "content")
    expect_match(output$uc_inputs$html, "uc_ard", fixed = TRUE)
    # the code is the report's data code: an edit here is written there
    session$setInputs(uc_code = "content <- adsl[1:5, ]")
    expect_identical(rv$p$outputs$data_code[rv$p$outputs$output_id == "U-1"],
                     "content <- adsl[1:5, ]")
    session$setInputs(uc_ard = TRUE)
    expect_true(.user_reads_ard(rv$p, "U-1"))
    # the switch keeps the datasets
    expect_identical(lf_rows(rv$p, "figures", "U-1")$datasets, "ADSL")
    session$setInputs(uc_ard = FALSE, uc_run = 1)
    expect_match(output$uc_result$html, "It ran", fixed = TRUE)
    # converted when asked, and confirmed
    session$setInputs(uc_convert_all = 1)
    session$setInputs(uc_convert_ok = 1)
    expect_identical(report_info(rv$p, "F-1")$type, "user")
    # the offer is gone
    expect_false(grepl("F-1", tryCatch(output$uc_offer$html %||% "", error = function(e) ""),
                       fixed = TRUE))
  })
})

test_that("content the contract does not name stops, saying which item it is", {
  e <- new.env()
  eval(parse(text = .user_content_fun), e)
  rtfplot <- function(p) structure(list(), class = "rtfplot")
  environment(e$.user_content) <- environment()
  expect_error(e$.user_content(list(data.frame(a = 1), structure(list(), class = "lm"))),
               "`content` (item 2) is lm", fixed = TRUE)
  expect_error(e$.user_content(1:3), "`content` is integer", fixed = TRUE)
})

test_that("a user-code report's ARD: its analyses, or the one taken in", {
  p <- new_planner()
  p <- set_ard_rows(p, "datasets", "", data.frame(dataset = "ADSL",
                                                  path = "data/adam/adsl.rds"))
  p <- add_output(p, "U-1", type = "user", data_code = "content <- ard")
  p <- set_lf_rows(p, "figures", "U-1", data.frame(datasets = "ADSL", ard = "TRUE"))
  p <- set_ard_rows(p, "analyses", "U-1", data.frame(
    analysis_id = "AGE", method = "continuous", dataset = "ADSL", variables = "AGE"))
  code <- program_code(p, "U-1")
  expect_true(any(grepl("this report's ARD", code, fixed = TRUE)))
  p2 <- use_imported_ard(p, "U-1", "u1.rds")
  expect_true(any(grepl("input/ard/u1.rds", program_code(p2, "U-1"), fixed = TRUE)))
})

test_that("a user-code report's run names the line of its code and a missing dataset", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("UE")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(as.data.frame(cards::ADSL), file.path(s$path, "data/adam/adsl.rds"))
  p <- catalog_add_files(add_standard_defaults(s$planner, "UE"), study_files(s))
  # no datasets chosen: adsl is not there
  p <- add_output(p, "U-1", type = "user",
                  data_code = "x <- 1\ny <- 2\ncontent <- adsl")
  # a table sheet's own row: not used
  p <- set_sheet_rows(p, "tables", "U-1", data.frame(cols = "TRT01A"))
  s$planner <- p
  save_study(s)
  res <- preview_user(s, "U-1")
  expect_match(res$error, "adsl")
  expect_identical(res$code_line, 3L)
  shiny::testServer(server_for("UE"), {
    session$setInputs(target = "U-1", nav = "make", step = "content", content_nav = "content")
    session$setInputs(uc_run = 1)
    h <- output$uc_result$html
    expect_match(h, "Line 3 of the code", fixed = TRUE)
    expect_match(h, "Add ADSL to the data it reads", fixed = TRUE)
    expect_match(output$uc_note$html, "not used", fixed = TRUE)
    # the ARD switch says there is none yet, and where to make one
    session$setInputs(uc_ard = TRUE)
    expect_match(output$uc_ard_state$html, "No ARD yet", fixed = TRUE)
    # the ARD tab of a report that does not read one says so
    session$setInputs(uc_ard = FALSE)
    expect_match(output$ard_kind_note$html, "does not read an ARD", fixed = TRUE)
  })
})

test_that("converting asks first, and Later holds while the study is open", {
  skip_on_cran()
  local_home()
  p <- add_output(new_planner(), "F-1", type = "figure",
                  data_code = "plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE))")
  p <- add_output(p, "F-2", type = "figure",
                  data_code = "plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE))")
  create_study("UL", planner = p)
  shiny::testServer(server_for("UL"), {
    rv <- session$userData$rv
    expect_match(output$uc_offer$html, "F-1, F-2", fixed = TRUE)
    session$setInputs(uc_convert_all = 1)
    # nothing yet: asked first
    expect_identical(report_info(rv$p, "F-1")$type, "figure")
    session$setInputs(uc_later = 1)
    # one converted from its own screen: the offer stays away
    session$setInputs(target = "F-1", fig_to_user = 1)
    session$setInputs(uc_convert_ok = 1)
    expect_identical(report_info(rv$p, "F-1")$type, "user")
    expect_identical(report_info(rv$p, "F-2")$type, "figure")
    expect_error(output$uc_offer)
  })
})
