# A report's steps of its own code (R/step_code.R): data_code / content_code
# on the report sheet, the program around them, the migration of the old
# "user" type and the report list's data code, the app's switch

with_catalog <- function(p = new_planner()) {
  set_ard_rows(p, "datasets", "", data.frame(
    dataset = c("ADSL", "ADVS"), path = c("data/adam/adsl.rds", "data/adam/advs.rds")))
}

test_that("a step's code and the report's datasets are kept where the spec has them", {
  p <- add_output(with_catalog(), "F-1", type = "figure")
  expect_true(is.na(step_code(p, "F-1", "content")))
  p <- set_step_code(p, "F-1", "content", "content <- 1\r\n\n")
  expect_identical(step_code(p, "F-1", "content"), "content <- 1")
  expect_identical(p$sheets$report$content_code[p$sheets$report$output_id == "F-1"], "content <- 1")
  # blank: the definition again
  expect_true(is.na(step_code(set_step_code(p, "F-1", "content", "  "), "F-1", "content")))
  p <- set_report_datasets(p, "F-1", c("adsl", "ADVS", "ADSL"))
  expect_identical(report_datasets(p, "F-1"), c("ADSL", "ADVS"))
  expect_identical(.report_datasets(p, "F-1"), c("ADSL", "ADVS"))
  # three types only
  expect_identical(report_types(), c("table", "listing", "figure"))
  expect_error(add_output(p, "U-1", type = "user"))
})

test_that("a figure whose content is its code: its datasets read, its code, report_content()", {
  p <- add_output(with_catalog(), "F-1", type = "figure")
  p <- set_report_datasets(p, "F-1", c("ADSL", "ADVS"))
  p <- set_step_code(p, "F-1", "content", "content <- ggplot2::ggplot(advs)")
  code <- program_code(p, "F-1")
  expect_true(any(grepl("^adsl <- ", code)) && any(grepl("^advs <- ", code)))
  expect_true("content <- ggplot2::ggplot(advs)" %in% code)
  expect_true("content <- report_content(content)" %in% code)
  expect_true(any(grepl("source(\"programs/tfl/fig_setup.R\")", code, fixed = TRUE)))
  expect_true(any(grepl("rtf_figures(content", code, fixed = TRUE)))
  # report_content() is fig_setup.R's, not each program's
  expect_false(any(grepl("report_content <- function", code, fixed = TRUE)))
  expect_silent(parse(text = code))
  # no ARD unless the report reads one; its own analyses as `ard`
  expect_false(any(grepl("this report's ARD", code, fixed = TRUE)))
  q <- set_fig_ard_source(p, "F-1", "own")
  expect_true(any(grepl("this figure's ARD", program_code(q, "F-1"), fixed = TRUE)))
  q <- use_imported_ard(p, "F-1", "f1.rds")
  expect_true(any(grepl("input/ard/f1.rds", program_code(q, "F-1"), fixed = TRUE)))
})

test_that("a table: its data step's code (ard normalized), its content step's code", {
  p <- add_output(with_catalog(), "T-1")
  p <- set_report_datasets(p, "T-1", "ADSL")
  p <- set_step_code(p, "T-1", "data", "ard <- cards::ard_continuous(adsl, variables = AGE)")
  code <- program_code(p, "T-1")
  expect_true("ard <- cards::ard_continuous(adsl, variables = AGE)" %in% code)
  expect_true("data <- normalize_ard(ard)" %in% code)
  expect_true("data <- report_data(data, \"data\")" %in% code)
  # its content as the table definition makes it (the plan)
  expect_true(any(grepl("table_plan(", code, fixed = TRUE)))
  expect_true(any(grepl("saveRDS(data", code, fixed = TRUE)))
  expect_silent(parse(text = code))
  # the content its code: no plan, `content` to the report
  p <- set_step_code(p, "T-1", "content", "content <- head(data)")
  code <- program_code(p, "T-1")
  expect_false(any(grepl("table_plan(", code, fixed = TRUE)))
  expect_true(any(grepl("rtf_tables(content", code, fixed = TRUE)))
  expect_silent(parse(text = code))
})

test_that("a listing: its data step's code, then its columns as defined", {
  p <- add_output(with_catalog(), "L-1", type = "listing")
  p <- set_lf_rows(p, "listings", "L-1", data.frame(dataset = "ADSL", sort = "USUBJID"))
  p <- set_lf_rows(p, "listing_cols", "L-1", data.frame(vars = c("USUBJID", "AGE")))
  p <- set_report_datasets(p, "L-1", "ADSL")
  p <- set_step_code(p, "L-1", "data", "data <- adsl[adsl$AGE > 60, ]")
  code <- program_code(p, "L-1")
  expect_true("data <- adsl[adsl$AGE > 60, ]" %in% code)
  # its order and its columns on that data, not its dataset read again
  expect_true(any(grepl("data <- data |>", code, fixed = TRUE)))
  expect_true(any(grepl("listing_spec(", code, fixed = TRUE)))
  expect_identical(sum(grepl("^adsl <- ", code)), 1L)
  expect_silent(parse(text = code))
})

test_that("report_content() and report_data() check what the code left", {
  e <- new.env()
  eval(parse(text = .report_content_fun), e)
  rtfplot <- function(p) structure(list(), class = "rtfplot")
  environment(e$report_content) <- environment()
  g <- structure(list(), class = c("gg", "ggplot"))
  expect_s3_class(e$report_content(g), "rtfplot")
  out <- e$report_content(list(data.frame(a = 1), g))
  expect_s3_class(out[[2]], "rtfplot")
  expect_s3_class(out[[1]], "data.frame")
  expect_error(e$report_content(list(data.frame(a = 1), structure(list(), class = "lm"))),
               "`content` (item 2) is lm", fixed = TRUE)
  expect_error(e$report_content(1:3), "`content` is integer", fixed = TRUE)
  expect_identical(e$report_data(data.frame(a = 1)), data.frame(a = 1))
  # `data` not made: R's own data() function
  expect_error(e$report_data(utils::data, "data"), "leaves `data` as function", fixed = TRUE)
})

test_that("the old user type and data code become steps of the report's own code", {
  p <- with_catalog()
  p <- add_output(p, "F-1", type = "figure")
  p <- add_output(p, "T-1")
  p <- add_output(p, "T-2")
  p <- add_output(p, "L-1", type = "listing")
  p <- add_output(p, "F-2", type = "figure")
  p <- add_output(p, "F-3", type = "figure")
  r <- p$sheets$report
  # (a table has no report row of its own until something is set on it)
  r[nrow(r) + 1L, ] <- NA_character_
  r$output_id[nrow(r)] <- "T-1"
  r$type[r$output_id %in% c("F-1", "T-1")] <- "user"
  p$sheets$report <- r
  p <- set_lf_rows(p, "figures", "F-1", data.frame(datasets = "ADSL", ard = "TRUE"))
  p <- set_lf_rows(p, "listings", "L-1", data.frame(dataset = "ADSL"))
  p$fig_designs <- list(`F-3` = list(data = list()))
  old <- c(`F-1` = "plot <- ggplot2::ggplot(advs)", `T-1` = "content <- adsl",
           `T-2` = "ard <- my_ard()", `L-1` = "data$X <- 1",
           `F-2` = "plot <- ggplot2::ggplot(adsl)", `F-3` = "never used")
  q <- .migrate_step_code(p, old)
  # user: the type its ID says, its code the content step's, plot given as content
  expect_identical(report_info(q, "F-1")$type, "figure")
  expect_identical(report_info(q, "T-1")$type, "table")
  expect_identical(step_code(q, "F-1", "content"),
                   "plot <- ggplot2::ggplot(advs)\ntfl_check(plot)\ncontent <- plot")
  expect_identical(step_code(q, "T-1", "content"), "content <- adsl")
  # its datasets: the figures row's and those its code names; its ARD own
  expect_identical(report_datasets(q, "F-1"), c("ADSL", "ADVS"))
  expect_identical(report_datasets(q, "T-1"), "ADSL")
  expect_identical(.fig_ard_source(q, "F-1")$kind, "own")
  expect_identical(nrow(lf_rows(q, "figures", "F-1")), 0L)
  # a table's data code: its data step; a listing's: its rework
  expect_identical(step_code(q, "T-2", "data"), "ard <- my_ard()")
  expect_identical(q$outputs$process_code[q$outputs$output_id == "L-1"], "data$X <- 1")
  expect_true(is.na(step_code(q, "L-1", "data")))
  # a figure written by hand: the content step; a designed one's never ran
  expect_match(step_code(q, "F-2", "content"), "content <- plot$")
  expect_true(is.na(step_code(q, "F-3", "content")))
  # again: nothing changes
  expect_identical(.migrate_step_code(q, character()), q)
})

test_that("old workbooks that say type user are read, and saved as they now are", {
  d <- withr_tempdir()
  p <- add_output(with_catalog(), "F-1", type = "figure")
  write_planner(p, d)
  f <- file.path(d, "report_spec.xlsx")
  # as 0.0.2.9203 wrote it: type user, the code in the report list's data_code
  wb <- openxlsx::loadWorkbook(f)
  rep <- openxlsx::readWorkbook(wb, "report")
  rep$type[rep$output_id %in% "F-1"] <- "user"
  openxlsx::writeData(wb, "report", rep)
  meta <- openxlsx::readWorkbook(wb, "_tflplanner")
  names(meta)[names(meta) == "setup_code"] <- "data_code"
  meta$data_code[meta$output_id %in% "F-1"] <- "content <- advs"
  openxlsx::removeWorksheet(wb, "_tflplanner")
  openxlsx::addWorksheet(wb, "_tflplanner")
  openxlsx::writeData(wb, "_tflplanner", meta)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  q <- read_planner(file.path(d, c("table_spec.xlsx", "report_spec.xlsx")))
  expect_identical(report_info(q, "F-1")$type, "figure")
  expect_identical(step_code(q, "F-1", "content"), "content <- advs")
  expect_true("ADVS" %in% report_datasets(q, "F-1"))
  # the files themselves unchanged until saved
  expect_identical(openxlsx::readWorkbook(f, "report")$type[1L], "user")
  write_planner(q, d)
  back <- read_planner(file.path(d, c("table_spec.xlsx", "report_spec.xlsx")))
  expect_identical(step_code(back, "F-1", "content"), "content <- advs")
  expect_false("data_code" %in% names(openxlsx::readWorkbook(f, "_tflplanner")))
})

test_that("the review checks a step's code against the rules", {
  p <- add_output(with_catalog(), "F-1", type = "figure")
  p <- set_report_datasets(p, "F-1", "ADSL")
  p <- set_step_code(p, "F-1", "content", "x <- advs")
  r <- .rules_step_code(p)
  expect_setequal(r$rule, c("U02", "U03"))
  expect_true(all(r$output_id == "F-1"))
  # a figure with its code: something makes it (no R06)
  rr <- .review_planner(p)
  expect_false(any(rr$rule == "R06" & rr$output_id %in% "F-1"))
})

test_that("a report's own code runs out of the app; the line of an error is said", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("ggplot2")
  local_home()
  s <- create_study("UC")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(as.data.frame(cards::ADSL), file.path(s$path, "data/adam/adsl.rds"))
  p <- catalog_add_files(add_standard_defaults(s$planner, "UC"), study_files(s))
  p <- add_output(p, "F-1", type = "figure")
  p <- set_report_datasets(p, "F-1", "ADSL")
  p <- set_step_code(p, "F-1", "content", paste(
    "content <- list(head(adsl[, c(\"USUBJID\", \"AGE\")], 20),",
    "ggplot2::ggplot(adsl, ggplot2::aes(AGE)) + ggplot2::geom_histogram(bins = 5))"))
  p <- add_output(p, "F-2", type = "figure")
  p <- set_step_code(p, "F-2", "content", "x <- 1\ny <- 2\ncontent <- adsl")
  s$planner <- p
  save_study(s)
  res <- preview_code(s, "F-1")
  expect_null(res$error)
  parts <- .code_preview_parts(res$content)
  expect_length(parts$pages, 1L)
  expect_length(parts$figures, 1L)
  # no dataset registered: adsl is not there, on line 3 of its code
  res <- preview_code(s, "F-2")
  expect_match(res$error, "adsl")
  expect_identical(res$code_step, "content")
  expect_identical(res$code_line, 3L)
  shiny::testServer(server_for("UC"), {
    rv <- session$userData$rv
    session$setInputs(target = "F-1", nav = "make", step = "content", content_nav = "content")
    expect_identical(output$content_is_code, "yes")
    expect_identical(output$data_is_code, "no")
    # a figure with no design: its datasets above the data step; its ARD in
    # one place, the figure's ARD source
    expect_identical(output$data_inputs, "yes")
    expect_match(output$uc_inputs$html, "uc_ds", fixed = TRUE)
    expect_no_match(output$uc_inputs$html, "uc_ard\"", fixed = TRUE)
    expect_match(output$ard_kind_note$html, "fig_ard_kind", fixed = TRUE)
    # the editor writes the step's code
    session$setInputs(cc_code = "content <- adsl[1:5, ]")
    expect_identical(step_code(rv$p, "F-1", "content"), "content <- adsl[1:5, ]")
    session$setInputs(uc_ds = c("ADSL"))
    expect_identical(report_datasets(rv$p, "F-1"), "ADSL")
    session$setInputs(fig_ard_kind = "own")
    expect_identical(.fig_ard_source(rv$p, "F-1")$kind, "own")
    expect_match(output$uc_ard_state$html, "analyses", fixed = TRUE)
    session$setInputs(fig_ard_kind = "none", cc_run = 1)
    expect_match(output$cc_result$html, "It ran", fixed = TRUE)
    # the other report: its error's line, and the dataset to add
    session$setInputs(target = "F-2")
    session$setInputs(cc_run = 1)
    h <- output$cc_result$html
    expect_match(h, "Line 3 of the content step", fixed = TRUE)
    expect_match(h, "Add ADSL to the data it reads", fixed = TRUE)
    expect_match(output$cc_check$html, "adsl", fixed = TRUE)
    # back to the definition: asked, then the code goes
    session$setInputs(content_mode = "gui")
    expect_false(is.na(step_code(rv$p, "F-2", "content")))
    session$setInputs(mode_drop = 1)
    expect_true(is.na(step_code(rv$p, "F-2", "content")))
    # a table's data step as code: its editor shown, the switch says so
    session$setInputs(target = "F-1", step = "ard", data_mode = "code")
    expect_identical(output$data_is_code, "yes")
  })
})

test_that("the rules are seven, said in full where the switch is", {
  expect_length(.code_rules, 7L)
  h <- as.character(.code_rules_ui())
  expect_identical(lengths(regmatches(h, gregexpr("<li>", h))), 7L)
  expect_match(h, "U01-U05", fixed = TRUE)
})

test_that("the sample's figures of their own code: F-14-2-2 reads T-14-2-2's ARD as `ard`", {
  local_home()
  s <- suppressMessages(create_sample_study(run = FALSE))
  p <- s$planner
  expect_identical(.fig_ard_source(p, "F-14-2-2"), list(kind = "table", id = "T-14-2-2"))
  code <- unlist(strsplit(program_code(p, "F-14-2-2"), "
", fixed = TRUE))
  expect_true("risk_df <- tfl_km_risk(ard, km_fit)" %in% code)
  # the ARD read by the program (its source's rows), not by the code
  expect_true(any(grepl("filter(output_id == \"T-14-2-2\")", code, fixed = TRUE)))
  expect_false(any(grepl("km_ard", code, fixed = TRUE)))
  expect_true(any(grepl("ard_built <- ard_fingerprint", code, fixed = TRUE)))
  expect_true("record_report(report_id, ard = ard_built)" %in% code)
  expect_identical(.fig_ard_need(p, "F-14-2-2"), "T-14-2-2")
  expect_identical(nrow(.rules_step_code(p)), 0L)
})

test_that("report_content() is written once, in the study's fig_setup.R", {
  local_home()
  s <- suppressMessages(create_sample_study(run = FALSE))
  setup <- readLines(file.path(s$path, "programs", "tfl", "fig_setup.R"))
  expect_true(any(grepl("^report_content <- function", setup)))
  expect_true(any(grepl("^report_data <- function", setup)))
  prog <- readLines(file.path(s$path, "programs", "tfl", "F-14-2-1.R"))
  expect_true("content <- report_content(content)" %in% prog)
  expect_false(any(grepl("report_content <- function", prog, fixed = TRUE)))
  expect_identical(report_info(s$planner, "F-14-2-1")$type, "figure")
})

test_that("a step's code editor and the whole app UI build (in English and Japanese)", {
  # a start-up error (an argument shiny rejects, as an invalid CSS unit)
  # stops the app before anything is shown
  for (lang in c("en", "ja")) {
    t <- function(x) tr(x, lang)
    for (prefix in c("dc", "cc")) {
      ui <- expect_no_error(code_step_ui(prefix, t, lang))
      html <- as.character(ui)
      expect_match(html, "rp-code-step", fixed = TRUE)
      expect_match(html, paste0("id=\"", prefix, "_code\""), fixed = TRUE)
      expect_match(html, "rows=\"26\"", fixed = TRUE)
    }
    expect_no_error(as.character(app_ui(lang)))
  }
  # the editor's height: the app's style
  expect_match(as.character(app_ui()), ".rp-code-step textarea { height: max(", fixed = TRUE)
})
