# A figure's steps (#293, the user's figure feedback): step 1 "1 Data", its
# data steps (list and form) and the numbers from an ARD below them; step 2
# "2 Figure", the settings and the layers, laid out Design | Edit | Preview.

# (slow: its tests write study folders or start the app -- run on CI
# and locally with NOT_CRAN=true, not in CRAN's check)
skip_on_cran()

test_that("a figure's step names are its own; a table's stay", {
  expect_identical(.step_labels_of$figure[["ard"]], "1 Data")
  expect_identical(.step_labels_of$figure[["content"]], "2 Figure")
  expect_null(.step_labels_of$table)
  expect_false("ard" %in% .type_idle_tabs$figure)
})

test_that("the data steps are step 1's, the settings and layers step 2's", {
  local_home()
  p <- add_output(new_planner(), "F1", type = "figure")
  d <- tflspec::tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP")
  p <- set_fig_design(p, "F1", d)
  create_study("S1", planner = p)
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "F1", nav = "make", step = "ard")
    h1 <- output$pd_stack_data$html
    h2 <- output$pd_stack$html
    # step 1: the data steps and the code lists, not the settings
    expect_match(h1, "Read a dataset", fixed = TRUE)
    expect_match(h1, "Code lists", fixed = TRUE)
    expect_false(grepl("Title, axes, colours", h1, fixed = TRUE))
    # step 2: the settings and the layers, not the data steps
    expect_match(h2, "Title, axes, colours", fixed = TRUE)
    expect_false(grepl("Read a dataset", h2, fixed = TRUE))
    # the settings chosen (the default): step 2's form, step 1 says choose
    expect_match(output$pd_form_data$html, "Choose a data step", fixed = TRUE)
    # a data step chosen: its form in step 1, step 2's says choose
    session$setInputs(pd_act = list(op = "sel", sec = "data", i = 1L, n = 1))
    expect_match(output$pd_form$html, "Choose the figure settings", fixed = TRUE)
    expect_false(grepl("Choose a data step", output$pd_form_data$html, fixed = TRUE))
    expect_true(nzchar(output$pd_piece_code_data))
    expect_identical(output$pd_piece_code, "")
    # the code box only with the piece's form, not where nothing is chosen
    expect_match(output$pd_form_data$html, "pd_piece_code_data", fixed = TRUE)
    expect_false(grepl("pd_piece_code", output$pd_form$html, fixed = TRUE))
    # a data step added in step 1
    n0 <- length(fig_design(rv$p, "F1")$data)
    session$setInputs(pd_add_data = "filter", pd_addbtn_data = 1)
    expect_identical(length(fig_design(rv$p, "F1")$data), n0 + 1L)
  })
})

test_that("a figure without a design: step 1 says where to start it", {
  local_home()
  p <- add_output(new_planner(), "F1", type = "figure")
  create_study("S1", planner = p)
  shiny::testServer(server_for("S1"), {
    session$setInputs(target = "F1", nav = "make", step = "ard")
    expect_match(output$pd_data_body$html, "start it in step 2", fixed = TRUE)
  })
})

test_that("what the steps make: kept short by the preview, shown in step 1 on demand", {
  local_home()
  s <- suppressMessages(create_sample_study(run = FALSE))
  r <- preview_figure(s, "F-14-2-3", max_px = 600)
  # (its ARD not made yet: the steps before the ARD's still there)
  expect_identical(names(r$objects)[1:2], c("df", "fit"))
  expect_identical(r$objects$df$class, "data frame")
  expect_true(r$objects$df$rows > 0L)
  expect_lte(nrow(r$objects$df$head), 6L)
  expect_true(length(r$objects$fit$text) > 0L)
  expect_identical(.fig_design_objects(fig_design(s$planner, "F-14-2-4"))[1:2], c("df", "est"))
  shiny::testServer(server_for("SAMPLE-01"), {
    session$setInputs(target = "F-14-2-3", nav = "make", step = "ard")
    expect_match(output$pd_objects$html, "Not made yet", fixed = TRUE)
    session$setInputs(pd_data_preview = 1)
    h <- output$pd_objects$html
    expect_match(h, "data frame, ", fixed = TRUE)
    expect_match(h, "<code>fit</code>", fixed = TRUE)
  })
})

test_that("a time-unit step in days says why it writes no line; in months it writes one", {
  d <- tflspec::tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE", pop = "SAFFL",
                                 group = "TRT01A")
  i <- which(vapply(d$data, function(p) identical(p$step, "time_unit"), NA))
  make <- function(d, codelists = TRUE) .fig_design_script(d, "F1")
  d$data[[i]]$unit <- "days"
  expect_match(.piece_code(make(d), d, list(sec = "data", i = i), make), "in days already", fixed = TRUE)
  d$data[[i]]$unit <- "months"
  expect_match(.piece_code(make(d), d, list(sec = "data", i = i), make), "AVAL / days_per_month", fixed = TRUE)
})

test_that("a step's In and Out: on one line, what they are when not said as placeholders", {
  local_home()
  p <- add_output(new_planner(), "F1", type = "figure")
  d <- tflspec::tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP")
  d$data <- c(d$data, list(list(step = "function", fn = "survival::survfit")))
  p <- set_fig_design(p, "F1", d)
  create_study("S1", planner = p)
  shiny::testServer(server_for("S1"), {
    session$setInputs(target = "F1", nav = "make", step = "ard")
    # the PARAMCD step: In df, Out df (the object above it, changed)
    session$setInputs(pd_act = list(op = "sel", sec = "data", i = 2L, n = 1))
    h <- output$pd_form_data$html
    expect_match(h, ">In<", fixed = TRUE)
    expect_match(h, ">Out<", fixed = TRUE)
    expect_match(h, 'class="d-flex gap-2"', fixed = TRUE)
    expect_match(h, '"placeholder":"df"', fixed = TRUE)
    # any function: suggested functions, its arguments named under the field
    k <- length(fig_design(session$userData$rv$p, "F1")$data)
    session$setInputs(pd_act = list(op = "sel", sec = "data", i = k, n = 2))
    h2 <- output$pd_form_data$html
    expect_match(h2, "ggsurvfit::survfit2", fixed = TRUE)
    expect_match(h2, "Its arguments: formula*", fixed = TRUE)
  })
})

test_that("an ARD analysis step is the figure's row of the ARD definition", {
  p <- add_output(new_planner(), "F1", type = "figure")
  d <- tflspec::tfl_fig_design(data = list(
    list(step = "read", dataset = "ADTTE"),
    list(step = "ard", analysis_id = "KM", fn = "cardx::ard_survival_survfit",
         `in` = "adtte_saf", args = "times = c(90, 180)")))
  q <- .fig_ard_steps_rows(set_fig_design(p, "F1", d), "F1", d)
  r <- ard_rows(q, "analyses", "F1")
  expect_identical(r$analysis_id, "KM")
  expect_identical(r$method, "cardx::ard_survival_survfit")
  expect_identical(r$data, "adtte_saf")
  expect_identical(r$args, "times = c(90, 180)")
  expect_identical(.fig_ard_source(q, "F1")$kind, "own")
  # the row's other columns kept; the same steps write nothing again
  q$ard$analyses$label[q$ard$analyses$analysis_id %in% "KM"] <- "KM estimates"
  d$data[[2]]$args <- "times = c(90, 180, 365)"
  q2 <- .fig_ard_steps_rows(q, "F1", d)
  r2 <- ard_rows(q2, "analyses", "F1")
  expect_identical(r2$label, "KM estimates")
  expect_identical(r2$args, "times = c(90, 180, 365)")
  expect_identical(.fig_ard_steps_rows(q2, "F1", d), q2)
  # an analysis data of another report: copied into the figure
  p2 <- add_output(p, "T1")
  p2$ard$analysis_data <- .normalize_ard_sheet(
    data.frame(output_id = "T1", data_id = "adtte_saf", from = "ADTTE"), "analysis_data")
  q3 <- .fig_ard_steps_rows(set_fig_design(p2, "F1", d), "F1", d)
  expect_true("adtte_saf" %in% .adata_rows(q3, "F1")$data_id)
  expect_identical(ard_rows(q3, "analyses", "F1")$data, "adtte_saf")
  # a figure that reads a table's ARD keeps it
  pt <- set_fig_ard_source(add_output(p, "T9"), "F1", "table:T9")
  qt <- .fig_ard_steps_rows(set_fig_design(pt, "F1", d), "F1", d)
  expect_identical(.fig_ard_source(qt, "F1")$kind, "table")
  expect_identical(ard_rows(qt, "analyses", "F1")$analysis_id, "KM")
  # a step without its function yet: nothing written
  d0 <- tflspec::tfl_fig_design(data = list(list(step = "ard", analysis_id = "X")))
  expect_identical(.fig_ard_steps_rows(p, "F1", d0), p)
})

test_that("a user-code figure's steps are a figure's", {
  local_home()
  p <- add_output(new_planner(), "F-9", type = "user")
  create_study("S1", planner = p)
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "F-9", nav = "make", step = "content")
    # (an F ID: a figure; nothing asks what it makes)
    expect_identical(.user_makes("F-9"), "figure")
    expect_no_match(output$uc_inputs$html, "uc_output", fixed = TRUE)
  })
})
