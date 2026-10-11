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
