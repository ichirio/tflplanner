# A figure's size on its page (step 3): the report sheet's
# figure_width_in / figure_height_in -- blank its own size, fit the page's
# body, or inches; the program draws it so, step 3's form sets it, its
# first page shows it

test_that("a figure's program draws it at its size on the page", {
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- set_report_datasets(p, "F-1", "ADSL")
  p <- set_step_code(p, "F-1", "content", "content <- ggplot2::ggplot(adsl)")
  code <- program_code(p, "F-1")
  # blank: its own size (a figure of its own code: rtfreporter's 6.5 x 4.5),
  # placed as rtfreporter places it
  sized <- code[startsWith(code, "content <- fig_sized(content, ")]
  expect_length(sized, 1L)
  expect_true(grepl("width_in = 6.5, height_in = 4.5,", sized, fixed = TRUE))
  expect_true("  rtf_figures(content)" %in% code)
  expect_true(any(grepl("6.5 x 4.5 in (its own size)", code, fixed = TRUE)))
  # fit: the page's body (letter landscape, margins 0.75): 9.5 wide
  f <- set_page_cell(p, "report", "figure_width_in", "F-1", "fit")
  expect_identical(.fig_size_of(f, "F-1")$width_in, 9.5)
  expect_identical(.fig_size_of(f, "F-1")$mode[["width"]], "fit")
  expect_true(any(grepl("rtf_figures(content, width_twips = 13680", program_code(f, "F-1"), fixed = TRUE)))
  # inches: that
  q <- set_page_cell(p, "report", "figure_width_in", "F-1", "6")
  expect_identical(.fig_size_of(q, "F-1")$width_in, 6)
  expect_true(any(grepl("content <- fig_sized(content, width_in = 6,", program_code(q, "F-1"), fixed = TRUE)))
  expect_true(any(grepl("rtf_figures(content, width_twips = 8640", program_code(q, "F-1"), fixed = TRUE)))
  expect_silent(parse(text = program_code(q, "F-1")))
})

test_that("a designed figure's own size: the company's figure style for its kind", {
  st <- data.frame(type = c(NA, "km"), key = c("width", "width"), value = c("7.3", "8.33"))
  local_mocked_bindings(.std_fig_style = function() list(settings = st))
  p <- add_output(new_planner(), "F-1", type = "figure")
  kind <- tflspec::tfl_fig_templates()
  km <- kind$template[kind$kind %in% "km"][1L]
  local_mocked_bindings(fig_design = function(x, output_id) list(template = km))
  expect_identical(.fig_native_size(p, "F-1"), c(8.33, 4.5))
  local_mocked_bindings(fig_design = function(x, output_id) list(template = "none"))
  expect_identical(.fig_native_size(p, "F-1"), c(7.3, 4.5))
  # no design: rtfreporter's
  local_mocked_bindings(fig_design = function(x, output_id) NULL)
  expect_identical(.fig_native_size(p, "F-1"), c(6.5, 4.5))
})

test_that("fig_sized() draws a ggplot at the size given, and leaves the rest", {
  e <- new.env()
  eval(parse(text = .report_content_fun), e)
  calls <- list()
  rtfplot <- function(p, ...) {
    calls[[length(calls) + 1L]] <<- list(...)
    structure(list(), class = "rtfplot")
  }
  environment(e$fig_sized) <- environment()
  g <- structure(list(), class = c("gg", "ggplot"))
  expect_s3_class(e$fig_sized(g, 6, 4, 150), "rtfplot")
  expect_identical(calls[[1L]]$render_width, 6)
  expect_identical(calls[[1L]]$height_twips, 5760)
  out <- e$fig_sized(list(g, data.frame(a = 1)), 6, 4)
  expect_s3_class(out[[1L]], "rtfplot")
  expect_s3_class(out[[2L]], "data.frame")
  expect_s3_class(e$fig_sized(data.frame(a = 1), 6, 4), "data.frame")
})

test_that("step 3 of a figure: its size, and its first page says where its figure comes from", {
  skip_on_cran()
  local_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- set_step_code(p, "F-1", "content", "content <- 1")
  p <- add_output(p, "T-1")
  create_study("FS", planner = p)
  shiny::testServer(server_for("FS"), {
    rv <- session$userData$rv
    session$setInputs(target = "F-1", nav = "make", step = "page")
    h <- output$page_form$html
    expect_match(h, "data-col=\"figure_width_in\"", fixed = TRUE)
    expect_match(h, "(its own: 6.5)", fixed = TRUE)
    expect_match(h, "data-act=\"fig_fit\"", fixed = TRUE)
    expect_no_match(h, "data-act=\"fig_native\"", fixed = TRUE)
    expect_match(output$page_sample$html, "Run the code", fixed = TRUE)
    session$setInputs(pg_edit = list(kind = "cell", sheet = "report", col = "figure_width_in",
                                     value = "6", target = "F-1"))
    expect_identical(page_value_from(rv$p, "report", "figure_width_in", "F-1")$value, "6")
    # fit to the page: both fit; its own size: both blank again
    session$setInputs(pg_act = list(act = "fig_fit", target = "F-1", sheet = "report", col = ""))
    expect_identical(page_value_from(rv$p, "report", "figure_width_in", "F-1")$value, "fit")
    expect_identical(page_value_from(rv$p, "report", "figure_height_in", "F-1")$value, "fit")
    expect_match(output$page_form$html, "data-act=\"fig_native\"", fixed = TRUE)
    session$setInputs(pg_act = list(act = "fig_native", target = "F-1", sheet = "report", col = ""))
    expect_false(identical(page_value_from(rv$p, "report", "figure_width_in", "F-1")$from, "own"))
    # a table: no figure size
    session$setInputs(target = "T-1")
    expect_no_match(output$page_form$html, "figure_width_in", fixed = TRUE)
  })
})
