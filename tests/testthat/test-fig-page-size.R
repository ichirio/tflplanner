# A figure's size on its page (step 3): the report sheet's
# figure_width_in / figure_height_in, blank the page's body; the program
# draws it so, step 3's form sets it, its first page shows it

test_that("a figure's program draws it at its size on the page", {
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- set_report_datasets(p, "F-1", "ADSL")
  p <- set_step_code(p, "F-1", "content", "content <- ggplot2::ggplot(adsl)")
  code <- program_code(p, "F-1")
  # the page's body (letter landscape, margins 0.75): 9.5 wide
  sized <- code[startsWith(code, "content <- fig_sized(content, ")]
  expect_length(sized, 1L)
  expect_true(grepl("width_in = 9.5,", sized, fixed = TRUE))
  # its own width: that
  q <- set_page_cell(p, "report", "figure_width_in", "F-1", "6")
  expect_identical(.fig_size_of(q, "F-1")$width_in, 6)
  expect_true(any(grepl("content <- fig_sized(content, width_in = 6,", program_code(q, "F-1"), fixed = TRUE)))
  expect_true(any(grepl("rtf_figures(content, width_twips = 8640L", program_code(q, "F-1"), fixed = TRUE)))
  expect_silent(parse(text = program_code(q, "F-1")))
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
    expect_match(h, "(fit: 9.5)", fixed = TRUE)
    expect_match(output$page_sample$html, "Run the code", fixed = TRUE)
    session$setInputs(pg_edit = list(kind = "cell", sheet = "report", col = "figure_width_in",
                                     value = "6", target = "F-1"))
    expect_identical(page_value_from(rv$p, "report", "figure_width_in", "F-1")$value, "6")
    # a table: no figure size
    session$setInputs(target = "T-1")
    expect_no_match(output$page_form$html, "figure_width_in", fixed = TRUE)
  })
})
