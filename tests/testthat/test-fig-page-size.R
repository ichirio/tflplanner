# A figure's size on its page: its content's tfl_image (tflspec#874), in
# the view the report sheet's figure_width_in / figure_height_in -- blank
# its own size, fit the page's body, or inches; the program draws it so,
# the content step's card sets it, its first page shows it

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

test_that("step 3 of a figure: no figure size there; its first page says where its figure comes from", {
  skip_on_cran()
  local_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- set_step_code(p, "F-1", "content", "content <- 1")
  p <- add_output(p, "T-1")
  create_study("FS", planner = p)
  shiny::testServer(server_for("FS"), {
    session$setInputs(target = "F-1", nav = "make", step = "page")
    h <- output$page_form$html
    # (a figure's own settings are its content's tfl_image, tflspec#874)
    for (a in c("data-arg=\"width_twips\"", "data-arg=\"height_twips\"", "fig_fit", "rtf_figures")) {
      expect_no_match(h, a, fixed = TRUE)
    }
    expect_match(output$page_sample$html, "Run the code", fixed = TRUE)
    # a figure of its own R code: its content has no tfl_image
    session$setInputs(step = "content")
    expect_match(output$fig_image_card$html, "drawn at its own size", fixed = TRUE)
    expect_no_match(output$fig_image_card$html, "fimg__width_in", fixed = TRUE)
  })
})

test_that("a figure's size, alignment and dpi: its content's tfl_image (tflspec#874)", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  p <- s$planner
  id <- "F-14-2-3"
  expect_identical(.fig_image_own(.spec_output(p, id)), list())
  q <- .fig_image_set(p, id, list(width_in = "fit", height_in = 5, dpi = 150L, align = "left"))
  o <- .spec_output(q, id)
  last <- o$content[[length(o$content)]]
  expect_identical(last$fn, "tfl_image")
  expect_identical(last$args[c("width_in", "height_in", "dpi", "align")],
                   list(width_in = "fit", height_in = 5, dpi = 150L, align = "left"))
  txt <- q$spec$text[[q$spec$where[[id]]]]
  expect_true(any(grepl("tfl_image", txt, fixed = TRUE)))
  expect_false(any(grepl("rtf_figures", txt, fixed = TRUE)))
  # the view: the report sheet's size cells, which the programs read
  expect_identical(page_value_from(q, "report", "figure_width_in", id)$value, "fit")
  expect_identical(page_value_from(q, "report", "figure_height_in", id)$value, "5")
  expect_identical(.fig_size_of(q, id)$mode[["width"]], "fit")
  expect_identical(.fig_dpi_of(q, id), 150)
  # the item taken away: its own size again
  r <- .fig_image_set(q, id, list())
  expect_identical(.fig_image_own(.spec_output(r, id)), list())
  expect_true(is.na(page_value_from(r, "report", "figure_width_in", id)$value))
  # the study's settings: figure: under the item's own
  r$spec$study$study$settings$figure <- list(dpi = 200, width_in = 7)
  expect_identical(.fig_image_now(r, id)[c("dpi", "width_in")], list(dpi = 200, width_in = 7))
  expect_identical(.fig_dpi_of(.fig_image_set(q, id, list(dpi = 120L)), id), 120)
  # a figure of R of its own has none
  expect_null(.fig_image_own(.spec_output(p, "F-14-2-1")))
  expect_error(.fig_image_set(p, "F-14-2-1", list(dpi = 150)), "R of its own")
})

test_that("the content step's tfl_image card: fields from tflspec's definitions, fit and its own size", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  id <- "F-14-2-3"
  shiny::testServer(server_for("SAMPLE-01"), {
    rv <- session$userData$rv
    session$setInputs(target = id, nav = "make", step = "content")
    h <- output$fig_image_card$html
    for (a in c("width_in", "height_in", "align", "dpi")) {
      expect_match(h, paste0("fimg__", a), fixed = TRUE)
    }
    expect_match(h, "fimg_fit", fixed = TRUE)
    expect_no_match(h, "fimg_native", fixed = TRUE)
    own <- function() .fig_image_own(rv$p$spec$outputs[[id]])
    session$setInputs(fimg_fit = 1)
    expect_identical(own()[c("width_in", "height_in")], list(width_in = "fit", height_in = "fit"))
    expect_identical(page_value_from(rv$p, "report", "figure_width_in", id)$value, "fit")
    expect_match(output$fig_image_card$html, "fimg_native", fixed = TRUE)
    session$setInputs(fimg_native = 1)
    expect_identical(own(), list())
    # a field typed: the item's argument
    session$setInputs(fimg__dpi = "150")
    session$elapse(500)
    expect_equal(as.numeric(own()$dpi), 150)
    # the page step: no figure size
    session$setInputs(step = "page")
    expect_no_match(output$page_form$html, "rtf_figures", fixed = TRUE)
  })
})

test_that("a number-or-word field (tfl_image width_in: inches or fit) is typed as text", {
  expect_identical(.arg_kind("number|enum[fit]"), "string")
  expect_identical(.arg_kind("number"), "number")
  expect_identical(.num_or_word_value("6.5"), 6.5)
  expect_identical(.num_or_word_value("fit"), "fit")
  h <- as.character(spec_card("img", "tfl_image", list(fn = "tfl_image", args = list(width_in = "fit"),
                                                       own_args = list(width_in = "fit"))))
  expect_match(h, "type=\"text\" id=\"img__width_in\"", fixed = TRUE)
  expect_no_match(h, "rp-badge-defaults", fixed = TRUE)
})
