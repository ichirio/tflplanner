# Step 3 on the YAML spec (#438, B4): a page's values read layer by layer
# (the report's own over its page configuration, a style set of
# defaults.yml; the study's tokens) with where they come from, written into
# the one layer's file, the key changed only

page_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE, home = home))$planner
}

texts_changed <- function(a, b) {
  k <- union(names(a$spec$text), names(b$spec$text))
  k[!vapply(k, function(r) identical(a$spec$text[[r]], b$spec$text[[r]]), NA)]
}
lines_new <- function(a, b, rel) setdiff(b$spec$text[[rel]], a$spec$text[[rel]])

test_that("a value comes from the report or its page configuration; a line by its block", {
  skip_on_cran()
  p <- page_sample()
  sp <- .pgs_spec(p)
  a <- .pgs_arg(sp, "T-14-3-1", "rtf_page", "margin_top_in")
  expect_identical(a$from, "style:compact")
  expect_identical(a$value, 0.5)
  expect_identical(.pgs_arg(sp, "L-16-2-7", "rtf_page", "orientation")$from, "own")
  # (the report's body line, then the configuration's last lines)
  d <- .pgs_lines(sp, "T-14-3-1", "footer")
  expect_identical(d$line, c("1", "last:98", "last:99"))
  expect_identical(d$from, c("own", "style:compact", "style:compact"))
  # (a report with no style: of its own: the study's default configuration)
  expect_identical(.pgs_lines(sp, "T-14-1-1", "footer")$from[2:3], rep("style:standard", 2))
  expect_identical(.pgs_default_style(sp$study), "standard")
  expect_identical(.pgs_patterns(sp), "compact")
  expect_identical(.pgs_pattern_of(sp, "T-14-3-1"), "compact")
  expect_true(is.na(.pgs_pattern_of(sp, "T-14-1-1")))
  # a configuration's form: complete, its own values and lines in their blocks
  expect_identical(.pgs_arg(sp, "@compact", "rtf_page", "margin_top_in")$from, "own")
  expect_identical(.pgs_lines(sp, "@compact", "footer")$line, c("last:98", "last:99"))
  expect_identical(.pgs_lines(sp, "@compact", "footer")$from, c("own", "own"))
  expect_identical(.pgs_lines(sp, NA, "header")$line, paste0("first:", 1:6))
  # a configuration's own: no layer lies under it
  expect_identical(names(.pgs_layers(sp, "@compact")), "own")
  expect_identical(names(.pgs_layers(sp, "T-14-3-1")), c("own", "style:compact"))
})

test_that("an argument written into the report's file only, and back; the same as below is nothing", {
  skip_on_cran()
  p <- page_sample()
  q <- .pgs_set_arg(p, "T-14-1-1", "rtf_page", "orientation", .pgs_value("portrait", "choice"))
  expect_identical(texts_changed(p, q), "spec/outputs/T-14-1-1.yml")
  expect_identical(lines_new(p, q, "spec/outputs/T-14-1-1.yml"), c("  rtf_page:", "    orientation: portrait"))
  # the view: the page sheet's row of the report
  expect_identical(sheet_rows(q, "page", "T-14-1-1")$orientation, "portrait")
  expect_true(any(grepl("orientation = \"portrait\"", program_code(q, "T-14-1-1"), fixed = TRUE)))
  r <- .pgs_set_arg(q, "T-14-1-1", "rtf_page", "orientation", NULL)
  expect_identical(texts_changed(p, r), character())
  expect_identical(.planner_body(r), .planner_body(p))
  expect_identical(program_code(r, "T-14-1-1"), program_code(p, "T-14-1-1"))
  # L-16-2-7's own landscape over nothing below: kept; the pattern's 0.5 typed: nothing
  expect_identical(texts_changed(p, .pgs_set_arg(p, "T-14-3-1", "rtf_page", "margin_top_in",
                                                 .pgs_value("0.5", "number"))), character())
  # an argument no sheet has a column for
  q <- .pgs_set_arg(p, "T-14-1-1", "rtf_style", "row_height_twips", .pgs_value("300", "number"))
  expect_identical(lines_new(p, q, "spec/outputs/T-14-1-1.yml"),
                   c("  rtf_style:", "    row_height_twips: 300"))
  expect_error(.pgs_value("big", "pt"), "number of points")
  expect_identical(.pgs_value("8", "pt"), 16L)
  expect_identical(.pgs_value("fit", "inches"), "fit")
  expect_identical(.pgs_value("6", "inches"), 8640L)
  expect_s3_class(.pgs_value("(none)", "text"), "tfl_unset")
})

test_that("an old rtf_default_format reads as rtf_style's shared level, and is written as rtf_style (#537)", {
  skip_on_cran()
  # rtf_style's own keys win; its sections stay as they are
  pg <- list(rtf_default_format = list(font_size_half_points = 16L, row_height_twips = 300L),
             rtf_style = list(row_height_twips = 260L, titles = list(align = "left")))
  expect_identical(.pgs_style_norm(pg),
                   list(rtf_style = list(row_height_twips = 260L, titles = list(align = "left"),
                                         font_size_half_points = 16L)))
  expect_identical(.pgs_style_norm(list(rtf_default_format = list(markup = "all"))),
                   list(rtf_style = list(markup = "all")))
  expect_identical(.pgs_style_norm(list(rtf_page = list(paper_size = "A4"))),
                   list(rtf_page = list(paper_size = "A4")))
  # a study saved with the old key: the form shows its values, a change writes rtf_style
  p <- page_sample()
  p <- .spec_ensure(p)
  p$spec$study$styles$compact$page$rtf_style <- NULL
  p$spec$study$styles$compact$page$rtf_default_format <-
    list(font_size_half_points = 16L, markup = "all")
  p$spec$study$styles$compact$page$rtf_style <- list(table = list(cell_valign = "center"))
  sp <- .pgs_spec(p)
  expect_identical(.pgs_arg(sp, "T-14-3-1", "rtf_style", "font_size_half_points")$from, "style:compact")
  expect_identical(.pgs_arg(sp, "@compact", "rtf_style", "markup")$value, "all")
  q <- .pgs_set_arg(p, "@compact", "rtf_style", "row_height_twips", 300L)
  pg <- q$spec$study$styles$compact$page
  expect_null(pg$rtf_default_format)
  expect_identical(pg$rtf_style[c("table", "font_size_half_points", "markup", "row_height_twips")],
                   list(table = list(cell_valign = "center"), font_size_half_points = 16L,
                        markup = "all", row_height_twips = 300L))
  y <- q$spec$text[["spec/defaults.yml"]]
  expect_false(any(grepl("rtf_default_format", y, fixed = TRUE)))
  expect_true(any(grepl("rtf_style:", y, fixed = TRUE)))
})

test_that("!unset: a line, a token, a value left out; Revert takes it back", {
  skip_on_cran()
  p <- page_sample()
  # (a configuration's line is left out by its block and number)
  q <- .pgs_set_line(p, "T-14-1-1", "footer", "last:99", omit = TRUE)
  expect_identical(lines_new(p, q, "spec/outputs/T-14-1-1.yml"),
                   c("    last:", "      '99': !unset ''"))
  d <- .pgs_lines(.pgs_spec(q), "T-14-1-1", "footer")
  expect_true(d$omitted[d$line == "last:99"])
  expect_false(any(grepl("Generated on", program_code(q, "T-14-1-1"), fixed = TRUE)))
  r <- .pgs_set_line(q, "T-14-1-1", "footer", "last:99", drop = TRUE)
  expect_identical(texts_changed(p, r), character())
  # a value of the defaults' taken away in one report
  q <- .pgs_set_arg(p, NA, "rtf_document", "watermark", "DRAFT")
  expect_identical(texts_changed(p, q), "spec/defaults.yml")
  expect_identical(q$spec$study$styles$standard$page$rtf_document$watermark, "DRAFT")
  expect_identical(sheet_rows(q, "report", NA)$watermark, "DRAFT")
  r <- .pgs_set_arg(q, "T-14-1-1", "rtf_document", "watermark", tflspec::tfl_unset())
  expect_identical(lines_new(q, r, "spec/outputs/T-14-1-1.yml"),
                   c("  rtf_document:", "    watermark: !unset ''"))
  expect_identical(.pgs_arg(.pgs_spec(r), "T-14-1-1", "rtf_document", "watermark")$unset, TRUE)
  # (none) where nothing below says it: nothing
  expect_identical(texts_changed(p, .pgs_set_arg(p, "T-14-1-1", "rtf_document", "watermark",
                                                 tflspec::tfl_unset())), character())
  # a token
  q <- .pgs_set_token(p, "T-14-1-1", "OUTPUT_TITLE", tflspec::tfl_unset())
  expect_true(.pgs_tokens(.pgs_spec(q), "T-14-1-1")$omitted[1L])
  # a line of the configuration's changed here: its own, in its block; a cell
  # left blank over one below: !unset
  q <- .pgs_set_line(p, "T-14-1-1", "header", "first:2", left = "Protocol X")
  expect_identical(lines_new(p, q, "spec/outputs/T-14-1-1.yml"),
                   c("  header:", "    first:", "      '2':", "        left: Protocol X", "        right: !unset ''"))
})

test_that("style: the page configuration chosen; one made, renamed, removed", {
  skip_on_cran()
  p <- page_sample()
  q <- .pgs_set_pattern(p, "T-14-1-1", "compact")
  expect_identical(texts_changed(p, q), "spec/outputs/T-14-1-1.yml")
  expect_identical(q$spec$outputs[["T-14-1-1"]]$style, "compact")
  # (the page step has no files: extends is as it was)
  expect_identical(q$spec$outputs[["T-14-1-1"]]$extends, p$spec$outputs[["T-14-1-1"]]$extends)
  expect_identical(.pgs_pattern_of(.pgs_spec(q), "T-14-1-1"), "compact")
  expect_identical(.pgs_arg(.pgs_spec(q), "T-14-1-1", "rtf_page", "margin_top_in")$from, "style:compact")
  r <- .pgs_set_pattern(q, "T-14-1-1", "Standard")
  expect_identical(texts_changed(p, r), character())
  expect_identical(.planner_body(r), .planner_body(p))
  # the study's default one is no choice: `style:` goes
  expect_identical(texts_changed(p, .pgs_set_pattern(q, "T-14-1-1", "standard")), character())
  # a built-in configuration is a choice
  s <- .pgs_set_pattern(p, "T-14-1-1", "tfl_small")
  expect_identical(s$spec$outputs[["T-14-1-1"]]$style, "tfl_small")
  expect_identical(.pgs_pattern_of(.pgs_spec(s), "T-14-1-1"), "tfl_small")
  expect_error(.pgs_set_pattern(p, "T-14-1-1", "Wide"), "No page configuration")
  q <- .pgs_add_pattern(p, "Wide")
  expect_identical(texts_changed(p, q), "spec/defaults.yml")
  expect_true("Wide" %in% .pgs_patterns(.pgs_spec(q)))
  # (a new configuration is a copy of the study's default one)
  expect_identical(q$spec$study$styles$Wide$page, q$spec$study$styles$standard$page)
  expect_error(.pgs_add_pattern(q, "Wide"), "already")
  expect_error(.pgs_add_pattern(q, "Standard"), "Standard is")
  expect_error(.pgs_add_pattern(q, "tfl_small"), "built-in")
  expect_error(.pgs_add_pattern(q, "tfl_standard"), "built-in")
  q <- .pgs_set_arg(q, "@Wide", "rtf_page", "orientation", "portrait")
  expect_identical(.pgs_arg(.pgs_spec(q), "@Wide", "rtf_page", "orientation")$value, "portrait")
  expect_identical(q$spec$study$styles$Wide$page$rtf_page$orientation, "portrait")
  # (and the default one: not changed)
  expect_null(q$spec$study$styles$standard$page$rtf_page$orientation)
  r <- .pgs_rename_pattern(p, "compact", "Small")
  expect_setequal(texts_changed(p, r), c("spec/defaults.yml", "spec/outputs/T-14-3-1.yml"))
  expect_identical(r$spec$outputs[["T-14-3-1"]]$style, "Small")
  expect_identical(.pgs_arg(.pgs_spec(r), "T-14-3-1", "rtf_page", "margin_top_in")$from, "style:Small")
  # (the setup's objects are named by the configuration: header_Small)
  expect_identical(gsub("_Small", "_compact", program_code(r, "T-14-3-1"), fixed = TRUE),
                   program_code(p, "T-14-3-1"))
  expect_error(.pgs_remove_pattern(p, "compact"), "used by T-14-3-1")
  expect_error(.pgs_remove_pattern(p, "standard"), "No page configuration")
  r <- .pgs_remove_pattern(q, "Wide")
  expect_identical(r$spec$study$styles, p$spec$study$styles)
  expect_false("Wide" %in% .pgs_patterns(.pgs_spec(r)))
})

test_that("the names: standard is free, tfl_standard and tfl_small are tflspec's", {
  none <- list(styles = list())
  expect_identical(.pgs_check_name(none, "standard"), "standard")
  expect_identical(.pgs_check_name(none, "@Compact"), "Compact")
  expect_error(.pgs_check_name(none, "tfl_standard"), "built-in")
  expect_error(.pgs_check_name(none, "tfl_small"), "built-in")
  expect_error(.pgs_check_name(none, "Standard"), "Standard is")
  expect_error(.pgs_check_name(none, "1x"), "letter, then letters")
  # (the sheets' own check of a pattern's name)
  x <- new_planner()
  expect_identical(.check_pattern_name(x, "standard"), "standard")
  expect_error(.check_pattern_name(x, "tfl_small"), "built-in")
})

test_that("the study's token goes to study.yml tokens:, edited on the default configuration's form", {
  skip_on_cran()
  p <- page_sample()
  q <- .pgs_set_token(p, NA, "COMPANY", "Sample Pharma")
  expect_identical(texts_changed(p, q), "study.yml")
  expect_identical(q$spec$study$study$tokens, list(COMPANY = "Sample Pharma"))
  expect_true(any(grepl("^tokens:", q$spec$text[["study.yml"]])))
  d <- .pgs_tokens(.pgs_spec(q), "T-14-1-1")
  expect_identical(d$from[d$name == "COMPANY"], "defaults")
  # (a report's own value goes over it, in its file)
  r <- .pgs_set_token(q, "T-14-1-1", "COMPANY", "Other")
  expect_identical(texts_changed(q, r), "spec/outputs/T-14-1-1.yml")
  # (back: no block)
  s <- .pgs_set_token(q, NA, "COMPANY", NULL)
  expect_false(any(grepl("^tokens:", s$spec$text[["study.yml"]])))
  expect_identical(texts_changed(p, s), character())
  # a built-in default configuration cannot be edited here
  b <- .spec_ensure(p)
  b$spec$study$study$settings$style <- "tfl_standard"
  expect_error(.pgs_write(b, NA, list(rtf_page = list(orientation = "portrait"))), "built-in")
})

test_that("the form: a card a function, the arguments with a value first, labels with the argument", {
  skip_on_cran()
  p <- page_sample()
  h <- as.character(.page_form_ui(p, "T-14-3-1", identity))
  expect_match(h, "(rtf_document(page =))", fixed = TRUE)
  expect_match(h, "(rtf_section(footer =))", fixed = TRUE)
  expect_match(h, "(font_size_half_points = pt x 2)", fixed = TRUE)
  # the arguments not in the code: folded, each with rtfreporter's default
  expect_false(grepl("pg-lower\" open", h, fixed = TRUE))
  expect_match(h, "rtfreporter's default", fixed = TRUE)
  expect_match(h, "(margin_top_in)", fixed = TRUE)
  expect_match(h, "style compact", fixed = TRUE)
  # the paper card's two rows above (paper, orientation, font, size; the
  # margins), whether written or not; the rest folded
  expect_lt(regexpr("data-arg=\"paper_size\"", h, fixed = TRUE), regexpr("margin_top_in", h, fixed = TRUE))
  expect_lt(regexpr("margin_top_in", h, fixed = TRUE), regexpr("Not in the code (", h, fixed = TRUE))
  # no figure card for a table
  expect_false(grepl("rtf_figures", h, fixed = TRUE))
  q <- .pgs_set_arg(p, "T-14-3-1", "rtf_page", "margin_top_in", 0.7)
  h <- as.character(.page_form_ui(q, "T-14-3-1", identity))
  expect_match(h, "pg-badge-own", fixed = TRUE)
  expect_match(h, "data-act=\"reset\" title=\"Back to the study default\" data-fn=\"rtf_page\" data-arg=\"margin_top_in\"",
               fixed = TRUE)
  # Standard and a pattern: both content cards
  h <- as.character(.page_form_ui(p, NA, identity))
  expect_match(h, "rtf_tables", fixed = TRUE)
  expect_match(h, "rtf_figures", fixed = TRUE)
})

test_that("step 3's form in the app: an argument, a line, the pattern, the study saved changes one file", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  root <- s$path
  md5 <- function() tools::md5sum(file.path(root, .spec_files_yaml(root)))
  before <- md5()
  shiny::testServer(server_for("SAMPLE-01"), {
    rv <- session$userData$rv
    session$setInputs(target = "T-14-1-1", nav = "make", step = "page")
    session$setInputs(pg_edit = list(kind = "arg", fn = "rtf_page", arg = "orientation",
                                     value = "portrait", n = 1))
    expect_identical(rv$p$spec$outputs[["T-14-1-1"]]$page$rtf_page$orientation, "portrait")
    expect_match(output$page_form$html, "pg-badge-own", fixed = TRUE)
    session$setInputs(pg_act = list(act = "omit", band = "footer", line = "last:98", n = 2))
    expect_s3_class(rv$p$spec$outputs[["T-14-1-1"]]$page$footer$last[["98"]], "tfl_unset")
    session$setInputs(pg_act = list(act = "drop", band = "footer", line = "last:98", n = 3))
    # the first page's sample follows an edit
    expect_false(grepl("Protocol X", output$page_sample$html, fixed = TRUE))
    session$setInputs(pg_act = list(act = "own", band = "header", line = "first:2", n = 4))
    session$setInputs(pg_edit = list(kind = "line", band = "header", line = "first:2", part = "left",
                                     value = "Protocol X", n = 5))
    session$elapse(3000)
    # (the report's YAML says it, and the form shows it.  The first page's
    # sample reads the sheets' view, where tflspec's spec-to-sheets step does
    # not yet carry a report's first / last line overrides: not asserted)
    expect_identical(rv$p$spec$outputs[["T-14-1-1"]]$page$header$first[["2"]]$left, "Protocol X")
    expect_match(output$page_form$html, 'value="Protocol X"', fixed = TRUE)
    session$setInputs(pg_act = list(act = "drop", band = "header", line = "first:2", n = 6))
    session$setInputs(page_pattern_by = list(report = "T-14-1-1", value = "compact"))
    expect_identical(rv$p$spec$outputs[["T-14-1-1"]]$style, "compact")
    session$setInputs(page_pattern_by = list(report = "T-14-1-1", value = "Standard"))
    session$setInputs(save = 1)
  })
  after <- md5()
  expect_identical(names(after)[after != before], file.path(root, "spec/outputs/T-14-1-1.yml"))
})

test_that("the paper card: paper, orientation, font, size; then the four margins (screen order)", {
  p <- .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
  sp <- .pgs_spec(p)
  h <- as.character(.page_paper_ui(sp, NA_character_, function(x) x))
  at <- function(a) regexpr(sprintf('data-arg="%s"', a), h, fixed = TRUE)
  args <- c("paper_size", "orientation", "font_table", "font_size_half_points")
  pos <- vapply(args, at, 0)
  said <- pos[pos > 0]
  expect_identical(names(said), names(sort(said)))
  # the font and the size are not in their own cards any more
  h2 <- as.character(.page_form_ui(p, NA_character_, function(x) x))
  n <- function(x) lengths(regmatches(x, gregexpr('data-arg="font_table"', x, fixed = TRUE)))
  expect_identical(n(h2), n(h))
})
