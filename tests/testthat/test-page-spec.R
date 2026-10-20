# Step 3 on the YAML spec (#438, B4): a page's values read layer by layer
# with where they come from, written into the one layer's file, the key
# changed only

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

test_that("a value comes from the report, its pattern or the defaults", {
  skip_on_cran()
  p <- page_sample()
  sp <- .pgs_spec(p)
  a <- .pgs_arg(sp, "T-14-3-1", "rtf_page", "margin_top_in")
  expect_identical(a$from, "pattern:Compact")
  expect_identical(a$value, 0.5)
  expect_identical(.pgs_arg(sp, "L-16-2-7", "rtf_page", "orientation")$from, "own")
  d <- .pgs_lines(sp, "T-14-3-1", "footer")
  expect_identical(d$line, c("1", "98", "99"))
  expect_identical(d$from, c("own", "defaults", "defaults"))
  expect_identical(.pgs_patterns(sp), "Compact")
  expect_identical(.pgs_pattern_of(sp, "T-14-3-1"), "Compact")
  expect_true(is.na(.pgs_pattern_of(sp, "T-14-1-1")))
  # a pattern's form: its own over Standard (the defaults)
  expect_identical(.pgs_arg(sp, "@Compact", "rtf_page", "margin_top_in")$from, "own")
  expect_identical(.pgs_lines(sp, "@Compact", "footer")$from, c("defaults", "defaults"))
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
  q <- .pgs_set_arg(p, "T-14-1-1", "rtf_default_format", "row_height_twips", .pgs_value("300", "number"))
  expect_identical(lines_new(p, q, "spec/outputs/T-14-1-1.yml"),
                   c("  rtf_default_format:", "    row_height_twips: 300"))
  expect_error(.pgs_value("big", "pt"), "number of points")
  expect_identical(.pgs_value("8", "pt"), 16L)
  expect_identical(.pgs_value("fit", "inches"), "fit")
  expect_identical(.pgs_value("6", "inches"), 8640L)
  expect_s3_class(.pgs_value("(none)", "text"), "tfl_unset")
})

test_that("!unset: a line, a token, a value left out; Revert takes it back", {
  skip_on_cran()
  p <- page_sample()
  q <- .pgs_set_line(p, "T-14-1-1", "footer", "99", omit = TRUE)
  expect_identical(lines_new(p, q, "spec/outputs/T-14-1-1.yml"), "    '99': !unset ''")
  d <- .pgs_lines(.pgs_spec(q), "T-14-1-1", "footer")
  expect_true(d$omitted[d$line == "99"])
  expect_false(any(grepl("Generated on", program_code(q, "T-14-1-1"), fixed = TRUE)))
  r <- .pgs_set_line(q, "T-14-1-1", "footer", "99", drop = TRUE)
  expect_identical(texts_changed(p, r), character())
  # a value of the defaults' taken away in one report
  q <- .pgs_set_arg(p, NA, "rtf_document", "watermark", "DRAFT")
  expect_identical(texts_changed(p, q), "spec/defaults.yml")
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
  # a line of the defaults' changed here: its own; a cell left blank over one below: !unset
  q <- .pgs_set_line(p, "T-14-1-1", "header", "2", left = "Protocol X")
  expect_identical(lines_new(p, q, "spec/outputs/T-14-1-1.yml"),
                   c("  header:", "    '2':", "      left: Protocol X", "      right: !unset ''"))
})

test_that("extends: the pattern chosen; a pattern made, renamed, removed", {
  skip_on_cran()
  p <- page_sample()
  q <- .pgs_set_pattern(p, "T-14-1-1", "Compact")
  expect_identical(texts_changed(p, q), "spec/outputs/T-14-1-1.yml")
  expect_identical(q$spec$outputs[["T-14-1-1"]]$extends, c("patterns/Compact", "defaults"))
  expect_identical(report_pattern(q, "T-14-1-1"), "Compact")
  r <- .pgs_set_pattern(q, "T-14-1-1", "Standard")
  expect_identical(texts_changed(p, r), character())
  expect_identical(.planner_body(r), .planner_body(p))
  expect_error(.pgs_set_pattern(p, "T-14-1-1", "Wide"), "No page pattern")
  q <- .pgs_add_pattern(p, "Wide")
  expect_identical(texts_changed(p, q), "spec/patterns/Wide.yml")
  expect_true("Wide" %in% page_patterns(q))
  expect_error(.pgs_add_pattern(q, "Wide"), "already")
  expect_error(.pgs_add_pattern(q, "standard"), "Standard is")
  q <- .pgs_set_arg(q, "@Wide", "rtf_page", "orientation", "portrait")
  expect_identical(sheet_rows(q, "page", "@Wide")$orientation, "portrait")
  r <- .pgs_rename_pattern(p, "Compact", "Small")
  expect_setequal(texts_changed(p, r), c("spec/patterns/Compact.yml", "spec/patterns/Small.yml",
                                         "spec/outputs/T-14-3-1.yml"))
  expect_identical(report_pattern(r, "T-14-3-1"), "Small")
  expect_identical(.pgs_arg(.pgs_spec(r), "T-14-3-1", "rtf_page", "margin_top_in")$from, "pattern:Small")
  expect_identical(program_code(r, "T-14-3-1"), program_code(p, "T-14-3-1"))
  expect_error(.pgs_remove_pattern(p, "Compact"), "used by T-14-3-1")
  r <- .pgs_remove_pattern(q, "Wide")
  expect_identical(texts_changed(p, r), character())
  expect_false("Wide" %in% page_patterns(r))
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
  expect_match(h, "pattern Compact", fixed = TRUE)
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
    session$setInputs(pg_act = list(act = "omit", band = "footer", line = "98", n = 2))
    expect_s3_class(rv$p$spec$outputs[["T-14-1-1"]]$page$footer[["98"]], "tfl_unset")
    session$setInputs(pg_act = list(act = "drop", band = "footer", line = "98", n = 3))
    # the first page's sample follows an edit
    expect_false(grepl("Protocol X", output$page_sample$html, fixed = TRUE))
    session$setInputs(pg_act = list(act = "own", band = "header", line = "2", n = 4))
    session$setInputs(pg_edit = list(kind = "line", band = "header", line = "2", part = "left",
                                     value = "Protocol X", n = 5))
    session$elapse(3000)
    expect_match(output$page_sample$html, "Protocol X", fixed = TRUE)
    session$setInputs(pg_act = list(act = "drop", band = "header", line = "2", n = 6))
    session$setInputs(page_pattern_by = list(report = "T-14-1-1", value = "Compact"))
    expect_identical(rv$p$spec$outputs[["T-14-1-1"]]$extends, c("patterns/Compact", "defaults"))
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
