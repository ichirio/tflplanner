# The AI drafts tab's card for the look of past reports (R/ai_style_ui.R)

test_that("the survey's files are the home's, not a study's", {
  local_home()
  expect_identical(.ai_home(), file.path(tflplanner_home(), "ai"))
  expect_identical(.ai_reference_home(), file.path(tflplanner_home(), "ai", "reference"))
  expect_identical(.ai_past_dir(), file.path(tflplanner_home(), "ai", "past_tfl"))
})

test_that("a cell's template reads in the agent's words", {
  expect_identical(.std_placeholders("{n:.0f} ({p:.1f%})"), "xx (xx.x%)")
  expect_identical(.std_placeholders("(N={n})"), "(N=xx)")
  expect_identical(.std_placeholders("{n:.0f}/{N:.0f} ({p:.2f%})"), "xx/xx (xx.xx%)")
  expect_identical(.std_placeholders("{OUTPUT_LABEL}"), "{OUTPUT_LABEL}")
})

test_that("the company standard maps onto the conventions tflspec sets it against", {
  s <- .builtin_standards()
  std <- .std_conventions(s)
  expect_named(std, c("kind", "key", "value"))
  expect_true(all(std$kind == "all"))
  v <- stats::setNames(std$value, std$key)
  expect_setequal(names(v), c("stub_ratio", "titles", "footnotes", "n_form", "stub_heading",
                              "categorical", "continuous", "digits"))
  expect_identical(v[["n_form"]], "(N=xx)")
  expect_identical(v[["categorical"]], "xx (xx.x%)")
  expect_identical(v[["stub_heading"]], "Characteristic")
  expect_identical(v[["stub_ratio"]], "2.2")
  expect_identical(v[["continuous"]], "n | Mean (SD) | Median | Min, Max")
  expect_match(v[["digits"]], "mean 1, sd 2", fixed = TRUE)
  expect_identical(v[["titles"]],
                   "page header, centred: {OUTPUT_LABEL} / {OUTPUT_TITLE} / <{OUTPUT_POPULATION}>")
  expect_match(v[["footnotes"]], "^page footer, left: \\{PROGRAM\\}")
  # what the standards do not hold is not there (shown as new)
  expect_false(any(c("rules", "spanner_rule", "zero", "first_row", "sort", "font", "orientation") %in%
                     names(v)))
  # a font and a page the company sets
  s$settings$value[s$settings$key == "font"] <- "Courier New"
  s$settings$value[s$settings$key == "font_size"] <- "9"
  v <- stats::setNames(.std_conventions(s)$value, .std_conventions(s)$key)
  expect_identical(v[["font"]], "Courier New 9 pt")
  s$default_page[1L, ] <- NA
  s$default_page$orientation <- "landscape"
  s$default_page$font <- "Arial"
  s$default_page$font_size_half_points <- "20"
  v <- stats::setNames(.std_conventions(s)$value, .std_conventions(s)$key)
  expect_identical(v[["orientation"]], "landscape")
  expect_identical(v[["font"]], "Arial 10 pt")
  # none at all: an empty frame
  expect_identical(nrow(.std_conventions(list())), 0L)
})

# a fictional agent's result (no company's file)
write_learned <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, "company_hints.yml")
  writeLines(c(
    "tfl_company_hints: {version: 1, files: 4, tables: 12}",
    "kinds:",
    "  all:",
    "    font: {value: \"Courier New 8 pt\", seen: 4, of: 4}",
    "    stub_heading: {value: \"Characteristic\", seen: 4, of: 4}",
    "  demog:",
    "    n_form: {value: \"N = xx\", seen: 3, of: 4}",
    "    zero: {value: \"-\", seen: 1, of: 4}",
    "  ae_soc_pt:",
    "    first_row: {value: \"Any TEAE\", seen: 2, of: 2}"), f)
  f
}

style_server <- function(input, output, session) {
  ai_style_server(input, output, session, notify = function(...) NULL,
                  guarded = function(expr) expr, t = identity)
}

test_that("the result is set against the standard, ticked lines adopted into the pack", {
  local_home()
  f <- write_learned(.ai_home())
  std <- .std_conventions()
  d <- tflspec::tfl_ai_style_diff(f, std)
  at <- function(kind, key) which(d$kind == kind & d$key == key)
  # the first ticks: proposed lines that differ or are new
  ticks <- .style_default_ticks(d)
  expect_true(ticks[at("all", "font")])            # new
  expect_true(ticks[at("demog", "n_form")])        # differs
  expect_true(ticks[at("ae_soc_pt", "first_row")]) # new
  expect_false(ticks[at("all", "stub_heading")])   # same
  expect_false(ticks[at("demog", "zero")])         # by hand
  expect_false(any(ticks[d$status == "not learned"]))
  shiny::testServer(style_server, {
    session$setInputs(style_read = 1)
    h <- output$style_diff$html
    expect_match(h, "Set against the company standard", fixed = TRUE)
    expect_match(h, "Differs from the standard", fixed = TRUE)
    expect_match(h, "Not in the standard", fixed = TRUE)
    expect_match(h, "Same as the standard", fixed = TRUE)
    expect_match(h, "Not learned", fixed = TRUE)
    expect_match(h, "by hand", fixed = TRUE)
    expect_match(h, "3 / 4", fixed = TRUE)
    expect_match(h, "(N=xx)", fixed = TRUE)
    expect_match(h, "Adopt the ticked lines and build the pack", fixed = TRUE)
    # a box a learned line, ticked as proposed
    boxes <- regmatches(h, gregexpr("<input id=\"[a-z-]*style_adopt_1_[0-9]+\"[^>]*>", h))[[1L]]
    expect_length(boxes, sum(!is.na(d$learned)))
    expect_identical(sum(grepl("checked", boxes, fixed = TRUE)), sum(ticks))
    # n_form unticked, zero (by hand) ticked: adopted as ticked
    ids <- sprintf("style_adopt_1_%d", c(at("demog", "n_form"), at("demog", "zero")))
    do.call(session$setInputs, stats::setNames(list(FALSE, TRUE), ids))
    session$setInputs(style_build = 1)
    pack <- file.path(.ai_reference_home(), "conventions.md")
    expect_true(file.exists(pack))
    expect_true(file.exists(file.path(.ai_reference_home(), "README.md")))
    md <- paste(readLines(pack, encoding = "UTF-8"), collapse = "\n")
    expect_match(md, "Courier New 8 pt", fixed = TRUE)
    expect_match(md, "Any TEAE", fixed = TRUE)
    expect_match(md, "`zero`: -", fixed = TRUE)
    expect_no_match(md, "`n_form`: N = xx", fixed = TRUE)
    expect_match(md, "`n_form`: (N=xx)", fixed = TRUE)   # the standard's, kept
    expect_match(md, "mean 1, sd 2", fixed = TRUE)
    built <- output$style_diff$html
    expect_match(built, "3 line(s) adopted.", fixed = TRUE)
    expect_match(built, "conventions.md", fixed = TRUE)
    expect_match(built, "style_pack_open", fixed = TRUE)
    # its folder opened
    opened <- NULL
    withr::local_options(tflplanner.open_folder = function(p) opened <<- p)
    session$setInputs(style_pack_open = 1)
    expect_identical(normalizePath(opened, "/"), normalizePath(.ai_reference_home(), "/"))
  })
})

test_that("a new reading starts its boxes afresh", {
  local_home()
  write_learned(.ai_home())
  shiny::testServer(style_server, {
    session$setInputs(style_read = 1)
    session$setInputs(style_adopt_1_1 = FALSE)
    session$setInputs(style_read = 2)
    expect_match(output$style_diff$html, "style_adopt_2_1", fixed = TRUE)
    session$setInputs(style_build = 1)
    md <- paste(readLines(file.path(.ai_reference_home(), "conventions.md")), collapse = "\n")
    expect_match(md, "Courier New 8 pt", fixed = TRUE)
  })
})

test_that("the app points tflspec at the home's AI folder while it runs", {
  local_home()
  withr::local_options(tflspec.ai_home = "before home")
  stop_fn <- NULL
  .app_options(on_stop = function(f) stop_fn <<- f)
  expect_identical(getOption("tflspec.ai_home"), file.path(tflplanner_home(), "ai"))
  stop_fn()
  expect_identical(getOption("tflspec.ai_home"), "before home")
})

# ---- tflspec's functions mocked --------------------

# a diff of one learned line (the shape tfl_ai_style_diff() gives)
fake_diff <- function(...) {
  data.frame(kind = "all", key = "font", learned = "Courier New 8 pt", seen = 4L, of = 4L,
             standard = NA_character_, status = "new", adopt = "proposed",
             stringsAsFactors = FALSE)
}

# the agent's result as tfl_ai_style_read() gives it, with notes and conflicts
fake_read <- function(file, notes = TRUE) {
  s <- data.frame(kind = "all", key = "font", value = "Courier New 8 pt", seen = 4L, of = 4L,
                  evidence = NA_character_, adopt = "proposed", stringsAsFactors = FALSE)
  if (notes) {
    attr(s, "notes") <- stats::setNames(c("", "a blank row before each SOC"), c("", "demog"))
    attr(s, "conflicts") <- data.frame(kind = "demog", key = "n_form", standard = "(N=xx)",
                                       files = "N = xx", seen = 3L, of = 4L,
                                       stringsAsFactors = FALSE)
  }
  s
}

test_that("the prompt goes to the AI folder with the standard, the hints file is read", {
  local_home()
  got <- list()
  local_mocked_bindings(
    tfl_ai_style_prompt = function(rtf_dir = NULL, dir = NULL, standard = NULL) {
      got$prompt <<- list(rtf_dir = rtf_dir, dir = dir, standard = standard)
      structure("PROMPT", files = list(result = file.path(dir, "company_hints.yml")))
    },
    tfl_ai_style_read = function(file = NULL) {
      got$read <<- file
      fake_read(file)
    },
    tfl_ai_style_diff = fake_diff,
    .package = "tflspec")
  # the field's placeholder is the AI folder's past_tfl
  expect_match(as.character(ai_style_ui(identity, identity)), .ai_past_dir(), fixed = TRUE)
  shiny::testServer(style_server, {
    # a blank field is past_tfl: not there yet
    session$setInputs(style_rtf = "", style_prompt = 1)
    expect_null(got$prompt)
    dir.create(.ai_past_dir(), recursive = TRUE)
    session$setInputs(style_rtf = "", style_prompt = 2)
    expect_identical(got$prompt$rtf_dir, .ai_past_dir())
    expect_identical(got$prompt$dir, .ai_home())
    expect_identical(got$prompt$standard, .std_conventions())
    # a folder of the person's own
    own <- withr_tempdir()
    session$setInputs(style_rtf = own, style_prompt = 3)
    expect_identical(got$prompt$rtf_dir, own)
    # no hints file yet: nothing is read
    session$setInputs(style_read = 1)
    expect_null(got$read)
    writeLines("x", .ai_hints_file())
    session$setInputs(style_read = 2)
    expect_identical(got$read, file.path(tflplanner_home(), "ai", "company_hints.yml"))
    h <- output$style_diff$html
    expect_match(h, "Notes from the agent", fixed = TRUE)
    expect_match(h, "demog: a blank row before each SOC", fixed = TRUE)
    expect_match(h, "the standard says (N=xx), the files show N = xx (3 of 4)", fixed = TRUE)
    expect_match(h, "demog / n_form", fixed = TRUE)
  })
})
