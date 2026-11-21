# study.yml's keys the view does not write (series_settings: from the
# series card, ai: review / run from the AI screens): kept when an edit of
# the study's own fields writes study.yml again

skip_on_cran()

keys_study <- function() {
  local_home(parent.frame())
  s <- suppressMessages(create_study("KS-1", planner = add_output(new_planner(), "A", process_code = "a <- 1")))
  f <- file.path(s$path, "study.yml")
  # (a new study's study.yml has its ai: review block already)
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  if (!any(lines == "ai:")) lines <- c(lines, "ai:", "  review:", "    table: 2")
  writeLines(c(lines, "series_settings:", "  tflspec_v1:", "    total: true",
               "    style: !series style"), f, useBytes = TRUE)
  f
}
# the two blocks as written, and the title
block_of <- function(f) {
  b <- .yaml_top_blocks(readLines(f, encoding = "UTF-8", warn = FALSE))
  list(title = sub("^title: ", "", b$title[1L]), series_settings = b$series_settings, ai = b$ai)
}

test_that("the YAML's top-level blocks, as written", {
  b <- .yaml_top_blocks(c("study_id: X", "series_settings:", "  s:", "    a: !series a", "# c",
                          "'ai':", "  language: ja"))
  expect_identical(names(b), c("study_id", "series_settings", "ai"))
  expect_identical(b$series_settings, c("series_settings:", "  s:", "    a: !series a", "# c"))
  expect_identical(.yaml_top_blocks(character()), list())
  expect_identical(.yaml_top_blocks(NULL), list())
})

test_that("a title changed and saved keeps study.yml's series_settings and ai blocks", {
  f <- keys_study()
  before <- block_of(f)
  s <- suppressMessages(open_study("KS-1"))
  s$meta$title <- "A new title"
  s <- suppressMessages(save_study(s))
  after <- block_of(f)
  expect_identical(after$title, "A new title")
  expect_identical(after$series_settings, before$series_settings)
  expect_identical(after$ai, before$ai)
  expect_true(any(grepl("style: !series style", readLines(f), fixed = TRUE)))
  # and again: the next save keeps them too
  s$meta$title <- "Another title"
  s <- suppressMessages(save_study(s))
  after <- block_of(f)
  expect_identical(after$title, "Another title")
  expect_identical(after$series_settings, before$series_settings)
  expect_true(all(spec_status("KS-1")$status == "same"))
})

test_that("a title edit not saved yet: the spec's study.yml keeps the blocks (the view's sync)", {
  f <- keys_study()
  s <- suppressMessages(open_study("KS-1"))
  meta <- s$meta
  meta$title <- "Unsaved title"
  p <- .spec_sync(s$planner, meta)
  txt <- p$spec$text[["study.yml"]]
  expect_true(any(txt == "title: Unsaved title"))
  expect_identical(sum(txt == "series_settings:"), 1L)
  expect_true(any(grepl("style: !series style", txt, fixed = TRUE)))
  expect_false(is.null(p$spec$study$study$ai$review))
  expect_true(p$spec$study$study$series_settings$tflspec_v1$total)
  # a second edit on top of the first: kept once, not twice
  meta$title <- "Unsaved again"
  p <- .spec_sync(p, meta)
  txt <- p$spec$text[["study.yml"]]
  expect_true(any(txt == "title: Unsaved again"))
  expect_identical(sum(txt == "series_settings:"), 1L)
  expect_identical(sum(txt == "ai:"), 1L)
})

test_that("a planner with no spec, written whole, keeps the blocks too", {
  f <- keys_study()
  before <- block_of(f)
  s <- suppressMessages(open_study("KS-1"))
  s$planner <- .planner_body(s$planner)
  s$meta$title <- "Written whole"
  s <- suppressMessages(save_study(s))
  after <- block_of(f)
  expect_identical(after$title, "Written whole")
  expect_identical(after$series_settings, before$series_settings)
  expect_identical(after$ai, before$ai)
  expect_true(s$planner$spec$study$study$series_settings$tflspec_v1$total)
})
