# study.yml's keys the writer does not write (ai: review / run from the AI
# screens, a block by hand, a settings: key no sheet holds): kept when a
# save writes study.yml again (tflspec#910)

skip_on_cran()

keys_study <- function() {
  local_home(parent.frame())
  s <- suppressMessages(create_study("KS-1", planner = add_output(new_planner(), "A", process_code = "a <- 1")))
  f <- file.path(s$path, "study.yml")
  # (a new study's study.yml has its ai: review block already; one more key
  # of the block's own and a comment in it)
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  if (!any(lines == "ai:")) lines <- c(lines, "ai:", "  review:", "    table: 2")
  at <- which(lines == "ai:")
  lines <- append(lines, c("  # kept as written", "  language: ja"), after = at)
  writeLines(lines, f, useBytes = TRUE)
  f
}
# the ai block as written, and the title
block_of <- function(f) {
  b <- .yaml_top_blocks(readLines(f, encoding = "UTF-8", warn = FALSE))
  list(title = sub("^title: ", "", b$title[1L]), ai = b$ai)
}

test_that("the YAML's top-level blocks, as written", {
  b <- .yaml_top_blocks(c("study_id: X", "settings:", "  s:", "    a: 1", "# c",
                          "'ai':", "  language: ja"))
  expect_identical(names(b), c("study_id", "settings", "ai"))
  expect_identical(b$settings, c("settings:", "  s:", "    a: 1", "# c"))
  expect_identical(.yaml_top_blocks(character()), list())
  expect_identical(.yaml_top_blocks(NULL), list())
})

test_that("a title changed and saved keeps study.yml's ai block", {
  f <- keys_study()
  before <- block_of(f)
  s <- suppressMessages(open_study("KS-1"))
  s$meta$title <- "A new title"
  s <- suppressMessages(save_study(s))
  after <- block_of(f)
  expect_identical(after$title, "A new title")
  expect_identical(after$ai, before$ai)
  expect_true(any(grepl("# kept as written", readLines(f), fixed = TRUE)))
  # and again: the next save keeps it too
  s$meta$title <- "Another title"
  s <- suppressMessages(save_study(s))
  after <- block_of(f)
  expect_identical(after$title, "Another title")
  expect_identical(after$ai, before$ai)
  expect_true(all(spec_status("KS-1")$status == "same"))
})

test_that("a title edit not saved yet: the spec's study.yml keeps the block (the view's sync)", {
  f <- keys_study()
  s <- suppressMessages(open_study("KS-1"))
  meta <- s$meta
  meta$title <- "Unsaved title"
  p <- .spec_sync(s$planner, meta)
  txt <- p$spec$text[["study.yml"]]
  expect_true(any(txt == "title: Unsaved title"))
  expect_identical(sum(txt == "ai:"), 1L)
  expect_true(any(grepl("# kept as written", txt, fixed = TRUE)))
  expect_false(is.null(p$spec$study$study$ai$review))
  expect_identical(p$spec$study$study$ai$language, "ja")
  # a second edit on top of the first: kept once, not twice
  meta$title <- "Unsaved again"
  p <- .spec_sync(p, meta)
  txt <- p$spec$text[["study.yml"]]
  expect_true(any(txt == "title: Unsaved again"))
  expect_identical(sum(txt == "ai:"), 1L)
})

test_that("a planner with no spec, written whole, keeps the block too", {
  f <- keys_study()
  before <- block_of(f)
  s <- suppressMessages(open_study("KS-1"))
  s$planner <- .planner_body(s$planner)
  s$meta$title <- "Written whole"
  s <- suppressMessages(save_study(s))
  after <- block_of(f)
  expect_identical(after$title, "Written whole")
  expect_identical(after$ai, before$ai)
  expect_identical(s$planner$spec$study$study$ai$language, "ja")
})

test_that("saving study.yml keeps every key the writer does not write, a settings: key too (tflspec#910)", {
  old <- c("study_id: X", "title: Old", "spec_version: 1",
           "settings:", "  rounding: sas", "  style: mine", "  # my own", "  my_key:", "  - a", "  - b",
           "  output_path: out",
           "keys:", "  id: USUBJID",
           "tokens:", "  SPONSOR: Old Pharma",
           "# by hand", "formats:", "  pct: '%.1f'",
           "created: '2026-01-01'", "updated: 2026-01-02 00:00:00", "tflplanner: 0.0.1",
           "# The AI kit", "ai:", "  language: Japanese",
           "texts:", "  a: b")
  new <- c("study_id: X", "title: New", "spec_version: 2",
           "settings:", "  rounding: r",
           "created: '2026-01-01'", "updated: 2026-11-23 00:00:00", "tflplanner: 0.0.2")
  k <- .keep_study_yml_keys(new, old)
  # the unknown settings: key at the end of the new settings:, the unknown
  # blocks at the end in their old order, each with its comment lines; the
  # writer's own keys (title, rounding, style, output_path, keys, tokens,
  # updated) as the writer has them, the ones it left out not back
  expect_identical(k$lines, c(new[1:5], "  # my own", "  my_key:", "  - a", "  - b", new[6:8],
                              "# by hand", "formats:", "  pct: '%.1f'",
                              "# The AI kit", "ai:", "  language: Japanese", "texts:", "  a: b"))
  expect_identical(k$kept, list(top = c("formats", "ai", "texts"), settings = "my_key"))
  # the values read: the kept ones as the old file has them
  mine <- list(study_id = "X", settings = list(rounding = "r"))
  was <- list(study_id = "X", title = "Old", settings = list(rounding = "sas", style = "mine", my_key = c("a", "b")),
              formats = list(pct = "%.1f"), ai = list(language = "Japanese"))
  expect_identical(.keep_study_yml_values(mine, was, k$kept),
                   list(study_id = "X", settings = list(rounding = "r", my_key = c("a", "b")),
                        formats = list(pct = "%.1f"), ai = list(language = "Japanese")))
  # a key the new text has: the new one, once
  k2 <- .keep_study_yml_keys(c(new, "ai:", "  language: English"), old)
  expect_identical(sum(k2$lines == "ai:"), 1L)
  expect_true("  language: English" %in% k2$lines)
  expect_false("  language: Japanese" %in% k2$lines)
  # no settings: in the new text: one of its own; an old flow map's keys as
  # the YAML package writes them; an old block at 4 spaces at 2
  k3 <- .keep_study_yml_keys(new[-(4:5)], c("study_id: X", "settings: {rounding: sas, extra: 3}"))
  expect_identical(k3$lines, c(new[-(4:5)], "settings:", "  extra: 3"))
  k4 <- .keep_study_yml_keys(new, c("settings:", "    rounding: sas", "    deep:", "      a: 1"))
  expect_identical(k4$lines, c(new[1:5], "  deep:", "    a: 1", new[6:8]))
  # nothing before: the writer's text
  expect_identical(.keep_study_yml_keys(new, character())$lines, new)
  expect_identical(.keep_study_yml_keys(new, character())$kept, list(top = character(), settings = character()))
})

test_that("a study written whole keeps what of the folder's study.yml the writer does not write (tflspec#910)", {
  root <- tempfile("keepkeys")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines(c("study_id: X", "settings:", "  rounding: sas", "  my_key: kept",
               "# by hand", "formats:", "  pct: '%.1f'", "ai:", "  language: Japanese"),
             file.path(root, "study.yml"))
  p <- list(spec = list(text = list(study.yml = c("study_id: X", "settings:", "  rounding: r")),
                        study = NULL))
  q <- .spec_keep_file_blocks(p, root)
  expect_identical(q$spec$text[["study.yml"]],
                   c("study_id: X", "settings:", "  rounding: r", "  my_key: kept",
                     "# by hand", "formats:", "  pct: '%.1f'", "ai:", "  language: Japanese"))
})

test_that("a save keeps study.yml's unknown blocks, settings: keys and ai:; the writer's keys change (tflspec#910)", {
  f <- keys_study()
  before <- block_of(f)
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  at <- which(x == "settings:")[1L]
  if (is.na(at)) {
    x <- c(x, "settings:", "  my_key: kept")
  } else {
    end <- at
    while (end < length(x) && grepl("^[ ]", x[end + 1L])) end <- end + 1L
    x <- append(x, "  my_key: kept", after = end)
  }
  extra <- c("# by hand", "formats:", "  pct: '%.1f'")
  writeLines(c(x, extra), f, useBytes = TRUE)
  s <- suppressMessages(open_study("KS-1"))
  # a change that rewrites study.yml: the writer's keys change, the others stay
  s$planner$study[["rounding"]] <- "r"
  s$meta$title <- "A new title"
  s <- suppressMessages(save_study(s))
  expect_identical(block_of(f)$title, "A new title")
  for (round in 1:2) {
    out <- readLines(f, encoding = "UTF-8", warn = FALSE)
    at <- which(out == extra[1L])
    expect_length(at, 1L)
    expect_identical(out[at + seq_along(extra) - 1L], extra)
    expect_identical(sum(out == "  my_key: kept"), 1L)
    at <- which(out == "ai:")
    expect_length(at, 1L)
    expect_identical(out[at + seq_along(before$ai) - 1L], before$ai)
    y <- yaml::read_yaml(f)
    # (the rounding: defaults.yml force:, not study.yml's settings)
    expect_null(y$settings$rounding)
    expect_identical(tflspec::tfl_read_study_spec(dirname(f))$force$content[[1L]]$args$rounding, "r")
    expect_identical(y$settings$my_key, "kept")
    expect_identical(y$formats, list(pct = "%.1f"))
    expect_identical(y$ai$language, "ja")
    # the spec in memory says so too
    expect_identical(s$planner$spec$study$study$settings$my_key, "kept")
    expect_identical(s$planner$spec$study$study$formats, list(pct = "%.1f"))
    expect_identical(s$planner$spec$study$study$ai$language, "ja")
    expect_true(all(spec_status("KS-1")$status == "same"))
    # (and the next save keeps them too, once)
    s$meta$title <- "Another title"
    s <- suppressMessages(save_study(s))
    expect_identical(block_of(f)$title, "Another title")
  }
})
