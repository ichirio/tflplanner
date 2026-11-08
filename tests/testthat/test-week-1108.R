# Tests that pin what the generated program writes, and what it does when
# run in a child R from the study folder, for the features of the week of
# 2026-11-08: a table's Rework (#599), a template listing's program and
# pages (#603), the fitted widths (#608), a template figure that reads an
# ARD (#602) and the stale-say window of the template card (#596).

skip_on_cran()

# the program of a report, run in its own R from the study folder
run_program <- function(s, id, dir = "tfl") {
  owd <- setwd(s$path)
  on.exit(setwd(owd), add = TRUE)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  file.path("programs", dir, paste0(id, ".R")),
                                  stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, out = paste(out, collapse = "\n"))
}
rtf_lines <- function(s, id) {
  f <- file.path(s$path, "output", "tfl", paste0(id, ".rtf"))
  if (file.exists(f)) readLines(f, warn = FALSE) else character()
}
count_in <- function(x, pattern) sum(lengths(regmatches(x, gregexpr(pattern, x, fixed = TRUE))))

sample_in_home <- function(env = parent.frame()) {
  home <- local_home(env)
  list(home = home, s = suppressMessages(create_sample_study(run = FALSE, home = home)))
}

# ---- #599: a table's Rework runs after normalize_ard() ---------------------

rework_run <- function(h, proc, id = "T-14-1-1") {
  s <- h$s
  s$planner$outputs$process_code[s$planner$outputs$output_id == id] <- proc
  s <- suppressMessages(save_study(s, home = h$home))
  suppressMessages(update_study_ard(s, id))
  unlink(file.path(s$path, "output", "tfl", paste0(id, ".rtf")))
  list(s = s, run = run_program(s, id))
}

test_that("a Rework is in the saved program after normalize_ard(), and its `data` is what the table reads", {
  h <- sample_in_home()
  r <- rework_run(h, "data <- data[data$variable != \"RACE\", ]")
  prog <- readLines(file.path(r$s$path, "programs", "tfl", "T-14-1-1.R"), warn = FALSE)
  i <- match("# normalize and rework", prog)
  expect_false(is.na(i))
  expect_identical(prog[i + 1:2], c("data <- ard |> normalize_ard()",
                                    "data <- data[data$variable != \"RACE\", ]"))
  # (the plan is made from `data`, after the Rework)
  expect_true(which(grepl("table_plan(data", prog, fixed = TRUE))[1L] > i + 2L)
  expect_false(any(grepl("table_plan(ard", prog, fixed = TRUE)))
  # run: the table has what the Rework left, and not the rows it took away
  expect_identical(r$run$status, 0L, info = r$run$out)
  rtf <- rtf_lines(r$s, "T-14-1-1")
  expect_gt(length(rtf), 0L)
  expect_gt(count_in(rtf, "Sex, n (%)"), 0L)
  expect_identical(count_in(rtf, "Race, n (%)"), 0L)
  # the same table with no Rework has the rows
  r0 <- rework_run(h, NA_character_)
  expect_identical(r0$run$status, 0L, info = r0$run$out)
  expect_gt(count_in(rtf_lines(r0$s, "T-14-1-1"), "Race, n (%)"), 0L)
})

test_that("a Rework that does not touch `data` gives the stop, in the program and when it runs", {
  h <- sample_in_home()
  r <- rework_run(h, "ard <- ard")
  prog <- readLines(file.path(r$s$path, "programs", "tfl", "T-14-1-1.R"), warn = FALSE)
  expect_true(any(grepl("^stop\\(\"tflplanner: the definition of T-14-1-1 does not hold: the data.steps leave no `data`",
                        prog)))
  expect_false(any(grepl("table_plan(", prog, fixed = TRUE)))
  expect_false(identical(r$run$status, 0L))
  expect_match(r$run$out, "the data.steps leave no `data`", fixed = TRUE)
  expect_identical(length(rtf_lines(r$s, "T-14-1-1")), 0L)
})

# ---- #603: a template listing is written and previewed from its content ----

tpl_listing <- function(h, where = NULL, id = "L-TPL") {
  args <- list(data = "ADAE",
               columns = list(list(vars = "USUBJID", label = "Subject"),
                              list(vars = "AEDECOD", label = "Term", width = 30L)))
  if (!is.null(where)) args$where <- where
  o <- tflspec::tfl_output(id, "listing", label = "AE listing",
                           template = list(name = "listing", args = args))
  s <- h$s
  s$planner <- .spec_add_output(s$planner, o, label = "AE listing")
  suppressWarnings(suppressMessages(save_study(s, home = h$home)))
}

test_that("a template listing's saved program plans with plan_listing and plan_paginate_rows, from `data`", {
  h <- sample_in_home()
  s <- tpl_listing(h)
  prog <- readLines(file.path(s$path, "programs", "tfl", "L-TPL.R"), warn = FALSE)
  expect_true(any(grepl("plan_listing(", prog, fixed = TRUE)))
  expect_true(any(grepl("plan_paginate_rows(max_rows = 22)", prog, fixed = TRUE)))
  expect_true(any(grepl("table_plan(data)", prog, fixed = TRUE)))
  expect_false(any(grepl("listing_spec|as_rtftables", prog)))
  # the paging comes after the listing
  expect_true(which(grepl("plan_paginate_rows", prog, fixed = TRUE))[1L] >
                which(grepl("plan_listing(", prog, fixed = TRUE))[1L])
  expect_silent(parse(text = prog))
})

test_that("a template listing's RTF has the pages the preview shows, for several row counts", {
  h <- sample_in_home()
  # (a listing template has no rows-a-page argument: it pages at 22, so the
  # pages move with the records kept)
  for (w in list(NULL, "AESEV == 'SEVERE'", "AESEV == 'MODERATE'", "AESEV == 'MILD' & AESER == 'Y'")) {
    s <- tpl_listing(h, where = w)
    pages <- preview_listing(s, "L-TPL")
    n <- vapply(pages, function(p) nrow(p$data), 1L)
    label <- paste("where:", w %||% "(none)")
    expect_true(all(n <= 22L), info = label)
    expect_gt(length(pages), 0L)
    run <- run_program(s, "L-TPL")
    expect_identical(run$status, 0L, info = run$out)
    # (a section a page: as many page breaks as pages)
    expect_identical(count_in(rtf_lines(s, "L-TPL"), "sbkpage"), length(pages), info = label)
  }
})

test_that("a code listing's RTF has the pages of the preview for several `max_rows`", {
  h <- sample_in_home()
  col <- function(v, ...) list(fn = "listing_col", args = c(list(vars = v), list(...)))
  for (mr in c(10L, 22L, 50L)) {
    id <- sprintf("L-C%d", mr)
    o <- tflspec::tfl_output(id, "listing", data = list(source = "dataset", dataset = "ADAE"),
      content = list(tflspec::tfl_item("plan_listing", list(cols = list(col("USUBJID"), col("AEDECOD", width = 30)),
                                                           type = "multiline")),
                     tflspec::tfl_item("plan_paginate_rows", list(max_rows = mr))))
    s <- h$s
    s$planner <- .spec_add_output(s$planner, o, label = "Code listing")
    s <- suppressWarnings(suppressMessages(save_study(s, home = h$home, fill_widths = TRUE)))
    h$s <- s
    prog <- readLines(file.path(s$path, "programs", "tfl", paste0(id, ".R")), warn = FALSE)
    expect_true(any(grepl(sprintf("max_rows = %d)", mr), prog, fixed = TRUE)))
    pages <- preview_listing(s, id)
    expect_true(all(vapply(pages, function(p) nrow(p$data), 1L) <= mr))
    run <- run_program(s, id)
    expect_identical(run$status, 0L, info = run$out)
    expect_identical(count_in(rtf_lines(s, id), "sbkpage"), length(pages), info = id)
  }
})

test_that("a code listing's program names its columns, whether or not the save fills widths", {
  h <- sample_in_home()
  col <- function(v, ...) list(fn = "listing_col", args = c(list(vars = v), list(...)))
  o <- tflspec::tfl_output("L-C1", "listing", data = list(source = "dataset", dataset = "ADAE"),
    content = list(tflspec::tfl_item("plan_listing", list(cols = list(col("USUBJID"), col("AEDECOD", width = 30))))))
  s <- h$s
  s$planner <- .spec_add_output(s$planner, o, label = "Code listing")
  s <- suppressWarnings(suppressMessages(save_study(s, home = h$home)))
  prog <- readLines(file.path(s$path, "programs", "tfl", "L-C1.R"), warn = FALSE)
  expect_false(any(grepl("listing_spec(list()", prog, fixed = TRUE)))
  expect_identical(run_program(s, "L-C1")$status, 0L)
})

# ---- #608: a listing's fitted widths: the yml, the program, the editor -----

test_that("a second save keeps the widths of the listings: the yml's are the program's, a typed one stays", {
  h <- sample_in_home()
  s <- suppressMessages(open_study("SAMPLE-01", home = h$home))
  tpl <- tflspec::tfl_output("L-W2", "listing", template = list(name = "listing", args = list(
    data = "ADAE", columns = list(list(vars = "USUBJID"), list(vars = "TRT01A"),
                                  list(vars = "AEDECOD", width = 31), list(vars = "AESEV")))))
  col <- function(v, ...) list(fn = "listing_col", args = c(list(vars = v), list(...)))
  code <- tflspec::tfl_output("L-W1", "listing", data = list(source = "dataset", dataset = "ADAE"),
    content = list(tflspec::tfl_item("plan_listing", list(cols = list(
      col("USUBJID"), col("TRT01A"), col("AEDECOD", width = 31), col("AESEV"))))))
  s$planner <- .spec_add_output(s$planner, code, label = "Code")
  s$planner <- .spec_add_output(s$planner, tpl, label = "Template")
  widths <- function(s, id) {
    o <- tflspec::tfl_read_output(file.path(s$path, "spec", "outputs", paste0(id, ".yml")))
    if (id == "L-W1") {
      vapply(o$content[[1L]]$args$cols, function(cl) cl$args$width %||% NA_real_, 0)
    } else vapply(o$template$args$columns, function(cl) cl$width %||% NA_real_, 0)
  }
  in_program <- function(s, id) {
    prog <- readLines(file.path(s$path, "programs", "tfl", paste0(id, ".R")), warn = FALSE)
    cols <- grep("listing_col[(].*width = ", prog, value = TRUE)
    as.numeric(sub("[,)].*", "", sub(".*width = ", "", cols)))
  }
  ids <- c("L-W1", "L-W2")
  s <- suppressMessages(save_study(s, home = h$home, fill_widths = TRUE))
  first <- lapply(ids, function(id) widths(s, id))
  skip_if(anyNA(unlist(first)), "this tflspec does not fit a listing's widths")
  s <- suppressMessages(save_study(s, home = h$home, fill_widths = TRUE))
  for (k in 1:2) {
    id <- ids[k]
    w <- widths(s, id)
    expect_identical(w, first[[k]], info = id)       # nothing moves on a second save
    expect_equal(w[3L], 31, info = id)               # the typed width stays
    expect_equal(in_program(s, id), w, info = id)    # the program says the file's
    expect_true(all(w > 0), info = id)
  }
})

# ---- #602: a template figure that reads an ARD -----------------------------

tpl_fig_study <- function(h, id = "F-9-1") {
  s <- h$s
  p <- s$planner
  pops <- p$ard$populations
  saf <- as.list(pops[match("SAF", pops$population_id),
                      intersect(c("population_id", "dataset", "where"), names(pops)), drop = FALSE])
  name <- "forest_hr"
  steps <- if (.tpl_own_data(tflspec::tfl_template_params(name))) .tpl_steps_from("ADTTE", saf, "PFS")
  o <- tflspec::tfl_output(id, "figure", label = name, population = "SAF",
                           template = list(name = name, args = list(param = "PFS", population = "SAF")),
                           data = if (length(steps)) list(steps = steps))
  s$planner <- .spec_add_output(p, o, label = name, population = "SAF")
  suppressWarnings(suppressMessages(save_study(s, home = h$home)))
}

test_that("a forest_hr template figure's program makes and reads its ARD, and its RTF has the figure", {
  skip_if_not_installed("cardx")
  skip_if_not_installed("ggplot2")
  h <- sample_in_home()
  s <- tpl_fig_study(h)
  expect_true(file.exists(file.path(s$path, "programs", "ard", "F-9-1.R")))
  prog <- readLines(file.path(s$path, "programs", "tfl", "F-9-1.R"), warn = FALSE)
  expect_false(any(grepl("reads an ARD: choose", prog, fixed = TRUE)))
  expect_true(any(grepl("readRDS(", prog, fixed = TRUE)))
  expect_true(any(grepl("F-9-1", prog[grepl("output_id", prog, fixed = TRUE)], fixed = TRUE)))
  # the ARD program makes the rows of its own
  made <- run_program(s, "F-9-1", dir = "ard")
  expect_identical(made$status, 0L, info = made$out)
  f <- list.files(file.path(s$path, "output", "ard"), "[.]rds$", full.names = TRUE)
  expect_gt(length(f), 0L)
  ard <- readRDS(f[1L])
  expect_true("F-9-1" %in% as.character(ard$output_id))
  # the figure's program reads it and writes the figure
  run <- run_program(s, "F-9-1")
  expect_identical(run$status, 0L, info = run$out)
  rtf <- rtf_lines(s, "F-9-1")
  expect_gt(length(rtf), 0L)
  expect_gt(count_in(rtf, "\\pict"), 0L)
})

# ---- #596: the stale-say window of the template card -----------------------

test_that(".tpl_stale_say: the window's boundary is inside, just past it is out", {
  past <- data.frame(v = "a", t = 100)
  expect_true(.tpl_stale_say("a", past, now = 100))
  expect_true(.tpl_stale_say("a", past, now = 115))                 # 15 s: in
  expect_false(.tpl_stale_say("a", past, now = 115.001))            # past it: out
  expect_true(.tpl_stale_say("a", past, now = 109, window = 10))
  expect_false(.tpl_stale_say("a", past, now = 110.5, window = 10))
  expect_false(.tpl_stale_say("a", past, now = 100, window = -1))
})

test_that(".tpl_stale_say: several entries are each looked at; the old and the repeated", {
  past <- data.frame(v = c("a", "b", "a", "c"), t = c(10, 100, 105, 110))
  # "a" held twice: the older is out of the window, the newer is in
  expect_true(.tpl_stale_say("a", past, now = 112))
  expect_true(.tpl_stale_say("b", past, now = 112))
  expect_true(.tpl_stale_say("c", past, now = 112))
  expect_true(.tpl_stale_say("a", past, now = 118))                 # the newer a (105)
  expect_false(.tpl_stale_say("a", past, now = 125))                # both out
  expect_true(.tpl_stale_say("c", past, now = 125))                 # c at 110: 15 s
  expect_false(.tpl_stale_say("b", past, now = 125))
  # one value of several lines is one value
  multi <- data.frame(v = "x\ny", t = 100)
  expect_true(.tpl_stale_say("x\ny", multi, now = 101))
  expect_false(.tpl_stale_say("x", multi, now = 101))
  expect_false(.tpl_stale_say("", multi, now = 101))
})

test_that(".tpl_stale_say: another report's or argument's past is not this one's", {
  # (as the server keeps them: a data frame a key of report, template and argument)
  past <- new.env()
  past[["L-1 listing columns"]] <- data.frame(v = "a", t = 100)
  past[["L-2 listing columns"]] <- data.frame(v = "b", t = 100)
  past[["L-1 listing where"]] <- data.frame(v = "c", t = 100)
  say <- function(key, val) .tpl_stale_say(val, past[[key]], now = 105)
  expect_true(say("L-1 listing columns", "a"))
  expect_false(say("L-1 listing columns", "b"))     # L-2's value
  expect_false(say("L-1 listing columns", "c"))     # another argument's
  expect_true(say("L-2 listing columns", "b"))
  expect_false(say("L-2 listing columns", "a"))
  expect_false(say("L-3 listing columns", "a"))     # a report that has none
  expect_false(say("L-1 listing sort", "a"))        # an argument that has none
})
