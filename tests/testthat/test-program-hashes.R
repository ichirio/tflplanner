# The programs' checksums kept between sessions (R/program_hashes.R, #749):
# a new session's first save makes no program it does not write, and writes
# the same files as without the record.

# A new R session as far as the save is concerned: every cache of the spec
# and the programs emptied, the record read again from its file
hashes_fresh <- function() {
  for (e in .spec_base_cache$entries) unlink(e$was$dir, recursive = TRUE)
  .spec_base_cache$entries <- NULL
  for (k in c("out", "p", "meta", "codelists")) .spec_sync_cache[[k]] <- NULL
  .ard_spec_cache$key <- .ard_spec_cache$spec <- NULL
  for (e in list(.program_cache, .ard_prog_cache, .put_cache, .state_cache, .batch_cache)) {
    rm(list = ls(e, all.names = TRUE), envir = e)
  }
  .code_hash_flush()
  builds <- .code_hash_rec$builds
  rm(list = ls(.code_hash_rec, all.names = TRUE), envir = .code_hash_rec)
  .code_hash_rec$builds <- builds
  invisible()
}

# The files a save writes, their bytes by md5 (study.yml without the time
# it was saved)
hashes_files <- function(root) {
  f <- list.files(root, recursive = TRUE, all.files = TRUE)
  f <- f[grepl("^(spec|programs)/|^study\\.yml$", f) & !grepl("(^|/)\\.edited/|^spec/\\.rejected/", f)]
  md5 <- stats::setNames(unname(tools::md5sum(file.path(root, f))), f)
  yml <- readLines(file.path(root, "study.yml"), encoding = "UTF-8", warn = FALSE)
  md5[["study.yml"]] <- paste(yml[!startsWith(yml, "updated:")], collapse = "\n")
  md5[order(names(md5))]
}

# The short title changed by one character and saved as the app saves
# (merged with the study as opened, the widths filled).  `count()`: the
# programs made.  The save's result.
hashes_title_save <- function(id) {
  s <- open_study(id)
  st <- s
  st$meta$short_title <- paste0(s$meta$short_title %||% "", "X")
  suppressMessages(save_study(st, base = s, fill_widths = TRUE))
}

hashes_counting <- function(env = parent.frame()) {
  n <- new.env()
  prog <- program_code
  ard <- ard_program_code
  testthat::local_mocked_bindings(
    program_code = function(...) {
      n$report <- (n$report %||% 0L) + 1L
      prog(...)
    },
    ard_program_code = function(...) {
      n$ard <- (n$ard %||% 0L) + 1L
      ard(...)
    },
    .env = env)
  function() c(report = n$report %||% 0L, ard = n$ard %||% 0L)
}

hashes_new_copy <- function(old = NULL) {
  if (!is.null(old)) {
    unregister_study(old$meta$study_id)
    unlink(old$path, recursive = TRUE)
  }
  suppressMessages(sample_study_copy())
}

test_that("a new session's first save makes no program and writes the same files as without the record", {
  skip_on_cran()
  local_home()
  count <- hashes_counting()
  s <- hashes_new_copy()
  .use_study(s$path)
  skip_if(is.null(.code_hash_env()), "tflplanner, tflspec or rtfreporter has no build time (load_all())")
  # without a record (its file removed): every program made to compare it
  hashes_fresh()
  unlink(file.path(tflplanner_home(), "cache"), recursive = TRUE)
  n0 <- count()
  without <- hashes_title_save(s$meta$study_id)
  made <- count() - n0
  expect_gt(made[["report"]], 10L)
  expect_gt(made[["ard"]], 0L)
  files_without <- hashes_files(s$path)
  # the record that save left
  expect_length(list.files(file.path(tflplanner_home(), "cache", "programs")), 1L)
  # the same study again, the same edit, in a new session with the record
  s <- hashes_new_copy(s)
  hashes_fresh()
  n0 <- count()
  with <- hashes_title_save(s$meta$study_id)
  expect_identical(count() - n0, c(report = 0L, ard = 0L))
  expect_identical(hashes_files(s$path), files_without)
  expect_identical(with$files$status, without$files$status)
  expect_identical(basename(with$files$file), basename(without$files$file))
})

test_that("a program edited by hand is written again whatever the record says", {
  skip_on_cran()
  local_home()
  s <- hashes_new_copy()
  .use_study(s$path)
  skip_if(is.null(.code_hash_env()), "tflplanner, tflspec or rtfreporter has no build time (load_all())")
  hashes_fresh()
  hashes_title_save(s$meta$study_id)
  lay <- study_layout()
  id <- s$planner$outputs$output_id[1L]
  rep_f <- file.path(s$path, lay[["programs_tfl"]], report_info(s$planner, id)$program)
  ard_f <- list.files(file.path(s$path, lay[["programs_ard"]]), "^[TFL]-.*\\.R$", full.names = TRUE)[1L]
  for (f in c(rep_f, ard_f)) {
    expect_true(file.exists(f))
    cat("# by hand\n", file = f, append = TRUE)
  }
  hashes_fresh()
  out <- hashes_title_save(s$meta$study_id)
  st <- stats::setNames(out$files$status, normalizePath(out$files$file, "/", mustWork = FALSE))
  expect_identical(unname(st[normalizePath(c(rep_f, ard_f), "/")]), c("rewritten", "rewritten"))
  for (f in c(rep_f, ard_f)) {
    expect_false(any(grepl("# by hand", readLines(f, warn = FALSE), fixed = TRUE)))
  }
})

test_that("the record keeps a few input md5s a program, the newest first, and its file whole", {
  local_home()
  local_code_context()
  .code_context$root <- file.path(tflplanner_home(), "a-study")
  builds <- .code_hash_rec$builds
  on.exit({
    rm(list = ls(.code_hash_rec, all.names = TRUE), envir = .code_hash_rec)
    .code_hash_rec$builds <- builds
  }, add = TRUE)
  rm(list = ls(.code_hash_rec, all.names = TRUE), envir = .code_hash_rec)
  .code_hash_rec$builds <- builds
  for (i in 1:5) .code_hash_put("T1", paste0("k", i), paste0("h", i))
  expect_identical(names(.code_hash_rec$entries$T1), c("k5", "k4", "k3"))
  expect_identical(.code_hash_get("T1", "k4"), "h4")
  expect_null(.code_hash_get("T1", "k1"))
  expect_null(.code_hash_get("T1", NULL))
  expect_null(.code_hash_get("T2", "k5"))
  .code_hash_flush()
  f <- .code_hash_file()
  expect_true(file.exists(f))
  expect_length(list.files(dirname(f)), 1L)
  # read back in a new session
  rm(list = setdiff(ls(.code_hash_rec, all.names = TRUE), "builds"), envir = .code_hash_rec)
  expect_identical(.code_hash_get("T1", "k5"), "h5")
  # a file that does not read: an empty record
  writeLines("not an rds", f)
  rm(list = setdiff(ls(.code_hash_rec, all.names = TRUE), "builds"), envir = .code_hash_rec)
  expect_null(.code_hash_get("T1", "k5"))
  # no study: nothing kept
  .code_context$root <- NULL
  expect_null(.code_hash_key("report", "T1", list()))
  expect_null(.code_hash_entries())
})

test_that("a program file's own checksum, only while its body has it", {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f), add = TRUE)
  expect_identical(.program_file_hash(f), NA_character_)
  .write_program(c("# ====", "#  Generated  : tflplanner, 2026-01-01", "# ====", "x <- 1"), f)
  h <- .program_file_hash(f)
  expect_match(h, "^[0-9a-f]{32}$")
  expect_identical(h, .body_hash(readLines(f)))
  cat("y <- 2\n", file = f, append = TRUE)
  expect_identical(.program_file_hash(f), NA_character_)
  writeLines("x <- 1", f)
  expect_identical(.program_file_hash(f), NA_character_)
})
