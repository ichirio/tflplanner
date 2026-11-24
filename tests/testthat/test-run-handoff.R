# The run after handoff (the user, 2026-11-24): 「なくしてください。tflspecなしで
# 実行するなら、生成済みのバッチフォルダ内のautoexec_*()を直接実行することに
# なります。FDAなどに渡した後の実行はこの形です。」  A batch folder copied out
# of the study's runs/ runs with its own autoexec_all.R, no arguments, in an R
# library of only the packages its renv.lock lists (the receiver's
# renv::restore()), and makes the same reports.  tflplanner is never needed;
# tflspec only when a program calls it ("rtfreporterも同じなので、tflspecを
# renv.lockに書いてインストールしてもらえばよいですね").

test_that("a batch folder handed over runs with only its renv.lock's packages and makes the same reports", {
  skip_on_cran()
  local_home()
  s <- suppressMessages(sample_study_copy())
  b <- run_def(s, "Handoff", "all", copy_data = TRUE)
  expect_true(dir.exists(file.path(b$batch, "data")))
  lock_file <- file.path(b$batch, "renv.lock")
  expect_true(file.exists(lock_file))
  lock <- jsonlite::fromJSON(lock_file, simplifyVector = FALSE)
  pk <- names(lock$Packages)
  expect_true(all(c("rtfreporter", "cards") %in% pk))
  expect_false("tflplanner" %in% pk)
  # tflspec is in it exactly when a program calls it
  progs <- list.files(file.path(b$batch, "programs"), "[.][Rr]$", recursive = TRUE,
                      full.names = TRUE)
  progs <- progs[basename(progs) != "batch.R"]
  calls <- vapply(progs, function(f) {
    any(grepl("tflspec::", sub("#.*$", "", readLines(f, warn = FALSE, encoding = "UTF-8")),
              fixed = TRUE))
  }, NA)
  expect_identical("tflspec" %in% pk, any(calls))

  # handed over: the folder copied out of the study
  to <- withr_tempdir()
  expect_true(file.copy(b$batch, to, recursive = TRUE))
  h <- file.path(to, basename(b$batch))
  # the receiver's library: only the lockfile's packages (no Suggests, no
  # tflplanner), at the versions the run had
  lib <- file.path(to, "lib")
  none <- file.path(to, "none")
  dir.create(lib)
  dir.create(none)
  for (p in pk) expect_true(file.copy(find.package(p), lib, recursive = TRUE), info = p)
  env <- c("current", R_LIBS = lib, R_LIBS_USER = none, R_LIBS_SITE = none)
  rs <- file.path(R.home("bin"), "Rscript")
  have <- processx::run(rs, c("-e", paste0(
    "cat(vapply(c('tflspec', 'tflplanner'), requireNamespace, NA, quietly = TRUE))")),
    env = env, error_on_status = FALSE)$stdout
  if (grepl("TRUE$", trimws(have))) skip("tflplanner is in R's own library here")
  expect_identical(trimws(have), paste("tflspec" %in% pk, "FALSE"))

  r <- processx::run(rs, "autoexec_all.R", wd = h, env = env, error_on_status = FALSE,
                     stderr_to_stdout = TRUE)
  expect_identical(r$status, 0L, info = r$stdout)
  cmp <- tflspec::tfl_compare_runs(b$batch, h)
  expect_length(attr(cmp, "changed")$outputs, 0L)
  expect_true(length(list.files(file.path(h, "output", "tfl"), "[.]rtf$")) > 0L)
  # nothing was made in the study by the handed-over run
  expect_length(list.dirs(file.path(s$path, "runs"), recursive = FALSE), 1L)
})
