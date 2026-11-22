withr_tempdir <- function(env = parent.frame()) {
  d <- tempfile("tflplanner-test")
  dir.create(d)
  withr_defer <- function() unlink(d, recursive = TRUE)
  do.call(on.exit, list(substitute(withr_defer()), add = TRUE), envir = env)
  d
}

# The app looks for a newer version on the network when it starts; not in
# the tests.
options(tflplanner.check_updates = FALSE)

# a figure design of the sample study (its YAML spec, as the app reads it)
sample_fig_design <- function(id) {
  .planner_from_yaml(
    system.file("sample", "SAMPLE-01", package = "tflplanner"))$fig_designs[[id]]
}

# The study code context (.use_study()) is the R session's: no test starts
# with one another test left (#680).  Reset when the helpers load, and for
# each test that makes a home (local_home()): none while it runs, the one
# before put back after it.
.code_context$opts <- NULL
.code_context$root <- NULL
local_code_context <- function(env = parent.frame()) {
  old <- list(opts = .code_context$opts, root = .code_context$root)
  .code_context$opts <- NULL
  .code_context$root <- NULL
  restore <- function() {
    .code_context$opts <- old$opts
    .code_context$root <- old$root
  }
  do.call(on.exit, list(substitute(restore()), add = TRUE), envir = env)
  invisible(old)
}

# The slowest test files (the whole app's server driven through
# shiny::testServer(), whole-study builds) start with skip_if_quick(): the
# quick check on a pull request into next sets TFLPLANNER_QUICK=true and leaves
# them out.  They run everywhere else -- locally, on each push to next, on pull
# requests into main and nightly (R-CMD-check.yaml).
skip_if_quick <- function() {
  if (identical(tolower(Sys.getenv("TFLPLANNER_QUICK")), "true")) {
    testthat::skip("slow file: left out of the quick check (TFLPLANNER_QUICK=true)")
  }
  invisible(TRUE)
}

# The sample study as create_sample_study(run = FALSE) makes it, built once
# per R session (each test process) into a cache folder under tempdir() and
# taken off the cache home's list (unregister_study(): what tflplanner keeps
# goes into the folder, .tflplanner/).  A test gets its own copy of the
# folder, registered in its own home (register_study() takes the kept state
# back), its code context set as create_sample_study() sets it: about 1 s
# instead of 13 (#747).  The cache is never a test's study:
# each test copies it, so the tests stay independent.  test-sample*.R and
# test-study.R test create_sample_study() itself.
.sample_cache <- new.env(parent = emptyenv())
.sample_cache_dir <- function() {
  d <- .sample_cache$dir
  if (!is.null(d) && dir.exists(d)) return(d)
  h <- tempfile("tflplanner-sample-cache")
  dir.create(h)
  old_home <- options(tflplanner.home = h)
  # the test's code context and program cache as they were (as
  # .with_study_context() keeps them): the build leaves nothing behind
  old_ctx <- list(opts = .code_context$opts, root = .code_context$root)
  old_cache <- as.list(.program_cache, all.names = TRUE)
  on.exit({
    options(old_home)
    .code_context$opts <- old_ctx$opts
    .code_context$root <- old_ctx$root
    rm(list = ls(.program_cache, all.names = TRUE), envir = .program_cache)
    list2env(old_cache, envir = .program_cache)
  }, add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(h, "ws")))
  s <- suppressMessages(create_sample_study(run = FALSE, home = h))
  unregister_study(s$meta$study_id, home = h)
  .sample_cache$dir <- s$path
  s$path
}
sample_study_copy <- function(home = tflplanner_home()) {
  src <- .sample_cache_dir()
  root <- studies_root(home)
  to <- file.path(root, basename(src))
  if (file.exists(to)) stop("A folder '", basename(src), "' is already there: ", to, call. = FALSE)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(src, root, recursive = TRUE, copy.date = TRUE)) {
    stop("The sample study cache was not copied to ", root, call. = FALSE)
  }
  s <- suppressMessages(register_study(to, home = home))
  # the code context create_sample_study() leaves (its save_study()): the
  # programs written as this study's setup has them (#680)
  .use_study(s$path)
  s
}
