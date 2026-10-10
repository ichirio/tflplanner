# Shared by the app's tests: a home of its own, two studies, a server
# started on one of them.

local_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

two_studies <- function() {
  p1 <- add_output(new_planner(), "A", data_code = "a <- 1",
                   description = "first")
  p2 <- add_output(new_planner(), "B", data_code = "b <- 2",
                   description = "second")
  create_study("S1", planner = p1)
  create_study("S2", planner = p2)
}

# The whole app's server, started on a study.  5-10 s a block: off on CRAN
# (its check time), run on CI and locally (NOT_CRAN=true)
server_for <- function(study) {
  testthat::skip_on_cran()
  start <- open_study(study)
  function(input, output, session) app_server(input, output, session, start)
}
