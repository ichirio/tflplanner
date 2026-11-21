# Shared by the app's tests: a home of its own, two studies, a server
# started on one of them.

local_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  local_code_context(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

two_studies <- function() {
  p1 <- add_output(new_planner(), "A", process_code = "a <- 1",
                   description = "first")
  p2 <- add_output(new_planner(), "B", process_code = "b <- 2",
                   description = "second")
  create_study("S1", planner = p1)
  create_study("S2", planner = p2)
}

# The whole app's server, started on a study.  5-10 s a block: off on CRAN
# (its check time), run on CI and locally (NOT_CRAN=true)
server_for <- function(study) {
  testthat::skip_on_cran()
  start <- open_study(study)
  function(input, output, session) {
    # (a form's edit is applied once the form is drawn, not 1.5 s after:
    # .form_live())
    session$userData$form_settle <- 0
    app_server(input, output, session, start)
  }
}

# Another report opened in a server_for() session.  With unsaved changes
# the app asks first (Save, Discard or Cancel: the forms apply their edits
# at once); a test that goes on with its edits answers Save, as a user who
# keeps them does.  Not asked (nothing unsaved): only the report opened.
# The other inputs (`...`: a step, a switch) are set on the report opened.
open_report <- function(session, target, ...) {
  session$setInputs(target = target)
  if (!identical(shiny::isolate(session$userData$current()), target)) {
    n <- (session$userData$switch_saves %||% 0) + 1
    session$userData$switch_saves <- n
    session$setInputs(switch_save = n)
  }
  if (...length()) session$setInputs(...)
  invisible()
}

# A template report's argument cards of every step, as one text: an
# argument is drawn in the step its `step:` names (data, analysis, content,
# page), the content step's card `main`
tpl_cards_html <- function(output, main = "tpl_card") {
  one <- function(o) tryCatch(paste(as.character(output[[o]]$html), collapse = ""),
                              error = function(e) "")
  paste(vapply(c("tpl_card_data", "tpl_card_ard", main, "tpl_card_page"), one, ""), collapse = " ")
}
