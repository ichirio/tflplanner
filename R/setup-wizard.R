# setup_tflplanner() with no arguments, in an interactive session: the
# three things done at the console, step by step, each asked before it is
# done.  Everything else is set in the app.

# The questions go through these two, so tests can answer them.
.ask_line <- function(prompt) readline(prompt)
.ask_yes <- function(question) isTRUE(utils::askYesNo(question))

.ask_path <- function(label, default) {
  a <- trimws(.ask_line(sprintf("%s [%s]: ", label, default)))
  a <- gsub('^"|"$', "", a)
  if (!nzchar(a)) a <- default
  normalizePath(path.expand(a), "/", mustWork = FALSE)
}

.setup_wizard <- function() {
  lang <- .console_language()
  t <- function(x) tr(x, lang)
  say <- function(...) cat(..., "\n", sep = "")
  say(t("tflplanner setup, in 3 steps. Press Enter to keep the value shown in [ ]."))

  # 1. the home and the study folders
  say("\n", t("1/3  Where tflplanner keeps its settings and the studies' saved state (its home)"))
  home <- .ask_path(t("Home folder"), tflplanner_home())
  root <- studies_root(home)
  root <- .ask_path(t("New study folders go to"), root)
  lng <- tflplanner_config(home)$language %||% lang
  a <- trimws(.ask_line(sprintf("%s (en / ja) [%s]: ", t("Language of the app"), lng)))
  if (a %in% app_languages()) lng <- a
  say(t("Home folder"), ": ", .show_path(home), "\n",
      t("New study folders go to"), ": ", .show_path(root), "\n",
      t("Language of the app"), ": ", lng)
  say(t("Saving them will:"))
  say("  - ", sprintf(t("create the folders %s and %s if they are not there"),
                      .show_path(home), .show_path(root)))
  say("  - ", sprintf(t("write the settings to %s"),
                      .show_path(.config_file(home))))
  say("  - ", sprintf(t("remember the home folder in %s"),
                      .show_path(.pointer_file())))
  say(t("Change them later with setup_tflplanner(home = , studies_root = ), or in the app's settings."))
  if (!.ask_yes(t("Save these settings?"))) {
    say(t("Nothing was changed."))
    return(invisible(NULL))
  }
  cfg <- suppressMessages(setup_tflplanner(home = home, studies_root = root,
                                           language = lng))
  lang <- lng

  # 2. the packages the programs use
  say("\n", t("2/3  Packages"))
  pk <- tflplanner_packages(check = FALSE)
  miss <- pk$package[pk$role == "suggested" & pk$status == "missing"]
  if (!length(miss)) {
    say(t("The packages tflplanner's programs use are all installed."))
  } else {
    say(t("Used by tflplanner's programs, not installed yet:"), " ",
        paste(miss, collapse = ", "))
    say(sprintf(t("They would be installed from CRAN with install.packages() into %s. tflplanner itself starts without them; the programs it writes need them."),
                .show_path(.libPaths()[1L])))
    if (.ask_yes(sprintf(t("Install these %d packages now?"), length(miss)))) {
      utils::install.packages(miss)
    } else {
      say(t("Not installed. Later:"), " install.packages(c(",
          paste0('"', miss, '"', collapse = ", "), "))")
    }
  }
  say(t("To update tflplanner, rtfreporter and tflspec later: update_tflplanner(), or the \"update and launch\" shortcut."))

  # 3. the shortcut
  say("\n", t("3/3  A shortcut that starts tflplanner with a double click"))
  plan <- .shortcut_plan(lang = lang)
  say(paste(.shortcut_plan_text(plan, lang), collapse = "\n"))
  if (.ask_yes(.shortcut_question(nrow(plan), lang))) {
    add_shortcut(ask = FALSE)
  } else {
    say(t("No shortcut was made. Later: add_shortcut()"))
  }

  say("\n", t("Done. Start tflplanner from its shortcut, or with launch_app() in R. Everything else is in the app's settings."))
  invisible(cfg)
}
