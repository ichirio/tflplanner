# The app.  A study is opened first; everything else is that study.  The
# study's planner lives in a reactiveValues and every grid shows a slice of
# one sheet (the rows of the report chosen in the sidebar) and writes the
# slice back when it is edited.  A grid is redrawn only when the slice it
# shows changes from outside (another report chosen, the study opened, a
# report copied, rows filled from the ARD), never because it was edited
# itself.  Nothing reaches the disk until the user saves.
#
# Every text is English here and translated by tr() (R/i18n.R).

.all_rows <- "__all__"
.default_rows <- "__default__"
.study_tabs <- c("data", "outputs", "make", "results")

.sheet_labels <- c(
  tables = "tables: roles", variables = "variables",
  codelists = "codelists: code list", cells = "cells",
  digits = "digits: decimals",
  layout = "layout: pages", columns = "columns", style = "style",
  cell_styles = "cell_styles: cell looks",
  col_header = "col_header: column header",
  report = "report", page = "page", header = "header", footer = "footer",
  titles = "titles", footnotes = "footnotes", tokens = "tokens: your own")

# The steps of making a report (the Make a report tab), by their tab values:
# their names here only, so a name changes in one place (and its Japanese)
.step_labels <- c(ard = "1 ARD", content = "2 Content", page = "3 Page and output")
# a kind's own names of the steps (a figure's step 1 is mostly its data; an
# ARD only sometimes, as a part of it)
.step_labels_of <- list(figure = c(ard = "1 Data", content = "2 Figure"))

.type_labels <- c(table = "Table", listing = "Listing", figure = "Figure")
# a new report's ID begins with its kind's prefix (the Add dialog)
.id_prefix <- c(table = "T-", listing = "L-", figure = "F-")
# the kinds in the Add dialog: tables, figures, listings (TFL)
.tfl_order <- c("table", "figure", "listing")
# the ID typed after the prefix (typed with it already: not twice)
.id_with_prefix <- function(id, kind) {
  id <- trimws(id %||% "")
  pre <- .id_prefix[kind %||% ""]
  if (!nzchar(id) || is.na(pre) || startsWith(toupper(id), toupper(pre))) return(id)
  paste0(pre, id)
}
# one line on each kind, where a report's kind is chosen
.type_notes <- list(
  table = "Table: made from the ARD and the table definition.",
  listing = "Listing: the records of a dataset, in columns you define.",
  figure = "Figure: made with the designer (layers on ggplot()), from a template or empty.")

# Where each kind of report is made, in the order of the work: the page
# (go()'s name) and the button's words.
.type_moves <- list(
  table = c(ard = "Go to ARD", tables = "Make the table"),
  figure = c(designer = "Make the figure"),
  listing = c(lf = "Make the listing"))

# The top tabs a kind of report has nothing on: shown faded while such a
# report is chosen (still open to click; the tab then says why).
.type_idle_tabs <- list(table = character(), figure = character(), listing = "ard")

# Text cut to `n` characters, with an ellipsis.
# What each study folder holds, by its study_layout() name (a folder with no
# note yet shows none, so a new folder never breaks the study tab).
.study_folder_notes <- function(nm, t = identity) {
  notes <- c(
    adam = "Input data: ADaM", sdtm = "Input data: SDTM",
    other = "Input data: other",
    spec = "Definition workbooks (written on save)",
    programs_ard = "ARD programs: one per output, ard_setup.R, autoexec_ard.R",
    programs_tfl = "Report programs, autoexec_report.R",
    ard = "Working data: the study ARD (ard.rds)",
    tfl = "Working reports: RTF",
    ard_import = "ARDs made elsewhere, taken in (imports.csv lists them)",
    toc_import = "TOCs taken in (imports.csv lists them; last.json: what the last one said)",
    runs = "Official runs: one run folder each (logs, results, code)",
    logs_preview = "What a report program printed in its last preview",
    docs = "Documents for reference: SAP, TOC / SHELL workbooks")
  out <- unname(notes[nm])
  has <- !is.na(out)
  out[has] <- vapply(out[has], function(x) t(x), character(1))
  out[!has] <- ""
  out
}

.ellipsis <- function(x, n) {
  ifelse(nchar(x) > n, paste0(substr(x, 1L, n - 1L), "\u2026"), x)
}

# A table cell as escaped HTML, with `tip` shown on hover (none when blank).
.cell_tip <- function(text, tip = "") {
  esc <- htmltools::htmlEscape
  ifelse(nzchar(tip),
         sprintf('<span title="%s">%s</span>', esc(tip, attribute = TRUE),
                 esc(text)),
         esc(text))
}

# the sheets whose tab offers help of its own above the grid
.assisted <- c("tables", "variables", "cells", "col_header")

# a report's ARD, in the same four words everywhere (ard_status()'s states)
.ard_state_labels <- c(built = "Made", outdated = "Outdated",
                       `not built` = "Not made", error = "Error")

# a report's run status in the four words (the status says why)
.run_state <- c(ok = "built", outdated = "outdated", unsaved = "outdated",
                todo = "not built", "not run" = "not built",
                "no program" = "not built", error = "error")

# why a report is to be made again (study_status()'s `why`), for the runs
# table: ": its program changed; T-14-2-2's ARD changed"
.why_text <- function(why, t = identity) {
  vapply(why, function(w) {
    w <- strsplit(w %||% "", " ", fixed = TRUE)[[1L]]
    w <- w[nzchar(w)]
    if (!length(w)) return("")
    txt <- vapply(w, function(k) {
      if (startsWith(k, "ard:")) sprintf(t("%s's ARD changed"), sub("^ard:", "", k)) else
        switch(k, program = t("its program changed"), setup = t("the study setup changed"), k)
    }, "")
    paste0(": ", paste(txt, collapse = "; "))
  }, "", USE.NAMES = FALSE)
}

.status_labels <- c(
  "no program" = "Not written (save)", unsaved = "Unsaved (save)",
  todo = "TODO (data part)", "not run" = "Not run", error = "Error",
  outdated = "Rerun needed", ok = "OK")

#' Start the tflplanner app
#'
#' Opens the study manager.  Choose a study -- it opens as it was last
#' saved -- or create or register one, then define its reports (Tables from
#' the table definition; Listings and Figures from their programs), their
#' data code, and run them.  The first time, tflplanner's home is set up
#' with the defaults ([setup_tflplanner()]).  The app is in English or
#' Japanese (`setup_tflplanner(language = )`, or its settings).
#'
#' @param study A study to open at start: a registered study's id, or a
#'   study folder.
#' @param ... Passed to [shiny::runApp()] (e.g. `port`).
#' @param launch.browser Where the app opens: by default the system's own
#'   web browser, as the desktop shortcut opens it -- also when R runs in
#'   RStudio, whose Viewer and Shiny window do not ask before a window with
#'   unsaved changes closes.  `FALSE` opens nothing (the address is printed);
#'   a function of the URL, or `TRUE` (shiny's
#'   `getOption("shiny.launch.browser")`), as [shiny::runApp()] takes it.
#' @param stop_on_close Stop the app when its last browser tab is closed
#'   (after a few seconds, so that reloading the page does not stop it).
#'   The shortcut and [launch_app()] start it this way.
#' @return `planner_app()` returns a [shiny::shinyApp()] object;
#'   `run_app()` runs it.
#' @seealso [launch_app()] to start it in its own R process, as the
#'   shortcut does ([add_shortcut()]).
#' @examples
#' if (interactive()) {
#'   run_app()
#'   run_app("ABC-101")
#' }
#' @export
run_app <- function(study = NULL, ..., stop_on_close = FALSE,
                    launch.browser = .external_browser) { # nolint: object_name_linter. shiny's name
  shiny::runApp(planner_app(study, stop_on_close = stop_on_close),
                launch.browser = launch.browser, ...)
}

# Is the app at an address of this computer (not a server)?
.is_local_host <- function(host) {
  isTRUE(host %in% c("localhost", "127.0.0.1", "::1", "[::1]"))
}

# Open a URL in the system's own web browser.  utils::browseURL() follows
# getOption("browser"), which RStudio replaces with its own (a localhost
# address then opens inside RStudio); the system is asked directly instead.
.external_browser <- function(url) {
  sys <- Sys.info()[["sysname"]]
  if (.Platform$OS.type == "windows") {
    shell.exec(url)
  } else if (identical(sys, "Darwin")) {
    system2("open", url, wait = FALSE)
  } else {
    utils::browseURL(url, browser = Sys.getenv("R_BROWSER", "xdg-open"))
  }
  invisible(url)
}

#' @rdname run_app
#' @export
planner_app <- function(study = NULL, stop_on_close = FALSE) {
  if (!.is_set_up()) setup_tflplanner(home = NULL)
  .refresh_launcher()
  .serve_health()
  # the study is checked here, once; each session (a new tab, a reload)
  # opens it again from its folder -- as last saved, not as it was when the
  # app started (a copy taken at start would be written back by a save)
  start_path <- if (!is.null(study)) open_study(study)$path
  shiny::shinyApp(function(req) app_ui(tflplanner_language()),
                  function(input, output, session) {
                    if (isTRUE(stop_on_close)) .stop_when_closed(session)
                    start <- .session_study(start_path)
                    app_server(input, output, session, start)
                  })
}

# What the launcher asks a running app, at /tflplanner-health/health.txt:
# its version and its process, so a second start can open the same version
# and offer to stop an older one (inst/launcher/launch.R)
.serve_health <- function() {
  d <- file.path(tempdir(), "tflplanner-health")
  dir.create(d, showWarnings = FALSE)
  writeLines(c(paste("tflplanner", as.character(utils::packageVersion("tflplanner"))),
               paste("pid", Sys.getpid())), file.path(d, "health.txt"))
  shiny::addResourcePath("tflplanner-health", d)
  invisible(d)
}

# the study a session starts on, read from its folder now; NULL (the last
# study, as with no study given) when it cannot be opened any more
.session_study <- function(path) {
  if (is.null(path)) return(NULL)
  tryCatch(open_study(path), error = function(e) NULL)
}

# Stop the app once no browser tab is left: a tab that closes starts a
# short wait, and the app stops if no tab has come back by then (a reload
# ends one session and starts the next within it).  A session that an
# error ended (the page goes grey) waits much longer (`wait_failed`): the
# tab is still there, and its Reload brings the app back.
.open_tabs <- new.env()
.open_tabs$n <- 0L

.stop_when_closed <- function(session, wait = 5, wait_failed = 600) {
  .open_tabs$n <- .open_tabs$n + 1L
  failed <- FALSE
  session$onUnhandledError(function(e) {
    failed <<- TRUE
    message("tflplanner: the session stopped on an error: ",
            conditionMessage(e))
  })
  session$onSessionEnded(function() {
    .open_tabs$n <- .open_tabs$n - 1L
    later::later(function() {
      if (.open_tabs$n <= 0L) shiny::stopApp()
    }, if (failed) wait_failed else wait)
  })
}

app_server <- function(input, output, session, start) {
  .srv_part(.srv_study, environment())
  .srv_part(.srv_report_list, environment())
  .srv_part(.srv_ard_figure, environment())
  .srv_part(.srv_ard_make, environment())
  .srv_part(.srv_ard_analysis, environment())
  .srv_part(.srv_ard_data, environment())
  .srv_part(.srv_toc, environment())
  .srv_part(.srv_ard_own, environment())
  .srv_part(.srv_double, environment())
  .srv_part(.srv_builder, environment())
  .srv_part(.srv_builder_rows, environment())
  .srv_part(.srv_col_header, environment())
  .srv_part(.srv_figure_page, environment())
  .srv_part(.srv_add_report, environment())
  .srv_part(.srv_first_table, environment())
  .srv_part(.srv_data_run, environment())
}

# app_server()'s parts (R/srv_*.R) run in its environment, one after
# another: each a function of R/srv_*.R whose enclosure is set to
# app_server()'s environment; what a part makes (its reactives, its
# functions) copied back there, so the parts after it see them as before
.srv_part <- function(part, env) {
  environment(part) <- env
  part()
}
.srv_keep <- function(env) {
  list2env(mget(ls(env, all.names = TRUE), env), parent.env(env))
  invisible()
}

# The study list's first column: the ID, a mark when its folder is not
# found, when it is the study open, and when its run is going on
.study_list_ids <- function(d, open_id = NULL, running = NULL, t = identity) {
  paste0(d$study_id, ifelse(d$folder, "", " \u26a0"),
         ifelse(d$study_id %in% open_id, " \u25cf", ""),
         ifelse(d$study_id %in% running, paste0(" (", t("running"), ")"), ""))
}
