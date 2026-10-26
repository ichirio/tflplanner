# The AI drafts tab's card for the look of past reports: an agent reads
# the RTF files of a folder (their numbers only -- column widths, borders,
# fonts, alignment: tflspec::tfl_rtf_style_scan()) and says the usual
# forms in a file tflspec names (its result, ai_style.yml); the app shows
# the prompt to give it and, when the file is there, its summary
# (tflspec::tfl_ai_style_read()).  Not a study's: the files are in
# <tflplanner home>/style.  Copying the result into the company standards
# is not here (yet).

# The folder of the survey's files (the home's, not a study's)
.ai_style_dir <- function() file.path(tflplanner_home(), "style")

# The agent's result: the path tflspec gave with the prompt, else the
# folder's YAML file (its name tflspec's, not said twice here)
.ai_style_result <- function(dir, files = NULL) {
  r <- if (!is.null(files) && "result" %in% names(files)) files[["result"]]
  if (length(r) && !is.na(r) && nzchar(r)) return(r)
  y <- if (dir.exists(dir)) list.files(dir, "[.]ya?ml$", full.names = TRUE) else character()
  if (length(y)) y[1L] else NA_character_
}

# Whether tflspec has the functions (tflspec #496)
.ai_style_ok <- function() {
  all(vapply(c("tfl_ai_style_prompt", "tfl_ai_style_read"), function(f)
    exists(f, envir = asNamespace("tflspec"), inherits = FALSE), NA))
}

# The card's content: on its own, so it can move to the tool's settings
# later (the AI tab shows it as a sub-tab for now: ai_style_panel())
#' @noRd
ai_style_ui <- function(ns, t) {
  shiny::tagList(
    shiny::p(class = "small text-muted", t("Not a study's: for every study. The agent reads the numbers of the RTF files (column widths, borders, fonts, alignment), never their text, and writes the usual forms; read here, they are not used anywhere yet.")),
    .ai_path_input(ns("style_rtf"), paste0(t("Folder of past RTF reports"), " (rtf_dir)"),
                   "C:/.../tfl", ns("style_browse"), t("Browse...")),
    shiny::div(class = "d-flex flex-wrap gap-2 align-items-center my-2",
               .btn(ns("style_prompt"), paste0(t("Write the prompt"), " (tfl_ai_style_prompt)"),
                    class = "btn-sm btn-primary"),
               .btn(ns("style_read"), paste0(t("Read the result"), " (tfl_ai_style_read)")),
               .btn(ns("style_open"), t("Open the folder"))),
    shiny::uiOutput(ns("style_out")))
}

#' @noRd
ai_style_panel <- function(ns, t) {
  bslib::nav_panel(paste0(t("Past reports' look"), " (tfl_ai_style_prompt)"), value = "style",
                   ai_style_ui(ns, t))
}

# its server part: the same module's input, output and session, wherever
# the card is drawn
#' @noRd
ai_style_server <- function(input, output, session, notify, guarded, t) {
  ns <- session$ns
  state <- shiny::reactiveValues(prompt = NULL, files = NULL, summary = NULL)
  shiny::observeEvent(input$style_browse, {
    d <- tryCatch(.ai_choose_dir(input$style_rtf %||% ""), error = function(e) NULL)
    if (length(d) && !is.na(d) && nzchar(d)) shiny::updateTextInput(session, "style_rtf", value = d)
  })
  shiny::observeEvent(input$style_prompt, {
    if (!.ai_style_ok()) return(notify(t("This tflspec cannot read past reports yet (tfl_ai_style_prompt)."), "warning"))
    rtf <- trimws(gsub("^\"|\"$", "", input$style_rtf %||% ""))
    if (!nzchar(rtf) || !dir.exists(rtf)) return(notify(t("Choose the folder of past RTF reports."), "warning"))
    out <- guarded(tflspec::tfl_ai_style_prompt(rtf, dir = .ai_style_dir()))
    if (is.null(out)) return()
    state$prompt <- paste(as.character(out), collapse = "\n")
    state$files <- unlist(attr(out, "files"))
    state$summary <- NULL
  })
  shiny::observeEvent(input$style_read, {
    if (!.ai_style_ok()) return(notify(t("This tflspec cannot read past reports yet (tfl_ai_style_prompt)."), "warning"))
    f <- .ai_style_result(.ai_style_dir(), state$files)
    if (is.na(f) || !file.exists(f)) {
      return(notify(sprintf(t("No result yet: the agent writes it in %s."), .ai_style_dir()), "warning"))
    }
    s <- guarded(tflspec::tfl_ai_style_read(f))
    if (is.null(s)) return()
    state$summary <- s
  })
  shiny::observeEvent(input$style_open, .open_folder(.ai_style_dir()))
  output$style_out <- shiny::renderUI({
    f <- state$files
    s <- state$summary
    prompt <- if (!is.null(state$prompt)) {
      shiny::tagList(
        if (length(f)) shiny::div(class = "small", t("Written:"), " ", shiny::code(paste(f, collapse = ", "))),
        shiny::div(class = "small mt-1", sprintf(t("Give the agent this prompt (it reads the numbers and writes %s):"),
                                                 basename(.ai_style_result(.ai_style_dir(), f)))),
        shiny::tags$textarea(class = "form-control small", rows = 8, readonly = NA, state$prompt))
    }
    sum_ui <- if (!is.null(s)) {
      txt <- attr(s, "text") %||% paste(utils::capture.output(print(s)), collapse = "\n")
      shiny::tagList(shiny::h6(class = "mt-2", t("The usual forms of the past reports")),
                     shiny::tags$pre(class = "small", paste(txt, collapse = "\n")))
    }
    if (is.null(prompt) && is.null(sum_ui)) {
      return(shiny::p(class = "small text-muted", sprintf(t("The files go to %s."), .ai_style_dir())))
    }
    shiny::tagList(prompt, sum_ui)
  })
}
