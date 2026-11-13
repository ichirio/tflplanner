# app_server(): the four-steps screen (provisional, R/foursteps_ui.R) -- the
# report chosen from the open study's spec/outputs, drawn from its YAML.
# Run in app_server()'s environment (.srv_part()).
.srv_foursteps <- function() {
  if (!.fs_on()) return(invisible())
  fs_study <- shiny::reactive({
    # the study open in the Study tab
    root <- tryCatch(rv$study$path, error = function(e) NULL)
    if (is.null(root) || !length(root) || !nzchar(root) || !dir.exists(root)) return(NULL)
    root
  })
  shiny::observeEvent(fs_study(), {
    f <- .fs_reports(fs_study() %||% "")
    shiny::updateSelectInput(session, "fs_report", choices = names(f))
  }, ignoreNULL = FALSE)
  fs_model_r <- shiny::reactive({
    root <- fs_study()
    shiny::req(root, input$fs_report)
    f <- .fs_reports(root)[[input$fs_report]]
    shiny::req(f)
    tryCatch(.fs_model(f, .fs_study(root)), error = function(e) e)
  })
  output$fs_screen <- shiny::renderUI({
    if (is.null(fs_study())) return(shiny::div(class = "text-muted", t("Open a study in the Study tab")))
    m <- fs_model_r()
    if (inherits(m, "error")) {
      return(shiny::div(class = "alert alert-danger", conditionMessage(m)))
    }
    .fs_screen_ui(m, t)
  })
  # the Preview card is made only when Update is pressed, for the chosen step
  output$fs_preview_body <- shiny::renderUI({
    n <- input$fs_update %||% 0L
    if (n < 1L) return(shiny::div(class = "rp-fs-note", t("Press Update to make the preview.")))
    m <- shiny::isolate(fs_model_r())
    shiny::req(!inherits(m, "error"))
    .fs_preview_ui(m, shiny::isolate(input$fs_step) %||% "data", t)
  })
}
