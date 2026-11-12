# app_server(): the four-steps screen (provisional, R/foursteps_ui.R) -- the
# report chosen from the study folder's spec/outputs, drawn from its YAML.
# Run in app_server()'s environment (.srv_part()).
.srv_foursteps <- function() {
  if (!.fs_on()) return(invisible())
  fs_study <- shiny::reactive({
    root <- trimws(input$fs_study %||% "")
    if (!nzchar(root) || !dir.exists(root)) return(NULL)
    root
  })
  shiny::observeEvent(fs_study(), {
    f <- .fs_reports(fs_study() %||% "")
    shiny::updateSelectInput(session, "fs_report", choices = names(f))
  }, ignoreNULL = FALSE)
  output$fs_screen <- shiny::renderUI({
    root <- fs_study()
    shiny::req(root, input$fs_report)
    f <- .fs_reports(root)[[input$fs_report]]
    shiny::req(f)
    m <- tryCatch(.fs_model(f, .fs_study(root)), error = function(e) e)
    if (inherits(m, "error")) {
      return(shiny::div(class = "alert alert-danger", conditionMessage(m)))
    }
    .fs_screen_ui(m, t)
  })
}
