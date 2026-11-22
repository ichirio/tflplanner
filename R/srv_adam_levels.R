# app_server(): [Data] -> ADaM levels (#742).  Run in app_server()'s
# environment (.srv_part()).  The file is chosen in the field (typed, by
# Browse..., or the ADaM folder's one define.xml); Apply reads it and puts
# its levels into the study (R/adam_levels.R), Clear the choice sets none.
# Both are edits of the study: on disk at the next save.
.srv_adam_levels <- function() {
  adam_dir <- function() if (has_study()) rv$study$path else NULL
  adam_field <- function() .adam_levels_clean(input$adam_levels_path)
  # another study opened: the field shows its applied file
  shiny::observeEvent(rv$study$path, {
    f <- .study_setting(rv$p, "adam_levels_file")
    shiny::updateTextInput(session, "adam_levels_path", value = if (is.na(f)) "" else f)
  })
  output$adam_levels_status <- shiny::renderUI({
    shiny::req(has_study())
    st <- .adam_levels_state(rv$p)
    shiny::p(class = paste("fw-semibold mb-1", if (st$source == "none") "text-muted" else "text-success"),
             .adam_levels_status_text(st, t))
  })
  output$adam_levels_candidate <- shiny::renderUI({
    shiny::req(has_study(), !nzchar(adam_field()))
    cand <- .adam_levels_candidate(adam_dir())
    shiny::req(!is.na(cand))
    shiny::div(class = "d-flex gap-2 align-items-center small mb-2",
               shiny::span(sprintf(t("Candidate: %s"), cand)),
               .btn("adam_levels_pick", t("Choose this"), class = "btn-sm btn-outline-secondary"))
  })
  output$adam_levels_pending <- shiny::renderUI({
    shiny::req(has_study(), .adam_levels_pending(rv$p, adam_field(), adam_dir()))
    shiny::div(class = "alert alert-warning py-1 px-2 small mb-2",
               t("The file chosen is not applied yet"))
  })
  output$adam_levels_apply_ui <- shiny::renderUI({
    b <- .btn("adam_levels_apply", t("Apply"), class = "btn-primary")
    if (!nzchar(adam_field())) b$attribs$disabled <- NA
    b
  })
  shiny::observeEvent(input$adam_levels_pick, {
    cand <- .adam_levels_candidate(adam_dir())
    if (!is.na(cand)) shiny::updateTextInput(session, "adam_levels_path", value = cand)
  })
  shiny::observeEvent(input$adam_levels_browse, {
    start <- .adam_levels_path(adam_field(), adam_dir())
    if (!nzchar(start) || !file.exists(start)) {
      start <- .ai_browse_start("adam", "", study_dir = adam_dir())
    }
    f <- guarded(.ai_choose_adam_spec(start))
    if (length(f) == 1L && !is.na(f)) {
      shiny::updateTextInput(session, "adam_levels_path", value = .adam_levels_rel(f, adam_dir()))
    }
  })
  shiny::observeEvent(input$adam_levels_apply, {
    if (!has_study()) return(notify(t("Open a study first"), "warning"))
    p2 <- guarded(.adam_levels_apply(rv$p, adam_field(), adam_dir(), t = t))
    if (is.null(p2)) return()
    rv$p <- p2
    shiny::updateTextInput(session, "adam_levels_path", value = .study_setting(p2, "adam_levels_file"))
    notify(sprintf(t("Applied: %s. Save to keep it."), .study_setting(p2, "adam_levels_file")))
  })
  shiny::observeEvent(input$adam_levels_clear, {
    if (!has_study()) return(notify(t("Open a study first"), "warning"))
    p2 <- guarded(.adam_levels_clear(rv$p))
    if (is.null(p2)) return()
    rv$p <- p2
    shiny::updateTextInput(session, "adam_levels_path", value = "")
    notify(t("The choice is cleared: no levels are applied. Save to keep it."))
  })
}
