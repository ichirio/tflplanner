# app_server(): [Data] -> ADaM levels (#742).  Run in app_server()'s
# environment (.srv_part()).  The file is chosen in the field (typed, by
# Browse..., or the ADaM folder's one define.xml); Apply reads it and puts
# its levels into the study (R/adam_levels.R), Clear the choice sets none.
# Both are edits of the study: on disk at the next save.
.srv_adam_levels <- function() {
  adam_dir <- function() if (has_study()) rv$study$path else NULL
  adam_field <- function() .adam_levels_clean(input$adam_levels_path)
  set_field <- function(v) {
    shiny::updateTextInput(session, "adam_levels_path", value = if (length(v) && !is.na(v)) v else "")
  }
  # the field shows the applied file when another study is opened, or the
  # changes are discarded -- not at each save or write-back of the same
  # study (rv$study is set again then): a file chosen and not applied stays
  last <- new.env(parent = emptyenv())
  shiny::observeEvent(list(rv$study$path, rv$discard_ver), {
    now <- list(path = rv$study$path, discard = rv$discard_ver)
    if (identical(now, last$seen)) return()
    last$seen <- now
    set_field(.study_setting(rv$p, "adam_levels_file"))
  }, ignoreNULL = FALSE)
  # the status: what is applied (the file, when, how much), and right under
  # it whether the file chosen is not applied yet
  output$adam_levels_status <- shiny::renderUI({
    shiny::req(has_study())
    st <- .adam_levels_state(rv$p)
    txt <- .adam_levels_status_text(st, t, .adam_levels_counts(rv$p))
    pend <- .adam_levels_pending(rv$p, adam_field(), adam_dir())
    shiny::tagList(
      shiny::div(class = paste("fw-semibold", if (st$source == "none") "text-muted" else "text-success"),
                 txt[1L]),
      if (length(txt) > 1L) shiny::div(class = "small text-muted", txt[2L]),
      if (nzchar(pend)) {
        shiny::div(class = "alert alert-warning py-1 px-2 small mt-2 mb-0",
                   t(.adam_levels_pending_msg[[pend]]))
      })
  })
  # under the field: the candidate (nothing chosen), then Apply and Clear
  # the choice, each enabled when it has something to do
  output$adam_levels_actions <- shiny::renderUI({
    shiny::req(has_study())
    field <- adam_field()
    cand <- if (!nzchar(field)) .adam_levels_candidate(adam_dir()) else NA_character_
    apply_b <- .btn("adam_levels_apply", t("Apply"), class = "btn-primary")
    if (!nzchar(field)) apply_b$attribs$disabled <- NA
    clear_b <- .btn("adam_levels_clear", t("Clear the choice"), class = "btn-outline-secondary")
    if (!.adam_levels_clearable(rv$p, field)) clear_b$attribs$disabled <- NA
    shiny::tagList(
      if (!is.na(cand)) {
        shiny::div(class = "d-flex gap-2 align-items-center small mb-2",
                   shiny::span(sprintf(t("Candidate: %s"), cand)),
                   .btn("adam_levels_pick", t("Choose this"), class = "btn-sm btn-outline-secondary"))
      },
      shiny::div(class = "d-flex gap-2", apply_b, clear_b))
  })
  shiny::observeEvent(input$adam_levels_pick, {
    cand <- .adam_levels_candidate(adam_dir())
    if (!is.na(cand)) set_field(cand)
  })
  shiny::observeEvent(input$adam_levels_browse, {
    start <- .adam_levels_path(adam_field(), adam_dir())
    if (!nzchar(start) || !file.exists(start)) {
      start <- .ai_browse_start("adam", "", study_dir = adam_dir())
    }
    f <- guarded(.ai_choose_adam_spec(start, caption = t("ADaM levels: a define.xml or a Define Spec"),
                                      xls = TRUE))
    if (length(f) == 1L && !is.na(f)) set_field(.adam_levels_rel(f, adam_dir()))
  })
  shiny::observeEvent(input$adam_levels_apply, {
    if (!has_study()) return(notify(t("Open a study first"), "warning"))
    p2 <- guarded(.adam_levels_apply(rv$p, adam_field(), adam_dir(), t = t))
    if (is.null(p2)) return()
    rv$p <- p2
    set_field(.study_setting(p2, "adam_levels_file"))
    notify(sprintf(t("Applied: %s. Save to keep it."), .study_setting(p2, "adam_levels_file")))
  })
  shiny::observeEvent(input$adam_levels_clear, {
    if (!has_study()) return(notify(t("Open a study first"), "warning"))
    p2 <- guarded(.adam_levels_clear(rv$p, adam_dir()))
    if (is.null(p2)) return()
    rv$p <- p2
    set_field("")
    notify(t("The choice is cleared: no levels are applied. Save to keep it."))
  })
}
