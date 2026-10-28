# ============================================================================
#  Step 4's card of the AI's assumptions (spec/_assumptions.yml): each kind
#  (K<n>, the assumptions that ask the same) answered for every report of
#  it, or one report's assumption (A<n>) on its own -- `ok`, or `wrong` and
#  the right content.  Saved to ai/assumption_answers.csv by
#  tflspec::tfl_ai_assumption_answers_write(); "Write the corrections"
#  (tfl_ai_corrections) then hands the wrong ones to the agent.
# ============================================================================

# The assumptions (tfl_ai_assumptions) with each id's own answer (an A<n>'s
# or a K<n>'s, from ai/assumption_answers.csv); NULL when the study has none
.ai_asm_now <- function(p) {
  if (is.null(p) || !file.exists(file.path(p, "spec", "_assumptions.yml"))) return(NULL)
  a <- tryCatch(tflspec::tfl_ai_assumptions(p), error = function(e) {
    message("tflplanner: spec/_assumptions.yml: ", conditionMessage(e))
    NULL
  })
  if (is.null(a) || !nrow(a)) return(NULL)
  ans <- tryCatch(tflspec::tfl_ai_assumption_answers_read(p), error = function(e) NULL)
  if (is.null(ans)) ans <- data.frame(id = character(), verdict = character(), correct = character())
  own <- function(ids, col) as.character(ans[[col]][match(ids, ans$id)])
  a$own_verdict <- own(a$id, "verdict")
  a$own_correct <- own(a$id, "correct")
  k <- unique(a$kind)
  kinds <- data.frame(kind = k, verdict = own(k, "verdict"), correct = own(k, "correct"),
                      stringsAsFactors = FALSE)
  structure(a, kinds = kinds)
}

# A field's input id (the ids are A<n> / K<n>)
.ai_asm_input <- function(what, id) paste0("asm_", what, "_", id)

# The answers typed that differ from the ones saved: one row an id (a
# row with neither verdict nor correct removes that id's answer)
.ai_asm_changes <- function(ids, verdict, correct, old_verdict, old_correct) {
  tidy <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x)] <- ""
    x
  }
  verdict <- tidy(verdict)
  correct <- tidy(correct)
  # (the right content is kept only for `wrong`; typed with no verdict, `wrong`)
  verdict[!nzchar(verdict) & nzchar(correct)] <- "wrong"
  correct[verdict == "ok"] <- ""
  ch <- verdict != tidy(old_verdict) | correct != tidy(old_correct)
  na <- function(x) ifelse(nzchar(x), x, NA_character_)
  data.frame(id = ids[ch], verdict = na(verdict[ch]), correct = na(correct[ch]),
             stringsAsFactors = FALSE)
}

#' @noRd
ai_asm_card <- function(ns, t) {
  bslib::card(
    class = "mt-3",
    bslib::card_header(
      class = "d-flex flex-wrap gap-2 align-items-center",
      shiny::span(class = "me-auto", with_tip(
        paste0(t("The AI's assumptions"), " (spec/_assumptions.yml)"),
        t("Answer a kind for every report of it, or open it to answer one report's assumption on its own (that answer wins). Wrong ones go to the agent with the corrections below."))),
      shiny::radioButtons(ns("asm_show"), NULL, inline = TRUE,
                          choices = stats::setNames(c("open", "all"), c(t("Not answered"), t("All")))),
      .btn(ns("asm_save"), paste0(t("Save the answers"), " (tfl_ai_assumption_answers_write)"),
           class = "btn-sm btn-primary")),
    shiny::uiOutput(ns("asm")))
}

# The server part; its value bumped each time answers are saved (they may
# answer questions of ai/questions.md too)
#' @noRd
ai_asm_server <- function(input, output, session, path, ver, notify, guarded, t) {
  ns <- session$ns
  saved <- shiny::reactiveVal(0L)
  asm <- shiny::reactive({
    ver()
    saved()
    .ai_asm_now(path())
  })
  status_word <- function(s) {
    v <- t(paste0("assumption-status:", s))
    if (startsWith(v, "assumption-status:")) s else v
  }
  badge <- c(open = "text-bg-warning", answered = "text-bg-info", confirmed = "text-bg-success",
             done = "text-bg-secondary")
  answer <- function(id, verdict, correct) {
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      shiny::selectInput(ns(.ai_asm_input("v", id)), NULL, width = "9rem",
                         choices = stats::setNames(c("", "ok", "wrong"),
                                                   c(t("(no answer)"), t("Right (ok)"), t("Wrong (wrong)"))),
                         selected = if (is.na(verdict)) "" else verdict),
      shiny::div(class = "flex-grow-1",
                 shiny::textInput(ns(.ai_asm_input("c", id)), NULL, width = "100%",
                                  value = if (is.na(correct)) "" else correct,
                                  placeholder = t("When wrong: the right content (correct)"))))
  }
  said <- function(item, assumption) {
    shiny::span(shiny::tags$b(item), ": ", assumption)
  }
  output$asm <- shiny::renderUI({
    a <- asm()
    if (is.null(a)) {
      return(shiny::p(class = "small text-muted mb-0",
                      t("No assumptions: the agent writes spec/_assumptions.yml when it assumes something.")))
    }
    kinds <- attr(a, "kinds")
    show <- input$asm_show %||% "open"
    keep <- vapply(kinds$kind, function(k) show == "all" || any(a$status[a$kind == k] == "open"), TRUE)
    if (!any(keep)) return(shiny::p(class = "small text-muted mb-0", t("Every assumption is answered.")))
    shiny::tagList(lapply(which(keep), function(i) {
      k <- kinds$kind[i]
      r <- a[a$kind == k, , drop = FALSE]
      n_open <- sum(r$status == "open")
      head <- shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-baseline",
        shiny::tags$b(k),
        shiny::span(class = "small text-muted", sprintf(t("%d reports"), r$n_reports[1])),
        if (n_open) shiny::span(class = "badge text-bg-warning", sprintf(t("%d not answered"), n_open))
        else shiny::span(class = paste("badge", badge[[r$status[1]]] %||% "text-bg-light"),
                         status_word(r$status[1])),
        if (!is.na(r$question[1]) && nzchar(r$question[1])) shiny::span(r$question[1])
        else said(r$item[1], r$assumption[1]))
      each <- shiny::tags$details(
        class = "small mt-1",
        shiny::tags$summary(sprintf(t("Each report (%s)"), paste(r$output_id, collapse = ", "))),
        lapply(seq_len(nrow(r)), function(j) shiny::div(
          class = "ms-3 py-1 border-top",
          shiny::div(class = "d-flex flex-wrap gap-2 align-items-baseline",
                     shiny::tags$b(r$id[j]), r$output_id[j],
                     shiny::span(class = "text-muted", r$where[j]),
                     said(r$item[j], r$assumption[j]),
                     shiny::span(class = paste("badge", badge[[r$status[j]]] %||% "text-bg-light"),
                                 status_word(r$status[j]))),
          answer(r$id[j], r$own_verdict[j], r$own_correct[j]))))
      shiny::div(class = "ai-assumption border-bottom py-2", head,
                 answer(k, kinds$verdict[i], kinds$correct[i]), each)
    }))
  })
  shiny::observeEvent(input$asm_save, {
    a <- asm()
    shiny::req(a)
    kinds <- attr(a, "kinds")
    ids <- c(kinds$kind, a$id)
    typed <- function(what) vapply(ids, function(id) input[[.ai_asm_input(what, id)]] %||% NA_character_, "")
    v <- typed("v")
    c_ <- typed("c")
    drawn <- !is.na(v)
    ch <- .ai_asm_changes(ids[drawn], v[drawn], c_[drawn],
                          c(kinds$verdict, a$own_verdict)[drawn], c(kinds$correct, a$own_correct)[drawn])
    if (!nrow(ch)) return(notify(t("No answer changed."), "warning"))
    f <- guarded(tflspec::tfl_ai_assumption_answers_write(path(), ch))
    if (is.null(f)) return()
    saved(saved() + 1L)
    notify(sprintf(t("%d answers saved to ai/assumption_answers.csv: write the corrections for the agent."),
                   nrow(ch)))
  })
  saved
}
