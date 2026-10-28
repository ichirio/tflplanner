# ============================================================================
#  Step 4's card of the AI's assumptions (spec/_assumptions.yml): each kind
#  (K<n>, the assumptions that ask the same) answered for every report of
#  it, or one report's assumption (A<n>) on its own -- `ok`, or `wrong` and
#  the right content.  Saved to ai/assumption_answers.csv by
#  tflspec::tfl_ai_assumption_answers_write(); "Write the corrections"
#  (tfl_ai_corrections) then hands the wrong ones to the agent.
#
#  Every row is drawn once (the filter only hides rows, in the page), and
#  the rows are drawn again only when nothing typed is unsaved: what the
#  person types is not lost to a redraw.
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

.ai_asm_tidy <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  x
}

# The answers typed that differ from the ones saved: one row an id (a
# row with neither verdict nor correct removes that id's answer)
.ai_asm_changes <- function(ids, verdict, correct, old_verdict, old_correct) {
  verdict <- .ai_asm_tidy(verdict)
  correct <- .ai_asm_tidy(correct)
  # (the right content is kept only for `wrong`; typed with no verdict, `wrong`)
  verdict[!nzchar(verdict) & nzchar(correct)] <- "wrong"
  correct[verdict == "ok"] <- ""
  ch <- verdict != .ai_asm_tidy(old_verdict) | correct != .ai_asm_tidy(old_correct)
  na <- function(x) ifelse(nzchar(x), x, NA_character_)
  data.frame(id = ids[ch], verdict = na(verdict[ch]), correct = na(correct[ch]),
             stringsAsFactors = FALSE)
}

# The ids answered `wrong` with no right content (tfl_ai_assumption_answers_write
# refuses them)
.ai_asm_missing_correct <- function(ch) ch$id[ch$verdict %in% "wrong" & is.na(ch$correct)]

# A kind's content: its item and assumption when its reports say the same,
# else NA (they differ)
.ai_asm_kind_said <- function(r) {
  said <- unique(paste0(.ai_asm_tidy(r$item), " : ", .ai_asm_tidy(r$assumption)))
  if (length(said) == 1L) said else NA_character_
}

.ai_asm_css <- "
.ai-asm-only-open .ai-assumption[data-open='0'] { display: none; }
.ai-asm-all-done { display: none; }
.ai-asm-only-open .ai-asm-list:not(:has(.ai-assumption[data-open='1'])) + .ai-asm-all-done { display: block; }
"

.ai_asm_js <- "
$(document).off('change.aiasm').on('change.aiasm', '.ai-asm-show input', function() {
  $(this).closest('.ai-asm-card').toggleClass('ai-asm-only-open', this.value === 'open');
});
$(document).off('input.aiasm').on('input.aiasm', '.ai-asm-correct input', function() {
  $(this).removeClass('is-invalid');
  var s = $(this).closest('.ai-asm-answer').find('select');
  if (this.value.trim() !== '' && s.val() === '') { s.val('wrong').trigger('change'); }
});
$(document).off('change.aiasmv').on('change.aiasmv', '.ai-asm-answer select', function() {
  $(this).closest('.ai-asm-answer').find('.ai-asm-correct input').removeClass('is-invalid');
});
Shiny.addCustomMessageHandler('ai-asm-invalid', function(x) {
  [].concat(x.ids || []).forEach(function(id) { $('#' + x.ns + id).addClass('is-invalid'); });
});
"

#' @noRd
ai_asm_card <- function(ns, t) {
  save_btn <- function(id) {
    .btn(ns(id), paste0(t("Save the answers"), " (tfl_ai_assumption_answers_write)"),
         class = "btn-sm btn-primary")
  }
  bslib::card(
    class = "mt-3 ai-asm-card ai-asm-only-open",
    shiny::tags$style(shiny::HTML(.ai_asm_css)),
    shiny::tags$script(shiny::HTML(.ai_asm_js)),
    bslib::card_header(
      class = "d-flex flex-wrap gap-2 align-items-center",
      shiny::span(class = "me-auto", with_tip(
        paste0(t("The AI's assumptions"), " (spec/_assumptions.yml)"),
        t("Answer a kind for every report of it, or open it to answer one report's assumption on its own (that answer wins). Wrong ones go to the agent with the corrections below."))),
      shiny::div(class = "ai-asm-show",
                 shiny::radioButtons(ns("asm_show"), NULL, inline = TRUE,
                                     choices = stats::setNames(c("open", "all"),
                                                               c(t("Not answered"), t("All"))))),
      save_btn("asm_save")),
    shiny::uiOutput(ns("asm_stale")),
    shiny::uiOutput(ns("asm")),
    shiny::div(class = "d-flex flex-wrap gap-2 align-items-center mt-2",
               save_btn("asm_save2"),
               shiny::span(class = "small text-muted",
                           t("Then write the corrections in \"The next round\" below."))))
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
  # what is drawn: the assumptions read, unless the person has typed
  # answers not saved yet (then a note, to read them again)
  shown <- shiny::reactiveVal(NULL)
  stale <- shiny::reactiveVal(FALSE)
  typed_changes <- function(a) {
    if (is.null(a)) return(NULL)
    kinds <- attr(a, "kinds")
    ids <- c(kinds$kind, a$id)
    typed <- function(what) vapply(ids, function(id) input[[.ai_asm_input(what, id)]] %||% NA_character_, "")
    v <- typed("v")
    drawn <- !is.na(v)
    .ai_asm_changes(ids[drawn], v[drawn], typed("c")[drawn],
                    c(kinds$verdict, a$own_verdict)[drawn], c(kinds$correct, a$own_correct)[drawn])
  }
  # (drawn again at once: after a save, or another study opened)
  force <- TRUE
  last_path <- NULL
  shiny::observeEvent(asm(), {
    p <- path()
    ch <- if (force || !identical(p, last_path)) NULL else shiny::isolate(typed_changes(shown()))
    force <<- FALSE
    last_path <<- p
    if (is.null(ch) || !nrow(ch)) {
      shown(asm())
      stale(FALSE)
    } else stale(TRUE)
  }, ignoreNULL = FALSE)
  shiny::observeEvent(input$asm_reread, {
    shown(asm())
    stale(FALSE)
  })
  output$asm_stale <- shiny::renderUI({
    if (!isTRUE(stale())) return(NULL)
    shiny::div(class = "alert alert-warning py-2 small d-flex flex-wrap gap-2 align-items-center",
               t("The assumptions changed (the answers checked again), and your answers here are not saved."),
               .btn(ns("asm_reread"), t("Read them again (drop what I typed)")))
  })
  status_word <- function(s) {
    v <- t(paste0("assumption-status:", s))
    if (startsWith(v, "assumption-status:")) s else v
  }
  badge <- c(open = "text-bg-warning", answered = "text-bg-info", confirmed = "text-bg-success",
             done = "text-bg-secondary")
  verdict_word <- c(ok = t("Right (ok)"), wrong = t("Wrong (wrong)"))
  # an answer's fields; `from` the answer a report's assumption has from its
  # kind (shown in its empty choice and field)
  answer <- function(id, verdict, correct, from = NULL) {
    none <- if (is.null(from)) t("(no answer)")
            else sprintf(t("(%s's answer: %s)"), from$kind, verdict_word[[from$verdict]])
    hint <- if (!is.null(from) && !is.na(from$correct)) paste0(from$kind, ": ", from$correct)
            else t("When wrong: the right content (correct)")
    shiny::div(
      class = "ai-asm-answer d-flex flex-wrap gap-2 align-items-center",
      shiny::tagAppendAttributes(
        shiny::selectInput(ns(.ai_asm_input("v", id)), NULL, width = "13rem", selectize = FALSE,
                           choices = stats::setNames(c("", "ok", "wrong"),
                                                     c(none, verdict_word[["ok"]], verdict_word[["wrong"]])),
                           selected = if (is.na(verdict)) "" else verdict),
        title = t("Back to no answer: the answer saved is removed.")),
      shiny::div(class = "ai-asm-correct flex-grow-1",
                 shiny::textInput(ns(.ai_asm_input("c", id)), NULL, width = "100%",
                                  value = if (is.na(correct)) "" else correct, placeholder = hint)))
  }
  said <- function(item, assumption) {
    shiny::span(shiny::tags$b(item), " : ", assumption)
  }
  output$asm <- shiny::renderUI({
    a <- shown()
    if (is.null(a)) {
      return(shiny::p(class = "small text-muted mb-0",
                      t("No assumptions: the agent writes spec/_assumptions.yml when it assumes something.")))
    }
    kinds <- attr(a, "kinds")
    rows <- lapply(seq_len(nrow(kinds)), function(i) {
      k <- kinds$kind[i]
      r <- a[a$kind == k, , drop = FALSE]
      n_open <- sum(r$status == "open")
      content <- .ai_asm_kind_said(r)
      q <- r$question[1]
      head <- shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-baseline",
        shiny::tags$b(k),
        shiny::span(class = "small text-muted", sprintf(t("%d reports"), r$n_reports[1])),
        if (n_open) shiny::span(class = "badge text-bg-warning", sprintf(t("%d not answered"), n_open))
        else shiny::span(class = paste("badge", badge[[r$status[1]]] %||% "text-bg-light"),
                         status_word(r$status[1])),
        if (is.na(content)) shiny::span(shiny::tags$b(r$item[1]), " ",
                                        shiny::span(class = "text-muted", t("(differs by report)")))
        else said(r$item[1], r$assumption[1]),
        if (!is.na(q) && nzchar(q)) shiny::span(class = "small text-muted", q))
      from <- if (is.na(kinds$verdict[i])) NULL
              else list(kind = k, verdict = kinds$verdict[i], correct = kinds$correct[i])
      ids <- r$output_id
      each <- shiny::tags$details(
        class = "small mt-1",
        shiny::tags$summary(sprintf(t("Each report (%s)"),
                                    if (length(ids) > 3L) sprintf(t("%d reports"), length(ids))
                                    else paste(ids, collapse = ", "))),
        lapply(seq_len(nrow(r)), function(j) shiny::div(
          class = "ms-3 py-1 border-top",
          shiny::div(class = "d-flex flex-wrap gap-2 align-items-baseline",
                     shiny::tags$b(r$id[j]), r$output_id[j],
                     shiny::span(class = "text-muted", r$where[j]),
                     said(r$item[j], r$assumption[j]),
                     shiny::span(class = paste("badge", badge[[r$status[j]]] %||% "text-bg-light"),
                                 status_word(r$status[j]))),
          answer(r$id[j], r$own_verdict[j], r$own_correct[j], from = from))))
      shiny::div(class = "ai-assumption border-bottom py-2", `data-open` = if (n_open) "1" else "0",
                 head, answer(k, kinds$verdict[i], kinds$correct[i]), each)
    })
    shiny::tagList(shiny::div(class = "ai-asm-list", rows),
                   shiny::p(class = "ai-asm-all-done small text-muted mb-0 mt-2",
                            t("Every assumption is answered.")))
  })
  save <- function() {
    a <- shown()
    shiny::req(a)
    ch <- typed_changes(a)
    if (!nrow(ch)) return(notify(t("No answer changed."), "warning"))
    bad <- .ai_asm_missing_correct(ch)
    if (length(bad)) {
      session$sendCustomMessage("ai-asm-invalid", list(ns = ns(""), ids = .ai_asm_input("c", bad)))
      return(notify(sprintf(t("%s: when wrong, write the right content."), paste(bad, collapse = ", ")),
                    "warning"))
    }
    f <- guarded(tflspec::tfl_ai_assumption_answers_write(path(), ch))
    if (is.null(f)) return()
    # (saved: the rows drawn again from the file)
    force <<- TRUE
    saved(saved() + 1L)
    notify(sprintf(t("%d answers saved to ai/assumption_answers.csv: write the corrections for the agent."),
                   nrow(ch)))
  }
  shiny::observeEvent(input$asm_save, save())
  shiny::observeEvent(input$asm_save2, save())
  saved
}
