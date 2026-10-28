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
  # (the AI's `value`, kept in the file by id, when tfl_ai_assumptions()
  # does not return it)
  if (!"value" %in% names(a)) a$value <- .ai_asm_file_values(p, a$id)
  a$value <- .ai_asm_values(p, a)
  k <- unique(a$kind)
  kinds <- data.frame(kind = k, verdict = own(k, "verdict"), correct = own(k, "correct"),
                      stringsAsFactors = FALSE)
  # (a kind's value: when each of its reports has the same one)
  kinds$value <- vapply(k, function(x) {
    v <- unique(a$value[a$kind == x])
    if (length(v) == 1L) v else NA_character_
  }, "", USE.NAMES = FALSE)
  structure(a, kinds = kinds)
}

# ---- what an assumption's report has, there --------------------------------
# The value the AI wrote (its `value`, when the assumptions have one), else
# the report's own: its YAML (the draft, ai/spec/outputs/<id>.yml, else the
# study's, spec/outputs/<id>.yml) followed by `where` (keys, "." or " "
# between) and `item` (a key, or the `id` of a record in a list); text a
# line a value, NA when not found
.ai_asm_values <- function(p, a) {
  given <- if ("value" %in% names(a)) {
    v <- a$value
    if (is.list(v)) vapply(v, function(x) .ai_asm_value_text(x), "") else .ai_asm_tidy(v)
  } else rep("", nrow(a))
  ys <- list()
  read_report <- function(id) {
    if (is.na(id) || !nzchar(id)) return(NULL)
    if (!id %in% names(ys)) {
      fs <- file.path(p, c(file.path("ai", "spec", "outputs"), file.path("spec", "outputs")),
                      paste0(id, ".yml"))
      f <- fs[file.exists(fs)][1]
      ys[id] <<- list(if (is.na(f)) NULL else tryCatch(yaml::read_yaml(f), error = function(e) NULL))
    }
    ys[[id]]
  }
  out <- vapply(seq_len(nrow(a)), function(i) {
    if (nzchar(given[i])) return(given[i])
    y <- read_report(a$output_id[i])
    x <- .ai_asm_follow(y, a$where[i], a$item[i])
    if (is.null(x)) "" else .ai_asm_value_text(x)
  }, "")
  ifelse(nzchar(out), out, NA_character_)
}

# The `value` of each id in spec/_assumptions.yml, as text ("" when none)
.ai_asm_file_values <- function(p, ids) {
  y <- tryCatch(yaml::read_yaml(file.path(p, "spec", "_assumptions.yml")), error = function(e) NULL)
  if (!is.list(y)) return(rep("", length(ids)))
  have <- vapply(y, function(r) if (is.list(r) && !is.null(r$id)) as.character(r$id)[1] else "", "")
  vals <- vapply(y, function(r) if (is.list(r) && !is.null(r$value)) .ai_asm_value_text(r$value) else "", "")
  out <- vals[match(ids, have)]
  out[is.na(out)] <- ""
  out
}

.ai_asm_follow <- function(y, where, item) {
  if (is.null(y)) return(NULL)
  key <- function(node, k) {
    if (!is.list(node) || !nzchar(k)) return(NULL)
    nm <- names(node)
    if (!is.null(nm)) {
      at <- match(tolower(k), tolower(nm))
      if (!is.na(at)) return(node[[at]])
    }
    # a list of records: the one of that id
    ids <- vapply(node, function(r) if (is.list(r) && !is.null(r$id)) as.character(r$id)[1] else "", "")
    at <- match(tolower(k), tolower(ids))
    if (is.na(at)) NULL else node[[at]]
  }
  node <- y
  for (k in strsplit(.ai_asm_tidy(where), "[. ]+")[[1]]) {
    node <- key(node, k)
    if (is.null(node)) return(NULL)
  }
  key(node, .ai_asm_tidy(item))
}

# A value as at most 3 lines: a vector "a / b", a record "k: v, k: v", a
# list of them a line each, a mapping "k: v" a line a leaf
.ai_asm_value_text <- function(x, lines = 3L, width = 120L) {
  one <- function(v) {
    if (is.null(v)) return("")
    if (!is.list(v)) return(paste(as.character(v), collapse = " / "))
    nm <- names(v)
    if (is.null(nm)) return(paste(vapply(v, one, ""), collapse = "; "))
    paste(paste0(nm, ": ", vapply(v, one, "")), collapse = ", ")
  }
  out <- if (!is.list(x)) one(x)
         else if (is.null(names(x))) vapply(x, one, "")
         else paste0(names(x), ": ", vapply(x, one, ""))
  out <- out[nzchar(out)]
  if (!length(out)) return("")
  more <- length(out) > lines
  out <- utils::head(out, lines)
  long <- nchar(out) > width
  out[long] <- paste0(substr(out[long], 1L, width - 1L), "\u2026")
  paste(c(out, if (more) "\u2026"), collapse = "\n")
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
.ai-asm-value { white-space: pre-wrap; font-family: var(--bs-font-monospace); font-size: .8rem; vertical-align: top; display: inline-block; }
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
document.addEventListener('toggle', function(e) {
  var d = e.target;
  if (!d.matches || !d.matches('details.ai-asm-each') || !d.open) return;
  Shiny.setInputValue(d.dataset.ns + 'asm_open', {kind: d.dataset.kind, n: Date.now()}, {priority: 'event'});
}, true);
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
  # the kinds whose reports' rows are drawn (opened once: drawn again with
  # the rest; a field's value is read only while it is drawn)
  opened <- character()
  drawn_kinds <- character()
  typed_changes <- function(a) {
    if (is.null(a)) return(NULL)
    kinds <- attr(a, "kinds")
    ids <- c(kinds$kind, a$id)
    typed <- function(what) vapply(ids, function(id) input[[.ai_asm_input(what, id)]] %||% NA_character_, "")
    v <- typed("v")
    drawn <- !is.na(v) & c(rep(TRUE, nrow(kinds)), a$kind %in% drawn_kinds)
    .ai_asm_changes(ids[drawn], v[drawn], typed("c")[drawn],
                    c(kinds$verdict, a$own_verdict)[drawn], c(kinds$correct, a$own_correct)[drawn])
  }
  # (drawn again at once: after a save, or another study opened)
  force <- TRUE
  last_path <- NULL
  shiny::observeEvent(asm(), {
    p <- path()
    if (!identical(p, last_path)) opened <<- character()
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
  value <- function(v) {
    if (is.na(v)) return(NULL)
    shiny::div(class = "text-muted small ms-1", shiny::tags$b(t("Now:")), " ",
               shiny::span(class = "ai-asm-value", v, .noWS = c("after-begin", "before-end")))
  }
  # a kind's reports' rows
  report_rows <- function(a, k) {
    kinds <- attr(a, "kinds")
    i <- match(k, kinds$kind)
    r <- a[a$kind == k, , drop = FALSE]
    from <- if (is.na(kinds$verdict[i])) NULL
            else list(kind = k, verdict = kinds$verdict[i], correct = kinds$correct[i])
    shiny::tagList(lapply(seq_len(nrow(r)), function(j) shiny::div(
      class = "ms-3 py-1 border-top",
      shiny::div(class = "d-flex flex-wrap gap-2 align-items-baseline",
                 shiny::tags$b(r$id[j]), r$output_id[j],
                 shiny::span(class = "text-muted", r$where[j]),
                 said(r$item[j], r$assumption[j]),
                 shiny::span(class = paste("badge", badge[[r$status[j]]] %||% "text-bg-light"),
                             status_word(r$status[j]))),
      if (is.na(kinds$value[i])) value(r$value[j]),
      answer(r$id[j], r$own_verdict[j], r$own_correct[j], from = from))))
  }
  output$asm <- shiny::renderUI({
    a <- shown()
    if (is.null(a)) {
      return(shiny::p(class = "small text-muted mb-0",
                      t("No assumptions: the agent writes spec/_assumptions.yml when it assumes something.")))
    }
    kinds <- attr(a, "kinds")
    # (the kinds opened before: their reports drawn at once, still open)
    drawn_kinds <<- intersect(opened, kinds$kind)
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
      ids <- r$output_id
      each <- shiny::tags$details(
        class = "ai-asm-each small mt-1", `data-ns` = ns(""), `data-kind` = k,
        open = if (k %in% drawn_kinds) NA,
        shiny::tags$summary(sprintf(t("Each report (%s)"),
                                    if (length(ids) > 3L) sprintf(t("%d reports"), length(ids))
                                    else paste(ids, collapse = ", "))),
        shiny::div(id = ns(paste0("asm_rows_", k)), if (k %in% drawn_kinds) report_rows(a, k)))
      shiny::div(class = "ai-assumption border-bottom py-2", `data-open` = if (n_open) "1" else "0",
                 head, value(kinds$value[i]), answer(k, kinds$verdict[i], kinds$correct[i]), each)
    })
    shiny::tagList(shiny::div(class = "ai-asm-list", rows),
                   shiny::p(class = "ai-asm-all-done small text-muted mb-0 mt-2",
                            t("Every assumption is answered.")))
  })
  # a kind's reports opened the first time: their rows drawn into it
  shiny::observeEvent(input$asm_open, {
    k <- input$asm_open$kind
    a <- shown()
    if (is.null(a) || !k %in% a$kind || k %in% drawn_kinds) return()
    opened <<- union(opened, k)
    drawn_kinds <<- union(drawn_kinds, k)
    shiny::insertUI(paste0("#", ns(paste0("asm_rows_", k))), "beforeEnd", report_rows(a, k), immediate = TRUE)
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

# ---- a report adopted from the AI, in the Report list (R/srv_figure_page.R) --

# Each of `ids`: "" (not the AI's), "adopted" (adopted from its draft,
# ai/adopted.csv), "newer" (its draft changed since: adopting again would
# change it)
.ai_adopted_marks <- function(p, ids) {
  out <- stats::setNames(rep("", length(ids)), ids)
  f <- if (!is.null(p)) file.path(p, "ai", "adopted.csv")
  if (is.null(f) || !file.exists(f)) return(out)
  a <- tryCatch(utils::read.csv(f, colClasses = "character", stringsAsFactors = FALSE,
                                fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
  if (is.null(a) || !all(c("output_id", "md5") %in% names(a))) return(out)
  a <- a[!duplicated(a$output_id, fromLast = TRUE), , drop = FALSE]
  k <- ids[ids %in% a$output_id]
  out[k] <- "adopted"
  drafts <- file.path(p, "ai", "spec", "outputs", paste0(k, ".yml"))
  have <- file.exists(drafts)
  now <- unname(tools::md5sum(drafts[have]))
  out[k[have][now != a$md5[match(k[have], a$output_id)]]] <- "newer"
  out
}

.ai_mark_html <- function(marks, t) {
  marks <- unname(marks)
  ifelse(marks == "", "", ifelse(
    marks == "newer",
    sprintf(' <span class="badge text-bg-warning" title="%s">AI</span>',
            htmltools::htmlEscape(t("Adopted from the AI's draft; the draft changed since (step 4)"), TRUE)),
    sprintf(' <span class="badge text-bg-light border" title="%s">AI</span>',
            htmltools::htmlEscape(t("Adopted from the AI's draft"), TRUE))))
}
