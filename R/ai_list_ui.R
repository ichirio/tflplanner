# The AI drafts tab's step 4, the list (tflspec::tfl_ai_outputs()): every
# report the AI was asked for, its draft (ai/spec/outputs) and whether it
# is adopted into the study (spec/outputs).  A row's RTF opens from here;
# the reports ticked -- at first the drafts new to the study or newer than
# its report -- are adopted (copied, tflspec::tfl_ai_adopt()).  One
# confirmation lists them: the ones the study has already are overwritten
# only by their own button, beside the warning of what may be lost, with
# what changes (tfl_ai_adopted_diff()).  The study is read again, so the
# adopted reports are on the Report list.
# Design: _archive/ai-list-corrections-tab-design-2026-10-26.md.

# The list as the table shows it (NULL: no study, or tflspec cannot list)
.ai_list <- function(dir) {
  if (is.null(dir) || !dir.exists(dir)) return(NULL)
  d <- tryCatch(suppressWarnings(tflspec::tfl_ai_outputs(dir)), error = function(e) NULL)
  if (!is.data.frame(d)) return(NULL)
  for (k in c("output_id", "title", "population", "split", "status", "adopted", "rtf", "problem",
              "questions", "file", "toc_id", "request")) {
    if (is.null(d[[k]])) d[[k]] <- rep(NA_character_, nrow(d))
  }
  d
}

# What adopting the rows would do, or NULL when it cannot say: output_id,
# status (new / replace / same / no draft)
.ai_adopt_plan <- function(dir, ids) {
  p <- tryCatch(tflspec::tfl_ai_adopt(dir, outputs = ids, dry_run = TRUE), error = function(e) NULL)
  if (!is.data.frame(p)) return(NULL)
  p
}

# The differences of the reports adopting replaces
# (tflspec::tfl_ai_adopted_diff()), or NULL
.ai_adopt_diff <- function(dir, ids) {
  if (!length(ids)) return(NULL)
  d <- tryCatch(do.call(rbind, lapply(ids, function(id) tflspec::tfl_ai_adopted_diff(dir, id))),
                error = function(e) NULL)
  if (is.data.frame(d) && nrow(d)) d else NULL
}

#' @noRd
ai_list_panel <- function(ns, t) {
  bslib::nav_panel(
    paste0("4 ", t("Copy into the study"), " (tfl_ai_adopt)"), value = "list",
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mb-2",
      shiny::h6(class = "mb-0 me-auto", with_tip(
        paste0(t("The AI's reports"), " (tfl_ai_outputs)"),
        t("Every report the AI was asked for: its draft (ai/spec/outputs) and whether it is in the study (spec/outputs). Adopt (copy) puts the reports ticked on the study's Report list. At first only the drafts not adopted yet are ticked."))),
      shiny::uiOutput(ns("list_count"), inline = TRUE),
      .btn(ns("list_reread"), t("Read again"))),
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mb-2",
      .btn(ns("list_all"), t("Tick all")),
      .btn(ns("list_none"), t("Untick all")),
      .btn(ns("list_new"), t("Tick only those not in the study yet")),
      .btn(ns("list_newer"), t("Tick only the updated drafts")),
      .btn(ns("list_changed"), t("Tick only the changed ones")),
      shiny::uiOutput(ns("list_ticked"), inline = TRUE),
      .btn(ns("adopt"), paste0(t("Adopt (copy) the reports ticked into the study"), " (tfl_ai_adopt)"),
           class = "btn-sm btn-primary ms-auto")),
    shiny::uiOutput(ns("list_empty")),
    DT::DTOutput(ns("list_table")),
    shiny::tags$script(shiny::HTML(
      "$(document).off('click.airtf').on('click.airtf', '.ai-open-rtf', function(e) {
         e.preventDefault();
         Shiny.setInputValue(this.dataset.ns + 'open_rtf', {file: this.dataset.file, n: Date.now()},
                             {priority: 'event'});
       });
       $(document).off('change.aick').on('change.aick', '.ai-adopt-ck', function() {
         Shiny.setInputValue(this.dataset.ns + 'ck_toggle', {id: this.dataset.id, on: this.checked, n: Date.now()},
                             {priority: 'event'});
       });")))
}

# The rows a draft can be adopted from (its file, a state not an error)
# and, of them, the ones the list ticks at first: not adopted yet
.ai_adoptable <- function(d) {
  !is.na(d$file) & nzchar(d$file) & !(as.character(d$status) %in% c("error", "no answer"))
}
# A report's copy state: "new" (not in the study), "replace" (the draft is
# newer), "same", or NA (no draft to copy).  `plan` is .ai_adopt_plan()'s
# data frame; NULL (tflspec cannot say): `adopted` of the list says it.
.ai_copy_state <- function(d, plan) {
  ok <- .ai_adoptable(d)
  if (is.data.frame(plan) && all(c("output_id", "status") %in% names(plan))) {
    st <- as.character(plan$status)[match(d$output_id, plan$output_id)]
    st[!st %in% c("new", "replace", "same")] <- NA_character_
  } else {
    a <- as.character(d$adopted)
    st <- ifelse(is.na(a) | a == "no", "new", ifelse(a == "yes, draft newer", "replace",
                                                     ifelse(a == "yes", "same", NA_character_)))
  }
  st[!ok] <- NA_character_
  st
}
# the rows the list ticks at first: the drafts to copy, new or newer
.ai_ticked_first <- function(d, state) {
  d$output_id[.ai_adoptable(d) & state %in% c("new", "replace")]
}

# The server part: `path()` the study's folder, `ver()` bumped when the
# answers are checked again, `reload_now()` the app's
#' @noRd
ai_list_server <- function(input, output, session, path, ver, reload_now, notify, guarded, t,
                           dirty = function() FALSE) {
  ns <- session$ns
  again <- shiny::reactiveVal(0L)
  shiny::observeEvent(input$list_reread, again(again() + 1L))
  listed <- shiny::reactive({
    ver()
    again()
    .ai_list(path())
  })
  # what copying each adoptable report would do (the plan asked once)
  copy_state <- shiny::reactive({
    d <- listed()
    if (is.null(d)) return(NULL)
    ids <- d$output_id[.ai_adoptable(d)]
    .ai_copy_state(d, if (length(ids)) .ai_adopt_plan(path(), ids))
  })
  # the reports ticked: at first the drafts new to the study or newer; the boxes, the
  # buttons change it (the table drawn again only for the buttons)
  ticked <- shiny::reactiveVal(character())
  redraw <- shiny::reactiveVal(0L)
  # (the list read again: the ticks the person left kept, the drafts new
  # to the list ticked as at first)
  seen <- new.env()
  shiny::observeEvent(listed(), {
    d <- listed()
    if (is.null(d)) {
      seen$ids <- NULL
      return(ticked(character()))
    }
    first <- .ai_ticked_first(d, copy_state())
    ticked(if (is.null(seen$ids)) first
           else union(intersect(ticked(), d$output_id[.ai_adoptable(d)]), setdiff(first, seen$ids)))
    # (a report with no draft yet -- "not answered" -- is not seen: its draft,
    # when it comes, is ticked)
    seen$ids <- d$output_id[.ai_adoptable(d)]
  }, ignoreNULL = FALSE)
  shiny::observeEvent(path(), seen$ids <- NULL, ignoreInit = TRUE)
  shiny::observeEvent(input$ck_toggle, {
    id <- input$ck_toggle$id
    ticked(if (isTRUE(input$ck_toggle$on)) union(ticked(), id) else setdiff(ticked(), id))
  })
  set_ticked <- function(ids) {
    ticked(ids)
    redraw(redraw() + 1L)
  }
  shiny::observeEvent(input$list_all, {
    d <- listed()
    if (!is.null(d)) set_ticked(d$output_id[.ai_adoptable(d)])
  })
  shiny::observeEvent(input$list_none, set_ticked(character()))
  shiny::observeEvent(input$list_new, {
    d <- listed()
    if (!is.null(d)) set_ticked(d$output_id[.ai_adoptable(d) & copy_state() %in% "new"])
  })
  shiny::observeEvent(input$list_newer, {
    d <- listed()
    if (!is.null(d)) set_ticked(d$output_id[.ai_adoptable(d) & copy_state() %in% "replace"])
  })
  shiny::observeEvent(input$list_changed, {
    d <- listed()
    if (!is.null(d)) set_ticked(.ai_ticked_first(d, copy_state()))
  })
  output$list_count <- shiny::renderUI({
    d <- listed()
    if (is.null(d) || !nrow(d)) return(NULL)
    n_draft <- sum(!is.na(d$file) & nzchar(d$file))
    n_adopt <- sum(startsWith(as.character(d$adopted %||% ""), "yes"), na.rm = TRUE)
    shiny::span(class = "small text-muted", sprintf(t("%d reports, %d drafts, %d adopted"),
                                                    nrow(d), n_draft, n_adopt))
  })
  output$list_ticked <- shiny::renderUI({
    if (is.null(listed())) return(NULL)
    shiny::span(class = "small", sprintf(t("%d ticked"), length(ticked())))
  })
  output$list_empty <- shiny::renderUI({
    d <- listed()
    if (is.null(d) || !nrow(d)) {
      shiny::p(class = "small text-muted",
               t("No report listed yet: write the requests (step 1), then check the agent's SPEC drafts (step 3)."))
    }
  })
  output$list_table <- DT::renderDT({
    d <- listed()
    redraw()
    shiny::req(!is.null(d), nrow(d) > 0L)
    on <- shiny::isolate(ticked())
    ok <- .ai_adoptable(d)
    ck <- sprintf("<input type=\"checkbox\" class=\"ai-adopt-ck\" data-ns=\"%s\" data-id=\"%s\"%s%s>",
                  ns(""), htmltools::htmlEscape(d$output_id, attribute = TRUE),
                  ifelse(d$output_id %in% on, " checked", ""), ifelse(ok, "", " disabled"))
    rtf <- ifelse(!is.na(d$rtf) & nzchar(d$rtf), sprintf(
      "<a href=\"#\" class=\"ai-open-rtf\" data-ns=\"%s\" data-file=\"%s\">%s</a>",
      ns(""), htmltools::htmlEscape(d$rtf, attribute = TRUE), t("Open")), "")
    # (tflspec's words in the app's language: the state, adopted)
    tw <- function(x) ifelse(is.na(x) | !nzchar(x), "", vapply(as.character(x), t, ""))
    # (what copying would do, in plain words)
    state_words <- c(new = t("Not copied"), replace = t("The draft is newer (can overwrite)"),
                     same = t("Copied (same)"))
    copy_cell <- unname(state_words[as.character(copy_state())])
    # (a report's status in this list's words: "ok" and "error" are kept in
    # English elsewhere, as program states)
    # (the check's three levels: ERROR, WARNING, OK -- step 3's words)
    tw_status <- function(x) {
      x <- as.character(x)
      lv <- .ai_check_level(x)
      out <- tw(x)
      has <- !is.na(lv)
      out[has] <- vapply(lv[has], t, "", USE.NAMES = FALSE)
      out
    }
    # (a warning's or an error's reason under it, from ai/manifest.csv)
    v <- data.frame(ck, d$output_id, d$title, d$population, d$split,
                    .ai_status_cells(tw_status(d$status), d$status, d$output_id,
                                     .ai_manifest_reasons(path())),
                    copy_cell, rtf, d$problem, stringsAsFactors = FALSE)
    names(v) <- c(t("Tick to copy"), "output_id", t("Title (title)"), t("Analysis set (population)"),
                  t("Split (split)"), t("Check"), paste0(t("Adopted"), " (adopted)"), "RTF",
                  t("First problem"))
    v[is.na(v)] <- ""
    # (the boxes and the RTF links: not escaped)
    DT::datatable(v, rownames = FALSE, escape = which(!names(v) %in% c(t("Tick to copy"), t("Check"), "RTF")),
                  selection = "none",
                  options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
  })
  shiny::observeEvent(input$open_rtf, {
    f <- input$open_rtf$file %||% ""
    p <- path()
    if (!nzchar(f) || is.null(p)) return()
    full <- if (grepl("^(/|[A-Za-z]:)", f)) f else file.path(p, f)
    if (!file.exists(full)) return(notify(sprintf(t("No file %s."), f), "warning"))
    opener <- getOption("tflplanner.open_folder", .external_browser)
    opener(normalizePath(full))
  })
  # the reports ticked, apart: those not in the study yet, those it has
  # with an older draft (copying overwrites the study's), and the same ones
  split_ticked <- function() {
    d <- listed()
    ids <- intersect(ticked(), d$output_id[.ai_adoptable(d)])
    # (asked again at the click: the study may have changed since the list was read)
    st <- stats::setNames(.ai_copy_state(d, .ai_adopt_plan(path(), ids)), d$output_id)
    list(fresh = ids[st[ids] %in% "new"], over = ids[st[ids] %in% "replace"],
         same = ids[st[ids] %in% "same"])
  }
  pending <- shiny::reactiveVal(NULL)
  table_of <- function(d, cols, heads) {
    # (one line a row, within the dialog: a cell too long is cut, its full
    # text on hover)
    cell <- function(x) shiny::tags$td(class = "text-truncate", title = as.character(x), as.character(x))
    shiny::tags$table(class = "table table-sm small mb-2", style = "table-layout: fixed; width: 100%",
                      shiny::tags$thead(shiny::tags$tr(lapply(heads, shiny::tags$th))),
                      shiny::tags$tbody(lapply(seq_len(nrow(d)), function(r)
                        shiny::tags$tr(lapply(d[r, cols], cell)))))
  }
  # (not while the study has unsaved changes: the adopted files would be
  # taken in over them, or the study left apart from its files)
  unsaved <- function() {
    if (!isTRUE(dirty())) return(FALSE)
    notify(t("Save first: the study has unsaved changes."), "warning")
    TRUE
  }
  shiny::observeEvent(input$adopt, {
    if (unsaved()) return()
    if (!length(ticked())) return(notify(t("Tick the reports to adopt."), "warning"))
    s <- split_ticked()
    pending(s)
    if (!length(s$fresh) && !length(s$over)) {
      return(notify(t("The reports ticked are in the study as they are: nothing to adopt."), "message"))
    }
    diff <- .ai_adopt_diff(path(), s$over)
    if (!is.null(diff)) diff$change <- vapply(diff$change, t, "", USE.NAMES = FALSE)
    shiny::showModal(shiny::modalDialog(
      title = t("Adopt (copy) into the study"),
      if (length(s$fresh)) shiny::p(sprintf(t("Not in the study yet, copied to its Report list (%d): %s"),
                                            length(s$fresh), paste(s$fresh, collapse = ", "))),
      if (length(s$over)) shiny::div(
        class = "alert alert-warning py-2 small",
        shiny::strong(sprintf(t("In the study already (%d): %s"), length(s$over), paste(s$over, collapse = ", "))),
        shiny::p(class = "mb-1", t("Copying these replaces the study's reports with the AI's drafts: settings made on the screen may be lost. What was set for them in the study and not in the drafts may be lost too. They are left as they are unless you overwrite them.")),
        if (!is.null(diff)) shiny::tags$details(
          open = NA, shiny::tags$summary(sprintf(t("What replacing changes (%d)"), nrow(diff))),
          table_of(diff, c("output_id", "where", "change", "adopted", "draft"),
                   c("output_id", t("Where"), t("Change"), t("In the study"), t("Draft"))))),
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        if (length(s$over)) .btn(ns("adopt_over"), sprintf(t("Overwrite %d and copy"), length(s$over)),
                                 class = "btn-danger"),
        if (length(s$fresh)) .btn(ns("adopt_fresh"), sprintf(t("Copy the %d not in the study yet"), length(s$fresh)),
                                  class = "btn-primary")),
      size = "xl", easyClose = TRUE))
  })
  do_adopt <- function(fresh, over) {
    shiny::removeModal()
    if (unsaved()) return()
    p <- path()
    # (each call NULL when it failed: guarded() said why)
    r_fresh <- if (length(fresh)) guarded(tflspec::tfl_ai_adopt(p, outputs = fresh, overwrite = FALSE))
    r_over <- if (length(over)) guarded(tflspec::tfl_ai_adopt(p, outputs = over, overwrite = TRUE))
    again(again() + 1L)
    # (a draft with an ERROR is not copied: tflspec says which, and why)
    skipped <- c(.ai_adopt_skipped(r_fresh), .ai_adopt_skipped(r_over))
    if (length(skipped)) {
      notify(paste0(sprintf(t("%d drafts were not copied: they have an ERROR (fix them in step 3 and check again)."),
                            length(skipped)), " ",
                    paste0(names(skipped), ": ", skipped, collapse = "; ")),
             "error")
    }
    fresh <- setdiff(if (!is.null(r_fresh)) fresh, names(skipped))
    over <- setdiff(if (!is.null(r_over)) over, names(skipped))
    n_fresh <- length(fresh)
    n_over <- length(over)
    if (!(n_fresh + n_over)) return()
    # (saved too: their programs and batch.R written, for a run)
    adopted <- c(fresh, over)
    saved <- isTRUE(reload_now(save = TRUE, after = function(x) .ai_apply_toc_batches(x, p, adopted)))
    notify(if (saved) sprintf(t("%d reports adopted (copied) into the study, %d of them replaced: on the Report list."),
                              n_fresh + n_over, n_over)
           else sprintf(t("%d reports adopted (copied) into the study, but the study was not saved: save it to write their programs."),
                        n_fresh + n_over),
           if (saved) "message" else "warning")
  }
  shiny::observeEvent(input$adopt_fresh, {
    s <- pending()
    if (!is.null(s)) do_adopt(s$fresh, character())
  })
  shiny::observeEvent(input$adopt_over, {
    s <- pending()
    if (!is.null(s) && length(s$over)) do_adopt(s$fresh, s$over)
  })
  invisible(listed)
}

# The drafts tflspec::tfl_ai_adopt() did not copy because they have an
# ERROR (status "skipped (error)"), each its first problem, named by the
# report; none: character()
.ai_adopt_skipped <- function(res) {
  if (!is.data.frame(res) || !all(c("output_id", "status") %in% names(res))) return(character())
  i <- res$status %in% "skipped (error)"
  if (!any(i)) return(character())
  why <- if ("problem" %in% names(res)) as.character(res$problem[i]) else rep(NA_character_, sum(i))
  why[is.na(why) | !nzchar(why)] <- "ERROR"
  stats::setNames(why, as.character(res$output_id[i]))
}

# Whether a report's file (from the study folder) is the study's own,
# spec/outputs -- not a draft in the AI's area, ai/spec/outputs
.ai_in_study_spec <- function(f) {
  f <- as.character(f)
  !is.na(f) & startsWith(gsub("\\\\", "/", f), "spec/")
}

# ---- the batches of the TOC (Topline, ...) of the reports adopted ----------
# The one place the AI's TOC gives the report sets: spec/toc.yml, each
# output's `sets` (tflspec: a TOC_<batch> sheet or a column of marks) or
# `batches` (a name or a list of them), or a top-level `batches` map,
# name -> ids.

# toc.yml's batches: a data frame batch, output_id (none: no row)
.ai_toc_batches <- function(p) {
  none <- data.frame(batch = character(), output_id = character(), stringsAsFactors = FALSE)
  f <- if (!is.null(p)) file.path(p, "spec", "toc.yml")
  if (is.null(f) || !file.exists(f)) return(none)
  y <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
  if (!is.list(y)) return(none)
  names_of <- function(b) {
    b <- unlist(b, use.names = FALSE)
    b <- unlist(strsplit(as.character(b), "|", fixed = TRUE))
    b <- trimws(b)
    b[!is.na(b) & nzchar(b)]
  }
  rows <- list()
  for (o in y$outputs %||% list()) {
    if (!is.list(o) || is.null(o$id)) next
    b <- names_of(c(o$sets, o$batches))
    if (length(b)) rows[[length(rows) + 1L]] <- data.frame(batch = b, output_id = as.character(o$id),
                                                           stringsAsFactors = FALSE)
  }
  top <- y$batches
  if (is.list(top) && !is.null(names(top))) {
    for (n in names(top)) {
      ids <- names_of(top[[n]])
      if (length(ids)) rows[[length(rows) + 1L]] <- data.frame(batch = n, output_id = ids,
                                                               stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(none)
  out <- unique(do.call(rbind, rows))
  rownames(out) <- NULL
  out
}

# The planner with the reports `ids` in their TOC batches (a report's
# batches set already are kept; a name a batch folder cannot carry is left)
.ai_apply_toc_batches <- function(x, p, ids) {
  b <- .ai_toc_batches(p)
  if (!nrow(b) || !length(ids)) return(x)
  have <- x$outputs$batches %||% rep(NA_character_, nrow(x$outputs))
  for (id in intersect(ids, x$outputs$output_id)) {
    i <- match(id, x$outputs$output_id)
    if (!is.na(have[i]) && nzchar(have[i])) next
    n <- b$batch[b$output_id == id]
    n <- n[vapply(n, function(v) !inherits(tryCatch(.check_batch_name(v), error = function(e) e), "error"), NA)]
    if (length(n)) x <- .set_report_batches(x, id, n)
  }
  x
}

# The AI's reports among `ids` an official run should say before it runs:
# `newer` (their draft changed since adopted), `confirm` (places the AI
# left to confirm); NULL study: none
.ai_run_warnings <- function(p, ids) {
  none <- list(newer = character(), confirm = character())
  if (is.null(p) || !length(ids) || !dir.exists(file.path(p, "ai"))) return(none)
  m <- .ai_adopted_marks(p, ids)
  # (the study's reports with places still to confirm: tfl_ai_confirm())
  cf <- tryCatch(tflspec::tfl_ai_confirm(p, where = "adopted"), error = function(e) NULL)
  confirm <- if (is.data.frame(cf) && nrow(cf)) intersect(ids, unique(cf$output_id)) else character()
  list(newer = names(m)[m == "newer"], confirm = confirm)
}

# The reason a warning or an error report carries: ai/manifest.csv's
# `reason` column (tflspec's collect writes it; a manifest of before has no
# such column: no reason, NULL).  A named character vector, output_id ->
# reason, the rows that have one.
.ai_manifest_reasons <- function(dir) {
  f <- if (is.null(dir)) NULL else file.path(dir, "ai", "manifest.csv")
  if (is.null(f) || !file.exists(f)) return(NULL)
  m <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character",
                                encoding = "UTF-8", na.strings = character(), check.names = FALSE),
                error = function(e) NULL)
  if (is.null(m) || !all(c("output_id", "reason") %in% names(m))) return(NULL)
  m <- m[nzchar(trimws(m$reason)) & nzchar(trimws(m$output_id)), , drop = FALSE]
  m <- m[!duplicated(m$output_id), , drop = FALSE]
  if (!nrow(m)) return(NULL)
  stats::setNames(trimws(m$reason), m$output_id)
}

# The Check cell of step 4's list: the status, and under it the reason of a
# warning or an error (HTML; the texts escaped).  `status`: the cell's words,
# `raw`: the report's status (ok / warning / error ...), `reason`: the
# reasons, as .ai_manifest_reasons() gives them, or NULL.
.ai_status_cells <- function(status, raw, ids, reason) {
  out <- htmltools::htmlEscape(status)
  if (is.null(reason)) return(out)
  why <- unname(reason[match(ids, names(reason))])
  show <- as.character(raw) %in% c("warning", "error") & !is.na(why)
  out[show] <- sprintf("%s<div class=\"small text-muted ai-reason\">%s</div>", out[show],
                       htmltools::htmlEscape(why[show]))
  out
}
