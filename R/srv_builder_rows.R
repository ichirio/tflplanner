# app_server(): builder rows -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_builder_rows <- function() {
  # The rows' variables as the code list of `variable`: each variable (the
  # value), the heading the table prints for it (the label) and its place
  # (the order) -- a grid like the code lists', opened under the list.  It
  # is the same thing as the list and the labels above: an edit writes the
  # variables sheet (label, order), and the form is drawn again from it.
  vcl_open <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$b_vcl_toggle, vcl_open(!vcl_open()))
  output$b_vcl <- shiny::renderUI({
    bform_drawn()
    shiny::req(builder_case() %in% c("ok", "hierarchy"))
    on <- vcl_open()
    shiny::tagList(
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-link py-0 px-0 mb-1",
        onclick = "Shiny.setInputValue('b_vcl_toggle', Math.random(), {priority: 'event'})",
        shiny::span(class = "text-muted me-1", if (on) "\u25be" else "\u25b8"),
        t("Headings and order as a grid")),
      if (on) shiny::div(
        class = "border rounded p-2 mb-2 bg-body-tertiary",
        shiny::p(class = "small text-muted mb-1",
                 t("Each variable (value), the heading the table prints for it (label; blank: as the ARD names it) and its place (order). The same as the list above and its labels: the program writes the headings as plan_labels() and the order as plan_levels(variable = ...).")),
        rhandsontable::rHandsontableOutput("b_vcl_hot")))
  })
  vcl_key <- shiny::reactive(paste("vcl", bform_drawn(), sep = "|"))
  output$b_vcl_hot <- rhandsontable::renderRHandsontable({
    shiny::req(vcl_open(), builder_case() %in% c("ok", "hierarchy"))
    vcl_key()
    v <- bform$st$variables
    d <- data.frame(value = v$variable,
                    label = ifelse(is.na(v$label), "", v$label),
                    order = as.character(seq_len(nrow(v))),
                    stringsAsFactors = FALSE)
    h <- rhandsontable::rhandsontable(
      d, rowHeaders = TRUE, useTypes = FALSE, stretchH = "all",
      height = .grid_height(nrow(d)), planner_key = vcl_key())
    h <- rhandsontable::hot_context_menu(h, allowRowEdit = FALSE,
                                         allowColEdit = FALSE)
    suppressWarnings(rhandsontable::hot_col(h, "value", readOnly = TRUE))
  })
  shiny::observeEvent(input$b_vcl_hot, {
    h <- input$b_vcl_hot
    if (is.null(h$changes$changes)) return()
    if (!identical(h$params$planner_key, vcl_key())) return()
    d <- read_grid(h)
    st <- shiny::isolate(bstate())
    if (is.null(d) || is.null(st)) return()
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    o <- suppressWarnings(as.numeric(d$order))
    d <- d[order(is.na(o), o, seq_len(nrow(d))), , drop = FALSE]
    if (!setequal(d$value, st$variables$variable)) return()
    v <- st$variables[match(d$value, st$variables$variable), , drop = FALSE]
    lab <- trimws(as.character(d$label))
    v$label <- ifelse(is.na(lab) | !nzchar(lab), NA_character_, lab)
    rownames(v) <- NULL
    st$variables <- v
    p2 <- guarded(builder_write(rv$p, bform$id, st, was = bform$last))
    shiny::req(!is.null(p2))
    rv$p <- p2
    rv$bver <- rv$bver + 1L
  })
  # plan_cells() by variable (#454): the table on the plan_cells() card,
  # drawn again when a row's shape changes (cv_ver) or the form is; with no
  # ARD to edit against, what the plan says, read only
  cv_ver <- shiny::reactiveVal(0L)
  output$b_cv_ui <- shiny::renderUI({
    cv_ver()
    bform_drawn()
    id <- bform$id
    shiny::req(!is.null(id))
    p <- shiny::isolate(rv$p)
    m <- bform$meta
    pc <- tryCatch(.builder_args(builder_items(p, id, m), "plan_cells"),
                   error = function(e) NULL) %||% list()
    if (!shiny::isolate(builder_case()) %in% c("ok", "hierarchy") || is.null(m)) {
      return(.cells_vars_ro(pc, lang))
    }
    tryCatch(
      .cells_vars_ui(pc, m, bform$st, dg = .report_digits(p, id), lang = lang,
                     diff = tryCatch(.cells_company_diff(p), error = function(e) NULL),
                     edits = bform$cv_edit[[id]] %||% list()),
      error = function(e) {
        message("tflplanner: builder: ", conditionMessage(e))
        .cells_vars_ro(pc, lang)
      })
  })
  shiny::observeEvent(input$b_cv_std, {
    go("study")
    session$sendCustomMessage("rp-open-sec", "cells")
  })
  shiny::observeEvent(input$b_cv_ev, builder_guard({
    ev <- input$b_cv_ev
    id <- bform$id
    v <- ev$v %||% ""
    shiny::req(!is.null(id), identical(id, current()), nzchar(v))
    p <- rv$p
    m <- bform$meta
    st <- bform$st
    pc <- .builder_args(builder_items(p, id, m), "plan_cells") %||% list()
    act <- ev$act %||% ""
    edits <- bform$cv_edit[[id]] %||% list()
    redraw <- TRUE
    value <- pc[[v]]
    if (identical(act, "mode")) {
      # own: a copy of the common template of the kind chosen (the ARD's,
      # or the other); common: the own template goes
      md <- ev$value %||% ""
      if (startsWith(md, "own")) {
        k <- sub("^own:?", "", md)
        if (!k %in% c("continuous", "categorical")) {
          k <- .cells_vars_rows(pc, m)$kind[match(v, .cells_vars_rows(pc, m)$variable)]
        }
        value <- pc[[v]] %||% .cells_common_entry(pc, st, if (is.na(k)) "continuous" else k)
      } else {
        value <- NULL
      }
      edits[[v]] <- NULL
    } else {
      ed <- edits[[v]]
      e <- if (!is.null(ed) && identical(.cells_entry_build(ed), pc[[v]])) ed else
        .cells_entry_parse(pc[[v]])
      shiny::req(!identical(e$type, "r"))
      k <- ev$k %||% "0"
      u <- .cells_entry_part(e, k)
      shiny::req(!is.null(u))
      i <- as.integer(ev$i %||% NA)
      n <- length(u$conds)
      if (identical(act, "tpl")) {
        redraw <- FALSE
        u$fallback <- ev$value %||% ""
      } else if (identical(act, "cond")) {
        redraw <- FALSE
        shiny::req(!is.na(i), i >= 1L, i <= n, ev$field %in% c("col", "op", "value", "tpl"))
        u$conds[[i]][[ev$field]] <- ev$value %||% ""
        u$conds[[i]]["raw"] <- list(NULL)
        u$conds[[i]]$ok <- TRUE
      } else if (identical(act, "cond_add")) {
        # (written once its column and value are said)
        cols <- c(.cells_var_stats(m, v), .cells_cond_columns(m))
        u$conds[[n + 1L]] <- list(col = cols[1L] %||% "n", op = "==", value = "",
                                  tpl = if (is.na(u$fallback)) "" else u$fallback,
                                  raw = NULL, ok = TRUE)
      } else if (identical(act, "cond_rm")) {
        shiny::req(!is.na(i), i >= 1L, i <= n)
        u$conds <- u$conds[-i]
      } else if (act %in% c("cond_up", "cond_down")) {
        j <- if (identical(act, "cond_up")) i - 1L else i + 1L
        shiny::req(!is.na(i), i >= 1L, i <= n, j >= 1L, j <= n)
        u$conds[c(i, j)] <- u$conds[c(j, i)]
      } else return()
      e <- .cells_entry_part_set(e, k, u)
      # (kept only while a condition is not said yet: what the plan has
      # is read again otherwise)
      edits[v] <- list(if (.cells_entry_pending(e)) e)
      value <- .cells_entry_build(e)
    }
    bform$cv_edit[[id]] <- edits
    p2 <- guarded(.cells_var_put(p, id, v, value))
    shiny::req(!is.null(p2))
    if (!identical(p2, p)) {
      rv$p <- p2
      rv$btouched <- TRUE
    }
    if (!redraw) return()
    if (identical(p2, p)) cv_ver(cv_ver() + 1L) else rv$bver <- rv$bver + 1L
  }))
  # A variable's own decimals: the list (each with x to take it away) and a
  # line to add one
  builder_exc_ui <- function(n, st, rows) {
    exc_ver()
    exc <- bform$exc %||% st$exceptions
    tpl <- builder_templates(st)
    used <- unique(c(unlist(lapply(tpl[intersect(rows, names(tpl))], .template_stats)),
                     "n", "p"))
    id <- function(x) paste0("b", n, "_", x)
    shiny::tags$details(
      class = "small mb-2", open = if (nrow(exc)) NA,
      shiny::tags$summary(sprintf(t("A variable of its own (%d)"), nrow(exc))),
      if (nrow(exc)) shiny::tags$ul(
        class = "list-unstyled mb-1",
        lapply(seq_len(nrow(exc)), function(i) shiny::tags$li(
          sprintf("%s: %s %d", exc$variable[i], exc$statistic[i], exc$digits[i]),
          shiny::tags$a(
            href = "#", class = "ms-2 text-danger", title = t("Take it away"),
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'}); return false;",
                              id("exc_rm"), i), "\u00d7")))),
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-end",
        shiny::selectInput(id("exc_var"), t("Variable"), st$variables$variable,
                           width = "140px"),
        shiny::selectInput(id("exc_stat"), t("Statistic"), used, width = "110px"),
        shiny::numericInput(id("exc_dg"), t("Decimals"), value = 2, min = 0,
                            max = 6, step = 1, width = "88px"),
        .btn(id("exc_add"), t("Add"), class = "btn-sm btn-outline-primary mb-3")))
  }
  # (the form drawn again has new ids: read them again when it is)
  shiny::observeEvent({
    bform_drawn()
    input[[bid("exc_add")]]
  }, {
    v <- input[[bid("exc_var")]]
    k <- input[[bid("exc_stat")]]
    d <- suppressWarnings(as.integer(input[[bid("exc_dg")]]))
    shiny::req(nzchar(v %||% ""), nzchar(k %||% ""), length(d) == 1L, !is.na(d), d >= 0L)
    exc <- bform$exc %||% data.frame(variable = character(), statistic = character(),
                                     digits = integer(), stringsAsFactors = FALSE)
    exc <- exc[!(exc$variable == v & exc$statistic == k), , drop = FALSE]
    exc[nrow(exc) + 1L, ] <- list(v, k, d)
    rownames(exc) <- NULL
    bform$exc <- exc
    exc_ver(exc_ver() + 1L)
  })
  # (the form drawn again has new ids: read them again when it is)
  shiny::observeEvent({
    bform_drawn()
    input[[bid("exc_rm")]]
  }, {
    i <- input[[bid("exc_rm")]]
    exc <- bform$exc
    shiny::req(!is.null(exc), i >= 1L, i <= nrow(exc))
    exc <- exc[-i, , drop = FALSE]
    rownames(exc) <- NULL
    bform$exc <- exc
    exc_ver(exc_ver() + 1L)
  })
  # a row of one's own: added at the end of the shown rows, written at once
  # (the form drawn again has new ids: read them again when it is)
  shiny::observeEvent({
    bform_drawn()
    input[[bid("own_add")]]
  }, {
    lb <- trimws(input[[bid("own_label")]] %||% "")
    tp <- trimws(input[[bid("own_tpl")]] %||% "")
    if (!nzchar(lb) || !grepl("[{][^}]+[}]", tp)) {
      return(notify(t("A row to add needs a label and a template with a {statistic}.")))
    }
    st <- shiny::isolate(bstate())
    st$rows <- unique(c(st$rows, lb))
    st$templates[[lb]] <- tp
    p2 <- guarded(builder_write(rv$p, bform$id, st, was = bform$last))
    shiny::req(!is.null(p2))
    rv$p <- p2
    rv$bver <- rv$bver + 1L
  })
  # A continuous variable's rows: those shown, in order (drag), and those
  # offered (the company standards' rows); a row whose template needs a
  # statistic this report's ARD does not have says so.  A row of one's own:
  # a label and a template.
  builder_rows_ui <- function(bs, st, have) {
    tpl <- builder_templates(st)
    label_of <- function(lb) {
      lack <- .builder_stats_lacking(tpl[[lb]] %||% "", have)
      if (!nzchar(lack)) return(lb)
      shiny::span(lb, shiny::tags$small(class = "text-muted ms-1",
                                        sprintf(t("(the ARD has no %s)"), lack)))
    }
    offered <- setdiff(names(tpl), st$rows)
    shiny::tagList(
      sortable::bucket_list(
        header = NULL,
        group_name = bid("rows_group"),
        orientation = "horizontal",
        sortable::add_rank_list(
          text = t("Continuous variables' rows (continuous)"),
          labels = stats::setNames(lapply(st$rows, label_of), st$rows),
          input_id = bid("rows")),
        sortable::add_rank_list(
          text = t("Not shown (drag a row to the left)"),
          labels = stats::setNames(lapply(offered, label_of), offered),
          input_id = bid("rows_off"))),
      shiny::tags$details(
        class = "small mb-2", open = NA,
        shiny::tags$summary(with_tip(t("Add a row (row label = template)"),
                                      t("One row of plan_cells(continuous = c(row label = template)). Write the ARD's statistic names, {mean} {sd} ...; {mean:.2f} sets the decimals too."))),
        shiny::div(
          class = "d-flex flex-wrap gap-2 align-items-end",
          # (an example, as the placeholder: the fields start empty)
          shiny::textInput(bid("own_label"), t("Row label"),
                           placeholder = sprintf(t("e.g. %s"), "Mean +/- SD"), width = "160px"),
          shiny::textInput(bid("own_tpl"),
                           with_tip(t("Template"), t("{statistic} as the ARD names it: N, mean, sd, se, median, p25, p75, min, max ...; a format after a colon: {mean:.2f} two decimals, {p:.1f%} a percent. Blank: the decimals above. The table on the right shows the row once added.")),
                           placeholder = sprintf(t("e.g. %s"), "{mean} +/- {sd}"), width = "200px"),
          .btn(bid("own_add"), t("Add the row"), class = "btn-sm btn-outline-primary mb-3"))))
  }
  # every row's template: the table's, the standards', one's own
  builder_templates <- function(st) {
    bs <- builder_stats()
    tpl <- stats::setNames(as.list(bs$template), bs$row)
    own <- as.list(st$templates %||% list())
    tpl[names(own)] <- own
    tpl[unique(c(st$rows, names(tpl)))]
  }
  # the decimals of the statistics the shown rows print; each a number
  output_digits_ui <- function(n, st, rows) {
    tpl <- builder_templates(st)
    used <- unique(unlist(lapply(tpl[intersect(rows, names(tpl))], .template_stats)))
    if (!length(used)) return(NULL)
    rule <- .report_digits(shiny::isolate(rv$p), bform$id)
    cur <- st$digits
    shiny::tagList(
      shiny::tags$label(class = "form-label small mb-0",
                        with_tip(t("Decimals of each statistic"),
                                 t("The same for every analysis variable; a variable of its own below. A row's template that says its own format ({mean:.2f}) keeps it."))),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        lapply(used, function(k) {
          typed <- suppressWarnings(as.integer(
            shiny::isolate(input[[paste0("b", n, "_dg_", k)]])))
          v <- .or_na(typed, .or_na(unname(cur[k]), .or_na(unname(rule[k]), 1L)))
          shiny::numericInput(paste0("b", n, "_dg_", k), k, value = v, min = 0,
                              max = 6, step = 1, width = "88px")
        })))
  }
  # Whatever goes wrong in the builder stays in the builder: it is said once
  # (and logged), and the reactive stops quietly (req) instead of ending the
  # session.  (req's own silent stop passes through.)
  builder_guard <- function(expr) {
    tryCatch(expr, error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)
      message("tflplanner: builder: ", conditionMessage(e))
      notify(sprintf(t("The builder could not read the form: %s"),
                     conditionMessage(e)), "error")
      shiny::req(FALSE)
    })
  }

  # A table with no definition yet (made without the first-table form, an
  # older study, a workbook read in): what the builder shows is written to
  # the sheets once, so the preview has a table to show.  A table with no
  # row of its own in the tables sheet yet also gets, once, the cards the
  # ARD tells the values of, on: its Total column (rows over all
  # subjects), its factors' order, its labels -- then it has its own row,
  # and what the user switches off later stays off.
  # (not ignoreInit: the first form may be drawn in the flush that makes
  # this observer, and its draw must count)
  shiny::observeEvent(bform_drawn(), {
    shiny::req(bform_drawn() > 0L)
    id <- bform$id
    # (a template report: its items are the template's; nothing written
    # for a preview -- R/template_report.R)
    if (!is.null(.tpl_of(rv$p, id %||% ""))) {
      bform$fresh <- FALSE
      return()
    }
    tb <- sheet_rows(rv$p, "tables", id)
    inh <- inherited_rows(rv$p, "tables", id)
    has_cols <- (nrow(tb) && !is.na(tb$cols[1L])) ||
      (nrow(inh) && !is.na(inh$cols[1L]))
    # (fresh: no row of its own when the form was drawn -- the form's own
    # first write may have made one since)
    fresh <- isTRUE(bform$fresh)
    bform$fresh <- FALSE
    if (anyNA(bform$st$key) || (has_cols && !fresh)) return()
    p2 <- if (has_cols) rv$p else guarded(builder_write(rv$p, id, bform$st))
    if (is.null(p2)) return()
    hier <- length(bform$st$hierarchy) > 0L
    wrote <- FALSE
    if (fresh) {
      for (fn in .ard_on_cards(bform$meta, hier)) {
        have <- vapply(tryCatch(builder_items(p2, id, bform$meta), error = function(e) list()),
                       function(it) it$fn, "")
        if (fn %in% have) next
        p3 <- if (identical(fn, "plan_total")) {
          guarded(set_total_column(p2, id, "Total", "last"))
        } else {
          args <- .plan_starter_args(fn, bform$st, hier, meta = bform$meta)
          if (!is.null(args)) guarded(.builder_item_write(p2, id, fn, args))
        }
            if (!is.null(p3)) {
          p2 <- p3
          wrote <- TRUE
        }
      }
    }
    if (identical(p2, rv$p)) return()
    # (held: no change until one is made, .unheld())
    held$before <- if (!is.null(held$p) && identical(rv$p, held$p)) held$before else rv$p
    held$p <- p2
    rv$p <- p2
    rv$btouched <- TRUE
    # (the cards written: drawn again, on)
    if (wrote) rv$bver <- rv$bver + 1L
  })

  # "Back to the ARD's order" (plan_levels()' card, #428): the variable's
  # values -- a column key's, or the analysis variables' blocks -- in the
  # order the ARD keeps them; written as no order of the table's own, so
  # the code writes none (tflspec #355)
  shiny::observeEvent(input$b_lv_reset, builder_guard({
    nm <- input$b_lv_reset$variable
    st <- shiny::isolate(bstate())
    shiny::req(length(nm) == 1L, !is.null(st), identical(bform$id, current()))
    if (identical(nm, "variable")) {
      ard <- bform$st$variable_ard
      shiny::req(length(ard))
      vs <- st$variables$variable
      un <- st$variables$under %||% rep(NA_character_, length(vs))
      top <- vs[is.na(un) | !nzchar(un)]
      want <- c(intersect(ard, top), setdiff(top, ard))
      # (a nested variable after its parent, as it was shown)
      out <- character()
      for (v0 in want) {
        out <- c(out, v0)
        out <- c(out, vs[!is.na(un) & startsWith(un, paste0(v0, ":"))])
      }
      out <- c(out, setdiff(vs, out))
      st$variables <- st$variables[match(out, vs), , drop = FALSE]
      rownames(st$variables) <- NULL
    } else {
      ard <- bform$st$level_ard[[nm]]
      shiny::req(length(ard))
      st$auto_levels[[nm]] <- ard
      if (nm %in% names(st$arms)) st$arms[[nm]] <- ard else st$levels[[nm]] <- ard
    }
    p2 <- guarded(builder_write(rv$p, bform$id, st, was = bform$last))
    shiny::req(!is.null(p2))
    rv$p <- p2
    rv$btouched <- TRUE
    rv$bver <- rv$bver + 1L
  }))
  shiny::observeEvent(input$b_add_n, {
    id <- bform$id
    key <- bform$st$key[1L]
    p2 <- guarded(add_group_n(rv$p, id, key))
    if (is.null(p2)) return()
    rv$p <- p2
    bump()
    do_preview(id)
  })

  # a column variable's levels in order: as the definition has them, else
  # the ARD's
  key_levels <- function(k) {
    bform$st$arms[[k]] %||% bform$meta$keys[[k]] %||% character()
  }
  arms_id <- function(k) bid(paste0("arms_", make.names(k)))
  # the columns follow the column variables chosen: their order (outermost
  # first) when there are several, and each one's levels in order
  shiny::observe(builder_guard({
    bform_drawn()
    ks <- input[[bid("key")]]
    shiny::req(builder_case() %in% c("ok", "hierarchy"), length(ks) > 0L)
    n <- bform$n
    output[[paste0("b", n, "_arms_ui")]] <- shiny::renderUI(shiny::tagList(
      lapply(ks, function(k) {
        # a line until opened, and whether the code writes its order, as
        # plan_levels()' other variables (.levels_var())
        lv <- key_levels(k)
        .levels_var(
          arms_id(k), k, lv,
          sortable::rank_list(text = NULL,
                              labels = .levels_with_labels(
                                lv, k, sheet_rows(shiny::isolate(rv$p), "codelists", bform$id)),
                              input_id = arms_id(k),
                              orientation = "horizontal"),
          ard = bform$st$level_ard[[k]], from = bform$st$level_from[[k]], lang = lang)
      })))
  }))
  # the Total column: a switch, its heading and place; it sets `overall` on
  # the analyses by the column variable too (set_total_column()), which
  # the ARD then needs to run again
  # (the report's own, else the study's defaults': the card's switch is on
  # for either, and drawing it writes nothing)
  total_now <- function(p0, id) {
    tb <- sheet_rows(p0, "tables", id)
    inh <- inherited_rows(p0, "tables", id)
    val <- function(col) {
      a <- if (nrow(tb)) tb[[col]][1L] %||% NA else NA
      if (is.na(a) && nrow(inh)) a <- inh[[col]][1L] %||% NA
      a
    }
    total <- val("total")
    on <- !is.na(total)
    list(on = on,
         label = if (on) total else NA_character_,
         position = if (on && identical(val("total_position"), "first"))
           "first" else "last")
  }
  shiny::observe(builder_guard({
    bform_drawn()
    ks <- input[[bid("key")]]
    shiny::req(builder_case() %in% c("ok", "hierarchy"), length(ks) > 0L)
    n <- bform$n
    id <- bform$id
    p0 <- shiny::isolate(rv$p)
    now <- total_now(p0, id)
    runs <- sum(.total_rows(ard_rows(p0, "analyses", id), ks[1L]))
    # (the switch is the card's: its body shows while it is on)
    output[[paste0("b", n, "_total_ui")]] <- shiny::renderUI(shiny::tagList(
      shiny::div(
        # (the fields' labels on one line, their boxes under them)
        class = "d-flex flex-wrap gap-3 align-items-start mb-2 rp-total-fields",
        shiny::textInput(bid("total_label"),
                         with_tip(builder_label(.arg_meaning("plan_total", "label"), "label", "arg", lang),
                                  .arg_help("plan_total", "label", lang = lang)),
                         if (now$on) now$label else "", placeholder = "Total", width = "160px"),
        shiny::radioButtons(bid("total_pos"),
                            with_tip(builder_label(.arg_meaning("plan_total", "position"), "position", "arg", lang),
                                     .arg_help("plan_total", "position", lang = lang)),
                            stats::setNames(c("last", "first"), c(t("last"), t("first"))),
                            now$position, inline = TRUE),
        help_tip(t("plan_total puts the ARD's overall rows (results not split by group) in a Total column.")),
        if (now$on) from_badge("ard", lang)),
      if (now$on && runs == 0L) shiny::div(
        class = "small text-warning",
        sprintf(t("No analysis of this report is grouped by %s: the ARD has no overall rows to show."), ks[1L]))
      else if (now$on) shiny::div(
        class = "small text-muted",
        sprintf(t("%d analyses are run over all subjects too: Update preview makes the ARD again."), runs))))
  }))
  shiny::observe(builder_guard({
    bform_drawn()
    on <- input[[total_on_id()]]
    lbl <- input[[bid("total_label")]]
    pos <- input[[bid("total_pos")]]
    shiny::req(!is.null(on), builder_case() %in% c("ok", "hierarchy"))
    id <- bform$id
    p0 <- shiny::isolate(rv$p)
    now <- total_now(p0, id)
    want_lbl <- if (!isTRUE(on)) NULL else
      if (is.null(lbl) || !nzchar(trimws(lbl))) {
        if (is.na(now$label)) "Total" else now$label
      } else trimws(lbl)
    want_pos <- pos %||% now$position
    same <- identical(isTRUE(on), now$on) &&
      (!isTRUE(on) || (identical(want_lbl, now$label) && identical(want_pos, now$position)))
    if (same) return()
    p2 <- guarded(set_total_column(p0, id, want_lbl, want_pos))
    if (!is.null(p2)) {
      rv$p <- p2
      rv$btouched <- TRUE
    }
  }))
  # ---- what a variable prints, as the Statistics card says (with the
  # variable's own decimals): shown, never written for it
  builder_var_defaults <- function(var, kind, st) {
    faint <- function(...) shiny::div(class = "small text-muted mb-2", ...)
    if (identical(kind, "continuous")) {
      tpl <- st$templates %||% list()
      rows <- intersect(st$rows %||% character(), names(tpl))
      dg <- st$digits %||% integer()
      ex <- st$exceptions
      if (!is.null(ex) && nrow(ex)) {
        mine <- ex[ex$variable == var, , drop = FALSE]
        dg[mine$statistic] <- mine$digits
      }
      used <- unique(unlist(lapply(tpl[rows], .template_stats)))
      dg <- dg[intersect(used, names(dg))]
      faint(
        shiny::div(t("Prints (the Statistics card's):"), " ",
                   paste(sprintf("%s %s", rows, unlist(tpl[rows])), collapse = " \u00b7 ")),
        if (length(dg) && identical(st$value %||% "stat", "stat"))
          shiny::div(t("Decimals:"), " ",
                     paste(sprintf("%s %s", names(dg), dg), collapse = " \u00b7 ")))
    } else {
      f <- .cat_formats()
      key <- st$cat_format %||% f$key[1L]
      what <- if (identical(key, "own")) st$cat_own %||% "" else
        sprintf("%s (%s)", f$label[match(key, f$key)],
                .cat_template(key, st$pct_decimals %||% 1, st$value %||% "stat"))
      faint(shiny::div(t("Prints (the Statistics card's):"), " ", what))
    }
  }
  shiny::observe(builder_guard({
    bform_drawn()
    n <- bform$n
    v <- bform$st$variables
    if (!is.data.frame(v)) return(invisible())
    for (i in seq_len(nrow(v))) local({
      ii <- i
      output[[paste0("b", n, "_vdef", ii)]] <- shiny::renderUI({
        st <- bstate()
        builder_var_defaults(v$variable[ii], v$kind[ii], st)
      })
    })
  }))
  .srv_keep(environment())
}
