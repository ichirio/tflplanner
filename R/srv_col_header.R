# app_server(): col header -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_col_header <- function() {
  # ---- the column header (Q14): one form a header line, in two parts --
  hdr_ver <- shiny::reactiveVal(0L)
  hid <- function(u, part) bid(sprintf("h%d_%s", u, part))
  # the values of the first column variable, in the Columns card's order:
  # the header grid's value columns
  hdr_levels <- function() {
    keys <- shiny::isolate(input[[bid("key")]]) %||% bform$st$key
    k <- keys[1L]
    if (is.null(k) || is.na(k)) return(character())
    as.character(shiny::isolate(input[[arms_id(k)]]) %||% key_levels(k))
  }
  # every line gets a number of its own once, kept when lines move; a
  # cell-by-cell line's cells cover the values in their order
  hdr_uid <- function(lines) {
    if (is.null(lines)) return(NULL)
    lv <- hdr_levels()
    for (i in seq_along(lines)) {
      if (is.null(lines[[i]]$uid)) {
        bform$uid <- (bform$uid %||% 0L) + 1L
        lines[[i]]$uid <- bform$uid
      }
      if (identical(lines[[i]]$mode, "cells")) {
        lines[[i]]$segments <- header_segments(lines[[i]]$segments, lv)
      }
    }
    lines
  }
  # the lines as the inputs have them now
  hdr_now <- function() {
    lines <- bform$hdr
    if (is.null(lines)) return(NULL)
    for (i in seq_along(lines)) {
      l <- lines[[i]]
      st <- l$stub
      for (k in seq_along(st)) {
        v <- input[[hid(l$uid, paste0("stub", k))]]
        if (!is.null(v)) st[[k]] <- v
      }
      l$stub <- st
      for (part in c("mode", "key", "text", "align")) {
        v <- input[[hid(l$uid, part)]]
        if (!is.null(v)) l[[part]] <- v
      }
      for (j in seq_along(l$segments)) {
        v <- input[[hid(l$uid, paste0("seg", j))]]
        if (!is.null(v)) l$segments[[j]]$text <- v
      }
      b <- input[[hid(l$uid, "bold")]]
      if (!is.null(b)) l$bold <- if (isTRUE(b)) "TRUE" else NA_character_
      u <- input[[hid(l$uid, "ul")]]
      if (!is.null(u)) l$border_bottom <- if (isTRUE(u)) "single" else NA_character_
      lines[[i]] <- l
    }
    lines
  }
  shiny::observe(builder_guard({
    hdr_ver()
    n <- bform$n
    output[[paste0("b", n, "_hdr_ui")]] <- shiny::renderUI({
      lines <- bform$hdr
      keys <- shiny::isolate(input[[bid("key")]]) %||% bform$st$key
      hp <- .header_preset_choices(length(bform$meta$hierarchy) > 0L,
                                   .key_label(bform$meta, keys[1L]))
      head <- shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-end mb-2",
        shiny::h6(class = "me-auto mb-0", t("Column header")),
        # the two together, so a narrow form wraps them as one
        shiny::div(
          class = "d-flex flex-wrap gap-1",
          .btn(bid("hdr_load"), t("Load a standard column header..."),
               class = "btn-sm btn-outline-primary"),
          .btn(bid("hdr_add"), t("Add a line above"), class = "btn-sm btn-outline-primary")))
      if (is.null(lines)) {
        return(shiny::tagList(head, shiny::div(
          class = "small text-muted",
          t("This report's column header is more than the form shows (positions, KEY = value, styled row-header cells): edit it as R on this card (plan_col_header())."))))
      }
      act <- function(i, what, label, j = 0L, title = NULL) shiny::tags$button(
        type = "button", class = "btn btn-sm btn-link py-0 px-1", label, title = title,
        onclick = sprintf("Shiny.setInputValue('%s', {i: %d, act: '%s', j: %d, t: Date.now()}, {priority: 'event'})",
                          bid("hdr_act"), i, what, j))
      # the grid's columns: the row-header columns, then the values of the
      # first column variable, as the table on the right has them
      lv <- hdr_levels()
      stub_cols <- unique(unlist(lapply(lines, function(l)
        trimws(unlist(strsplit(names(l$stub), "|", fixed = TRUE))))))
      if (!length(stub_cols)) stub_cols <- "row_label"
      ns <- length(stub_cols)
      nv <- max(1L, length(lv))
      span <- function(k) sprintf("grid-column: span %d;", k)
      modes <- stats::setNames(c("each", "key", "all", "cells", "none"),
                               c(t("the same on each column"), t("one cell per value of a key"),
                                 t("one cell over them all"), t("cell by cell (merge with \u21e5)"),
                                 t("nothing")))
      cap <- function(l) switch(l$mode,
        each = t("the same on each column"),
        all = t("one cell over them all"),
        key = sprintf(t("one cell per value of %s"), l$key %||% ""),
        "")
      header_line <- function(i) {
        l <- lines[[i]]
        st <- l$stub
        # the row-header cells, each under its column (one over them all
        # when merged)
        # (text areas, as the line's text: a cell's text may have line
        # breaks (a hierarchy's "System Organ Class", a line break, "Preferred
        # Term"), which a one-line field drops, and the header read back would differ
        # from the sheet's though nothing was changed)
        stub_ui <- if (isTRUE(l$merge)) {
          list(shiny::div(style = span(ns), shiny::textAreaInput(
            hid(l$uid, "stub1"), NULL, if (is.na(st[[1L]])) "" else st[[1L]],
            width = "100%", rows = 1, placeholder = t("e.g. Characteristic"))))
        } else lapply(stub_cols, function(cn) {
          k <- match(cn, names(st))
          if (is.na(k)) return(shiny::div())
          shiny::textAreaInput(hid(l$uid, paste0("stub", k)), NULL,
                               if (is.na(st[[k]])) "" else st[[k]], width = "100%",
                               rows = 1, placeholder = t("e.g. Characteristic"))
        })
        val_ui <- switch(
          l$mode,
          none = list(shiny::div(style = span(nv), class = "rp-hdr-none", "\u2014")),
          cells = lapply(seq_along(l$segments), function(j) {
            sg <- l$segments[[j]]
            shiny::div(
              style = span(length(sg$levels)), class = "rp-hdr-cell",
              shiny::textAreaInput(hid(l$uid, paste0("seg", j)), NULL,
                                   if (is.na(sg$text %||% NA)) "" else sg$text,
                                   width = "100%", rows = 1),
              shiny::div(
                class = "rp-hdr-cell-tools",
                if (length(sg$levels) > 1L)
                  act(i, "split", "\u00d7", j, t("One cell a column again")),
                if (j < length(l$segments))
                  act(i, "merge", "\u21e5", j, t("One cell with the next column"))))
          }),
          list(shiny::div(
            style = span(nv), class = "rp-hdr-cell",
            shiny::textAreaInput(hid(l$uid, "text"), NULL,
                                 if (is.na(l$text)) "" else l$text, rows = 1, width = "100%"),
            shiny::div(class = "rp-hdr-cap", cap(l)))))
        tools <- shiny::div(
          class = "rp-hdr-tools",
          shiny::strong(sprintf(t("Line %d"), i)),
          shiny::tags$select(
            class = "form-select form-select-sm", style = "width: auto;",
            onchange = sprintf("Shiny.setInputValue('%s', {i: %d, act: 'mode', v: this.value, t: Date.now()}, {priority: 'event'})",
                               bid("hdr_act"), i),
            lapply(seq_along(modes), function(k) shiny::tags$option(
              value = modes[[k]], selected = if (identical(modes[[k]], l$mode)) NA,
              names(modes)[k]))),
          if (identical(l$mode, "key"))
            shiny::selectInput(hid(l$uid, "key"), NULL, keys, width = "8rem",
                               selected = if (!is.na(l$key)) l$key),
          shiny::span(t("Align")),
          shiny::selectInput(hid(l$uid, "align"), NULL, width = "9rem",
                             stats::setNames(c("", "left", "center", "right"),
                                             c(t("default (center)"), t("left"), t("center"), t("right"))),
                             selected = if (is.na(l$align)) "" else l$align),
          shiny::checkboxInput(hid(l$uid, "bold"), t("bold"), isTRUE(as.logical(l$bold))),
          shiny::checkboxInput(hid(l$uid, "ul"), t("underline"),
                               !is.na(l$border_bottom) && l$border_bottom != "none"),
          shiny::span(class = "ms-auto",
                      act(i, "up", "\u2191"), act(i, "down", "\u2193"), act(i, "del", "\u00d7")))
        c(list(tools), stub_ui, val_ui)
      }
      grid <- shiny::div(
        class = "rp-b-hdr rp-hgrid",
        style = sprintf("grid-template-columns: repeat(%d, minmax(7rem, 1.3fr)) repeat(%d, minmax(4.5rem, 1fr));",
                        ns, nv),
        lapply(stub_cols, function(cn) shiny::div(class = "rp-hcol", cn)),
        if (length(lv)) lapply(lv, function(v) shiny::div(class = "rp-hcol", v)) else
          shiny::div(class = "rp-hcol", t("the value columns")),
        unlist(lapply(seq_along(lines), header_line), recursive = FALSE))
      shiny::tagList(
        head,
        shiny::div(class = "rp-hgrid-wrap", grid),
        shiny::uiOutput(bid("hdr_tok")),
        if (header_uses_n(lines)) shiny::div(
          class = "rp-hdr-box",
          shiny::div(class = "rp-hdr-box-title",
                     builder_label("What the header's {n} counts", "values", "arg", lang)),
          shiny::uiOutput(bid("hdr_n_ui"))),
        shiny::tags$script(shiny::HTML(paste0(
          "if (!window.tflHdrInsert) {",
          " document.addEventListener('focusin', function(e) {",
          "  if (e.target.closest && e.target.closest('.rp-b-hdr') &&",
          "      /^(TEXTAREA|INPUT)$/.test(e.target.tagName) && e.target.type !== 'radio' &&",
          "      e.target.type !== 'checkbox') {",
          "   if (window.tflHdrField) window.tflHdrField.classList.remove('border-primary');",
          "   window.tflHdrField = e.target; e.target.classList.add('border-primary'); } });",
          " window.tflHdrInsert = function(tk) {",
          "  var f = window.tflHdrField; if (!f) return;",
          "  var a = f.selectionStart || 0, b = f.selectionEnd || 0;",
          "  f.value = f.value.slice(0, a) + tk + f.value.slice(b);",
          "  f.focus(); f.selectionStart = f.selectionEnd = a + tk.length;",
          "  $(f).trigger('change'); f.dispatchEvent(new Event('input', {bubbles: true})); };",
          "}"))))
    })
  }))
  # the insert chips, with what each token holds in the preview (drawn on
  # their own: the lines' fields are not drawn again while one types)
  shiny::observe(builder_guard({
    hdr_ver()
    n <- bform$n
    output[[paste0("b", n, "_hdr_tok")]] <- shiny::renderUI({
      keys <- input[[bid("key")]] %||% bform$st$key
      toks <- header_token_choices(keys, input[[bid("hdr_n")]] %||% bform$hdr_n)
      pv <- tryCatch(preview_now(), error = function(e) NULL)
      toks <- header_token_labels(toks, attr(pv$pages, "header_tokens"))
      chip <- function(k) shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-secondary py-0 me-1 mb-1",
        names(toks)[k], onclick = sprintf("tflHdrInsert('%s')", toks[[k]]))
      shiny::div(class = "rp-hdr-box",
                 shiny::div(class = "rp-hdr-box-title", t("Insert (into the field last clicked)")),
                 shiny::div(class = "small", lapply(seq_along(toks), chip)))
    })
    # what {n} counts: the populations the ARD states (the preview's), each
    # with its values; the choice kept while the preview is made again
    output[[paste0("b", n, "_hdr_n_ui")]] <- shiny::renderUI({
      keys <- input[[bid("key")]] %||% bform$st$key
      pv <- tryCatch(preview_now(), error = function(e) NULL)
      cur <- shiny::isolate(input[[bid("hdr_n")]])
      now <- if (is.null(cur)) bform$hdr_n else if (nzchar(cur)) cur else NA_character_
      ch <- header_n_choices(attr(pv$pages, "n_candidates"), keys, now, tr = t)
      shiny::tagList(
        shiny::radioButtons(bid("hdr_n"), NULL, choiceNames = unname(as.list(names(ch$choices))),
                            choiceValues = unname(as.list(ch$choices)),
                            selected = ch$selected %||% character(0)),
        if (!is.null(ch$warn)) shiny::div(class = "alert alert-warning py-1 px-2 my-1 small", ch$warn),
        if (!is.null(ch$note)) shiny::div(class = "small text-muted", ch$note))
    })
  }))
  # moving, removing, adding a line; a preset into the lines
  shiny::observeEvent({
    bform_drawn()
    input[[bid("hdr_act")]]
  }, {
    a <- input[[bid("hdr_act")]]
    lines <- hdr_now()
    i <- as.integer(a$i)
    shiny::req(!is.null(lines), i >= 1L, i <= length(lines))
    j <- switch(a$act, up = i - 1L, down = i + 1L, NA_integer_)
    no_text <- function(x) is.null(x) || !length(x) || is.na(x[1L]) || !nzchar(x[1L])
    l <- lines[[i]]
    lv <- hdr_levels()
    if (identical(a$act, "del")) {
      lines <- lines[-i]
    } else if (identical(a$act, "mode")) {
      to <- a$v
      if (identical(to, "cells") && !identical(l$mode, "cells")) {
        # cell by cell from what the line said
        l$segments <- switch(l$mode,
          each = lapply(lv, function(v) list(levels = v, text = l$text)),
          all = list(list(levels = lv, text = l$text)),
          lapply(lv, function(v) list(levels = v, text = NA_character_)))
        l$key <- (input[[bid("key")]] %||% bform$st$key)[1L]
      } else if (identical(l$mode, "cells") && !identical(to, "cells")) {
        txt <- Filter(function(x) !no_text(x), lapply(l$segments, `[[`, "text"))
        l$text <- if (length(txt)) txt[[1L]] else NA_character_
        l$segments <- NULL
      }
      if (identical(to, "key") && no_text(l$key)) {
        l$key <- (input[[bid("key")]] %||% bform$st$key)[1L]
      }
      l$mode <- to
      lines[[i]] <- l
    } else if (identical(a$act, "merge")) {
      k <- as.integer(a$j)
      sg <- l$segments
      shiny::req(k >= 1L, k < length(sg))
      sg[[k]]$levels <- c(sg[[k]]$levels, sg[[k + 1L]]$levels)
      if (no_text(sg[[k]]$text)) sg[[k]]$text <- sg[[k + 1L]]$text
      l$segments <- sg[-(k + 1L)]
      lines[[i]] <- l
    } else if (identical(a$act, "split")) {
      k <- as.integer(a$j)
      sg <- l$segments
      shiny::req(k >= 1L, k <= length(sg))
      one <- lapply(seq_along(sg[[k]]$levels), function(m)
        list(levels = sg[[k]]$levels[m], text = if (m == 1L) sg[[k]]$text else NA_character_))
      l$segments <- c(sg[seq_len(k - 1L)], one, sg[-seq_len(k)])
      lines[[i]] <- l
    } else if (!is.na(j) && j >= 1L && j <= length(lines)) {
      lines[c(i, j)] <- lines[c(j, i)]
    }
    bform$hdr <- hdr_uid(lines)
    hdr_ver(hdr_ver() + 1L)
  })
  shiny::observeEvent({
    bform_drawn()
    input[[bid("hdr_add")]]
  }, {
    lines <- hdr_now() %||% list()
    st <- if (length(lines)) lines[[1L]]$stub else c(row_label = NA_character_)
    st[] <- NA_character_
    new <- list(line = 0L, stub = st, merge = FALSE, mode = "key",
                key = (input[[bid("key")]] %||% bform$st$key)[1L],
                text = NA_character_, align = NA_character_,
                bold = NA_character_, border_bottom = "single")
    bform$hdr <- hdr_uid(c(list(new), lines))
    hdr_ver(hdr_ver() + 1L)
  })
  shiny::observeEvent({
    bform_drawn()
    input[[bid("hdr_load")]]
  }, {
    keys <- input[[bid("key")]] %||% bform$st$key
    hp <- .header_preset_choices(length(bform$meta$hierarchy) > 0L,
                                 .key_label(bform$meta, keys[1L]))
    shiny::showModal(shiny::modalDialog(
      title = t("Load a standard column header"),
      shiny::p(class = "small text-muted",
               t("The lines of the column header become the chosen one's; what they say now is replaced.")),
      shiny::radioButtons(
        bid("hdr_preset"), NULL, selected = character(0),
        choiceNames = lapply(seq_along(hp), function(k) shiny::span(
          names(hp)[k],
          shiny::span(class = "rp-hdr-preset-sample", header_preset_sample(header_presets()[[hp[[k]]]])))),
        choiceValues = unname(as.list(hp))),
      footer = shiny::tagList(
        .btn(bid("hdr_preset_no"), t("Cancel"), class = "btn-secondary"),
        .btn(bid("hdr_preset_ok"), t("Replace"), class = "btn-primary"))))
  })
  shiny::observeEvent({
    bform_drawn()
    input[[bid("hdr_preset_no")]]
  }, {
    shiny::removeModal()
  })
  shiny::observeEvent({
    bform_drawn()
    input[[bid("hdr_preset_ok")]]
  }, {
    pr <- input[[bid("hdr_preset")]]
    shiny::req(length(pr) == 1L, nzchar(pr), pr %in% names(header_presets()))
    shiny::removeModal()
    keys <- input[[bid("key")]] %||% bform$st$key
    bform$hdr <- hdr_uid(header_read(header_presets()[[pr]], keys))
    hdr_ver(hdr_ver() + 1L)
    # named as the list showed it (the key's name for "Arm")
    hp <- .header_preset_choices(length(bform$meta$hierarchy) > 0L,
                                 .key_label(bform$meta, keys[1L]))
    shown <- names(hp)[match(pr, hp)]
    notify(sprintf(t("The header is the preset %s now."), if (is.na(shown)) pr else shown))
  })
  bstate <- shiny::reactive(builder_guard({
    bform_drawn()
    shiny::req(builder_case() %in% c("ok", "hierarchy"),
               identical(bform$id, current()))
    st <- bform$st
    get <- function(x) input[[bid(x)]]
    hier <- length(st$hierarchy) > 0L
    # (the block's order: plan_levels()' card's, while it is there)
    vars <- if (hier) st$variables$variable else get("vars") %||% st$variables$variable
    # the column variables in the order dragged (outermost first)
    ks <- get("key")
    # (a template report has no table_plan() card: its columns as read)
    if (is.null(ks) && isTRUE(bform$tpl)) ks <- st$key
    shiny::req(length(ks) > 0L, setequal(vars, st$variables$variable))
    arms <- stats::setNames(lapply(ks, function(k)
      input[[arms_id(k)]] %||% key_levels(k)), ks)
    v <- st$variables[match(vars, st$variables$variable), , drop = FALSE]
    idx <- match(vars, st$variables$variable)
    v$label <- vapply(seq_along(vars), function(k) {
      l <- get(paste0("lab", idx[k]))
      if (is.null(l)) as.character(v$label[k]) else if (nzchar(trimws(l))) l else
        NA_character_
    }, "")
    lev <- stats::setNames(lapply(seq_along(vars), function(k)
      get(paste0("lv", idx[k])) %||% st$levels[[vars[k]]]), vars)
    v$under <- vapply(seq_along(vars), function(k) {
      u <- get(paste0("under", idx[k]))
      if (is.null(u)) as.character(v$under[k] %||% NA_character_) else
        if (nzchar(u)) u else NA_character_
    }, "")
    num <- function(x, d) {
      x <- suppressWarnings(as.numeric(get(x)))
      if (length(x) != 1L || is.na(x)) d else max(0, round(x))
    }
    rows <- get("rows") %||% st$rows
    tpl <- builder_templates(list(templates = c(as.list(st$templates),
                                                bform$own_tpl %||% list()),
                                  rows = st$rows))
    value <- get("value") %||% st$value
    used <- unique(unlist(lapply(tpl[intersect(rows, names(tpl))], .template_stats)))
    digits <- st$digits[intersect(names(st$digits), used)]
    # (a statistic the table has no decimals of shows the study's rule, or
    # 1 (output_digits_ui()): that number, sent back as drawn, is no value
    # of the table's own -- written, the study would change unasked)
    rule <- .report_digits(shiny::isolate(rv$p), bform$id)
    for (k in used) {
      dv <- suppressWarnings(as.numeric(get(paste0("dg_", k))))
      if (length(dv) != 1L || is.na(dv)) next
      dv <- as.integer(max(0, round(dv)))
      if (!k %in% names(digits) && identical(dv, as.integer(.or_na(unname(rule[k]), 1L)))) next
      digits[[k]] <- dv
    }
    digits <- digits[intersect(used, names(digits))]
    if (identical(digits, st$digits[names(digits)]) &&
        setequal(names(digits), names(st$digits))) digits <- st$digits
    exc_ver()
    list(key = ks, arms = arms, variables = v, levels = lev,
         rows = rows,
         templates = tpl[intersect(rows, names(tpl))],
         value = value, digits = digits,
         exceptions = bform$exc %||% st$exceptions,
         cat_format = get("cat") %||% st$cat_format,
         cat_own = {
           # read only while chosen: the field is filled in advance
           o <- if (identical(get("cat"), "own")) get("cat_own")
           if (is.null(o)) st$cat_own %||% NA_character_ else
             if (nzchar(trimws(o))) trimws(o) else NA_character_
         },
         pct_decimals = num("pct", st$pct_decimals),
         header = {
           hdr_ver()
           l <- hdr_now()
           if (is.null(l)) "keep" else header_write(l)
         },
         header_n = if (!is.null(get("hdr_n"))) {
           v <- get("hdr_n")
           if (nzchar(v)) v else NA_character_
         },
         auto_levels = st$auto_levels,
         hierarchy = st$hierarchy,
         row_keys = if (hier) {
           rk <- get("layers")
           # the hierarchy's levels are fixed (the page refuses to remove
           # them): its groups outermost, then its levels in their order
           if (is.null(rk) || !length(rk) || !all(st$hierarchy %in% rk)) st$row_keys else
             c(setdiff(rk, st$hierarchy), st$hierarchy)
         },
         row_choices = st$row_choices,
         # a summary table's row layers in their order (a hierarchy's are
         # its row keys); the block of variables among them, or not
         layers = {
           ly <- get("layers")
           if (hier || is.null(ly) || !length(ly)) st$layers else unique(as.character(ly))
         },
         layer_choices = st$layer_choices,
         # ... and its rows' text (.label, a key, "NA" none)
         row_label = {
           rl <- if (!hier) get("rowlabel")
           if (is.null(rl) || !nzchar(rl)) st$row_label else if (identical(rl, "NA")) NA_character_ else rl
         },
         row_pages = if (hier) {
           pg <- get("rowpages")
           if (is.null(pg)) st$row_pages else isTRUE(pg)
         },
         sort = if (hier) {
           if (identical(st$sort, "own")) "own" else {
             # (another key: its text, "=" before it; none yet, the order
             # as it was)
             lv <- vapply(seq_along(st$row_keys), function(i) {
               k <- get(paste0("sort", i)) %||% st$sort[i]
               if (!identical(k, "=")) return(k)
               own <- trimws(get(paste0("sort_own", i)) %||% "")
               if (nzchar(own)) paste0("=", own) else st$sort[i]
             }, "")
             attr(lv, "nocol") <- attr(st$sort, "nocol")
             # (levels(<key>) first: on the levels checked; none, no
             # attribute -- as the sheets' order read back)
             was_first <- attr(st$sort, "first") %||% logical(length(lv))
             first <- vapply(seq_along(lv), function(i)
               isTRUE(get(paste0("sort_first", i)) %||% was_first[i]), NA)
             if (any(first)) attr(lv, "first") <- first
             cc <- get("sort_col")
             attr(lv, "col") <- if (is.null(cc)) attr(st$sort, "col") %||% NA_character_ else
               if (nzchar(cc)) cc else NA_character_
             lv
           }
         })
  }))
  bstate_d <- shiny::debounce(bstate, 250)
  # the keys plan_sort() gets from the order chosen (under its fields)
  output$b_sort_code <- shiny::renderText({
    st <- bstate()
    shiny::req(length(st$hierarchy) > 0L, !identical(st$sort, "own"))
    .hier_sort(st$sort, attr(st$sort, "col") %||% NA_character_,
               hier = st$row_keys %in% st$hierarchy,
               nocol = attr(st$sort, "nocol") %||% character())
  })
  shiny::observeEvent(bstate_d(), {
    st <- bstate_d()
    id <- bform$id
    # (a hierarchy's rows changed, or its pages: drawn again -- the
    # plan_paginate_group() card's fields say what was written)
    rows_new <- length(st$hierarchy) > 0L &&
      (!identical(st$row_keys, bform$last$row_keys) ||
         (!is.null(bform$last) && !identical(st$row_pages, bform$last$row_pages)))
    # the key chosen as the rows' text is no row layer: taken out of them
    # (as builder_write() writes them)
    lk <- st$row_label
    if (!length(st$hierarchy) && length(lk) == 1L && !is.na(lk) && lk %in% st$layers &&
        length(st$layers) > 1L) {
      st$layers <- setdiff(st$layers, lk)
      shiny::updateSelectizeInput(session, bid("layers"), selected = st$layers)
    }
    # (a template report: the form as first drawn is the template's, not
    # an edit -- only what is changed after it is written)
    if (is.null(bform$last) && !is.null(.tpl_of(rv$p, id %||% ""))) {
      bform$last <- st
      return()
    }
    p2 <- guarded(builder_write(rv$p, id, st, was = bform$last))
    if (isTRUE(getOption("tflplanner.tpl_trace"))) {
      message(sprintf("[form] %s pct %s -> %s, cat %s -> %s, written %s", id,
                      format(bform$last$pct_decimals), format(st$pct_decimals),
                      format(bform$last$cat_format), format(st$cat_format),
                      !is.null(p2) && !identical(p2, rv$p)))
    }
    if (!is.null(p2)) bform$last <- st
    if (!is.null(p2) && isTRUE(rows_new)) {
      rv$p <- p2
      rv$btouched <- TRUE
      rv$bver <- rv$bver + 1L
      return()
    }
    if (!is.null(p2) && !identical(p2, rv$p)) {
      session$sendCustomMessage("builder-updating", TRUE)
      rv$p <- p2
      rv$btouched <- TRUE
      refresh_marks(p2)
    }
  })
  shiny::observe(builder_guard({
    st <- bstate()
    m <- bform$meta
    n <- bform$n
    # the statistics the table prints: a continuous variable's rows, a
    # categorical one's template -- each only when the table has one
    kinds <- st$variables$kind
    tpl <- c(if (any(kinds %in% "continuous")) unlist(st$templates),
             if (any(kinds %in% "categorical")) .cat_template(st$cat_format, st$pct_decimals))
    miss <- if (!is.null(m) && length(tpl)) .missing_stats(tpl, m$stats) else character()
    # the decimals' fields: drawn again only when the statistics change (a
    # number typed is not drawn over), the variables' own when they change
    rows_now <- input[[paste0("b", n, "_rows")]] %||% st$rows
    tpl_now <- builder_templates(bform$st)
    used_now <- unique(unlist(lapply(tpl_now[intersect(rows_now, names(tpl_now))],
                                     .template_stats)))
    key_now <- paste(n, paste(used_now, collapse = "|"), exc_ver())
    if (!identical(bform$dg_key, key_now)) {
      bform$dg_key <- key_now
      output[[paste0("b", n, "_digits_ui")]] <- shiny::renderUI(
        output_digits_ui(n, bform$st, rows_now))
      output[[paste0("b", n, "_exc_ui")]] <- shiny::renderUI(
        builder_exc_ui(n, bform$st, rows_now))
    }
    output[[paste0("b", n, "_warn")]] <- shiny::renderUI(
      if (length(miss)) shiny::div(
        class = "alert alert-warning py-1 small",
        sprintf(t("The ARD has no %s: those cells stay empty. Add them to the ARD code."),
                paste(miss, collapse = ", "))))
  }))
  # What the report's ARD holds (#343): folded to one line above the
  # right-hand tabs, a small table when opened, the whole normalized ARD on
  # a button (read only then).  From what "read the ARD" saved (meta_of()).
  output$builder_ard_card <- shiny::renderUI({
    id <- current()
    shiny::req(!is.null(id), identical(report_kind(), "table"))
    m <- meta_of(id)
    pb <- sheet_rows(rv$p, "layout", id)$pages_page_by
    pb <- .split_list(pb[!is.na(pb)][1L] %||% NA_character_)
    card <- ard_card_summary(m, page_by = pb, tr = t)
    # Update preview: needed only when the ARD is missing or outdated
    # (its mark), the cards' changes are shown without it
    btn <- shiny::div(class = "d-flex align-items-center gap-1 mb-1 rp-pv-bar",
                      preview_button("b_preview", id, "btn-sm btn-outline-primary py-0"),
                      help_tip(t(.preview_tip)))
    if (is.null(card$rows)) {
      return(shiny::tagList(btn, shiny::div(class = "rp-ard-card text-muted", card$line)))
    }
    r <- card$rows
    shiny::tagList(btn, shiny::tags$details(
      class = "rp-ard-card",
      shiny::tags$summary(card$line),
      shiny::tags$table(
        shiny::tags$thead(shiny::tags$tr(lapply(
          t(c("Role", "Variable", "Kind", "Levels / statistics", "Label")), shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(r)), function(i) shiny::tags$tr(
          shiny::tags$td(r$role[i]), shiny::tags$td(shiny::tags$code(r$variable[i])),
          shiny::tags$td(r$kind[i]), shiny::tags$td(r$detail[i]),
          shiny::tags$td(if (is.na(r$label[i])) "" else r$label[i]))))),
      .btn("ard_card_all", t("Show the whole normalized ARD"),
           class = "btn-sm btn-outline-secondary mb-1")))
  })
  shiny::observeEvent(input$b_preview, do_preview())
  shiny::observeEvent(input$ard_card_all, {
    id <- current()
    shiny::req(!is.null(id))
    d <- tryCatch(ard_data(rv$study, id), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) {
      notify(t("This report's ARD has not been read yet."))
      return()
    }
    m <- meta_of(id)
    main <- intersect(c(m$by, m$hierarchy, "variable", "variable_level", "context",
                        "stat_name", "stat_fmt", "stat"), names(d))
    show <- function(all) {
      x <- as.data.frame(d)[if (isTRUE(all)) names(d) else main]
      x[] <- lapply(x, function(v) {
        if (is.list(v)) v <- vapply(v, function(z) paste(format(z), collapse = ", "), "")
        if (is.numeric(v)) signif(v, 4) else v
      })
      DT::datatable(x, rownames = FALSE, filter = "top", selection = "none",
                    options = list(pageLength = 25, scrollX = TRUE, dom = "tip"))
    }
    output$ard_card_dt <- DT::renderDT(show(input$ard_card_allcols), server = TRUE)
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("The normalized ARD: %s (%d rows)"), id, nrow(d)),
      shiny::checkboxInput("ard_card_allcols", sprintf(t("All the columns (%d)"), ncol(d)), FALSE),
      DT::DTOutput("ard_card_dt"),
      size = "xl", easyClose = TRUE, footer = shiny::modalButton(t("Close"))))
  })

  # The table as it will print.  Made again only when what it is made from
  # changes -- the report's rows of the table sheets, the study's rounding,
  # its ARD rows -- so going back to a report, or a change elsewhere in the
  # definition, does not make it again (it took about half a second).
  pv_cache <- new.env(parent = emptyenv())
  preview_cached <- function(p, id, d) {
    from <- list(lapply(p$sheets[table_sheets()], function(s)
      s[is.na(s$output_id) | s$output_id == id, , drop = FALSE]),
      p$study["rounding"], d)
    hit <- pv_cache[[id]]
    if (!is.null(hit) && identical(hit$from, from)) return(hit$pages)
    pages <- preview_pages(p, id, d)
    pv_cache[[id]] <- list(from = from, pages = pages)
    pages
  }
  # the table's data: its ARD as read, normalized here with
  # normalize_ard()'s arguments as they are now (#428: a change of the
  # overall row, the hierarchy, the stack shows without making the ARD
  # again); a rework of the report's own: the data as it made them
  norm_cache <- new.env(parent = emptyenv())
  preview_data <- function(p, id) {
    raw <- if (.table_norm_builtin(p, id)) ard_raw(rv$study, id)
    if (is.null(raw)) return(ard_data(rv$study, id))
    its <- Filter(function(i) identical(i$fn, "normalize_ard"), builder_items(p, id))
    key <- list(raw, if (length(its)) its[[1L]]$args)
    hit <- norm_cache[[id]]
    if (!is.null(hit) && identical(hit$key, key)) return(hit$d)
    d <- .preview_normalized(p, id, raw)
    norm_cache[[id]] <- list(key = key, d = d)
    d
  }
  preview <- shiny::reactive({
    id <- current()
    shiny::req(!is.null(id), identical(report_info(rv$p, id)$type, "table"))
    rv$ard_ver
    d <- tryCatch(preview_data(rv$p, id), error = function(e) e)
    if (inherits(d, "error")) {
      return(list(id = id, error = paste(t("normalize_ard() stopped:"), conditionMessage(d))))
    }
    if (is.null(d)) return(list(id = id, error = t("No data to show yet: Update preview makes this table's ARD.")))
    tryCatch(list(id = id, pages = preview_cached(rv$p, id, d)),
             error = function(e) list(id = id, error = conditionMessage(e)))
  })
  # a change typed in the builder: drawn once the typing stops; another
  # report: drawn at once (the waiting is for the typing, and a debounced
  # value would still be the report before)
  preview_d <- shiny::debounce(preview, 100)
  preview_now <- shiny::reactive({
    id <- current()
    pv <- preview_d()
    if (!identical(pv$id, id)) pv <- preview()
    pv
  })
  # the first page as the report's rows and the study defaults make it:
  # header, titles, the body's start, footnotes, footer
  shiny::observeEvent(input$page_full, {
    shiny::showModal(shiny::modalDialog(
      title = t("First page (sample)"), size = "xl", easyClose = TRUE,
      shiny::div(class = "rp-page-full", shiny::uiOutput("page_sample_full")),
      footer = shiny::modalButton(t("Close"))))
  })
  output$page_sample_full <- shiny::renderUI(page_sample_ui())
  # SPEC (read only): this report's own rows of the page's sheets, as they
  # are written to the report workbook; the study defaults fill the rest
  output$page_spec_view <- shiny::renderUI({
    id <- current()
    if (is.null(id)) return(NULL)
    rv$ver
    p <- rv$p
    sheet_table <- function(sh) {
      d <- sheet_rows(p, sh, id)
      d$output_id <- NULL
      d <- d[, vapply(d, function(v) any(!is.na(v) & nzchar(as.character(v))), NA), drop = FALSE]
      if (!nrow(d) || !ncol(d)) return(NULL)
      shiny::tagList(
        shiny::h6(class = "mt-2 mb-1", sh),
        shiny::tags$table(
          class = "table table-sm small mb-1",
          shiny::tags$thead(shiny::tags$tr(lapply(names(d), shiny::tags$th))),
          shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i) shiny::tags$tr(
            lapply(d[i, ], function(v) shiny::tags$td(if (is.na(v)) "" else as.character(v))))))))
    }
    # the YAML spec: the report file's page: as it is written (#438)
    rel <- p$spec$where[[id]]
    lines <- if (!is.null(rel)) p$spec$text[[rel]]
    if (length(lines)) {
      at <- which(lines == "page:")
      pg <- if (length(at)) {
        top <- which(grepl("^[A-Za-z_]+:", lines) & seq_along(lines) > at[1L])
        lines[at[1L]:(if (length(top)) top[1L] - 1L else length(lines))]
      }
      return(shiny::tagList(
        shiny::div(class = "small text-muted mb-1", shiny::code(rel), " ",
                   help_tip(t("The report's own page: in its YAML file (edit it on the left). The pattern it extends and the study defaults fill what it leaves out."))),
        if (length(pg)) shiny::tags$pre(class = "small", paste(pg, collapse = "\n")) else
          shiny::p(class = "small text-muted", t("No rows of its own: the study defaults make its page."))))
    }
    out <- Filter(Negate(is.null), lapply(report_sheets(), sheet_table))
    shiny::tagList(
      shiny::p(class = "small text-muted mb-1",
               sprintf(t("%s's own rows, as written to %s (edit them on the left). Study defaults fill what they leave out."),
                       id, file.path(study_layout()[["spec"]], .report_file))),
      if (length(out)) out else
        shiny::p(class = "small text-muted", t("No rows of its own: the study defaults make its page.")))
  })
  output$page_sample <- shiny::renderUI(page_sample_ui())
  page_sample_ui <- function() {
    id <- current()
    if (is.null(id)) {
      return(shiny::p(class = "small text-muted",
                      t("Choose a report above, or in the list on the left (> opens it).")))
    }
    rv$ver
    p <- rv$p
    info <- report_info(p, id)
    body <- if (identical(info$type, "table")) {
      pv <- tryCatch(preview_now(), error = function(e) NULL)
      if (!is.null(pv$pages)) preview_html(pv$pages[1L], max_pages = 1L) else
        shiny::div(class = "text-muted small", pv$error %||% t("(the table)"))
    } else if (identical(info$type, "figure")) {
      page_sample_figure(p, id)
    } else {
      shiny::div(class = "text-muted small text-center py-4", t("(the listing: see step 2)"))
    }
    .page_sample_html(p, id, rv$meta$study_id %||% "", body,
                      program = info$program)
  }
  .srv_keep(environment())
}
