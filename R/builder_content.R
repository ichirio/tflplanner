# The table builder's content as cards: a table's plan is its items --
# table_plan() and the plan_*() layers, one function call each, in the
# order the plan applies them -- and the builder shows one card an item
# (R/builder_cards.R).  The items are what tflspec makes from the sheets
# (its bridge, .plan_content_from_sheets()): the report's own over the
# study's defaults, each marked where it came from.  The sheets stay the
# planner's storage: a card's edits go through builder_write() (the form)
# or, as R, back through the bridge into the rows of the sheets that verb
# is written from.

# ---- meanings ---------------------------------------------------------------

# What each verb is for, the card's label beside its name, until the
# schema says (`label`); in English, translated by tr()
.plan_meaning <- c(
  normalize_ard = "Normalize the ARD",
  table_plan = "Table structure",
  plan_cells = "Statistics in the cells",
  plan_digits = "Decimals and rounding",
  plan_levels = "Order of the values",
  plan_labels = "Labels",
  plan_nest = "Rows under another value",
  plan_total = "Total column",
  plan_compare = "Comparison columns",
  plan_stat_groups = "Groups of statistics",
  plan_sort = "Row order",
  plan_filter = "Row filter",
  plan_hide = "Hidden columns",
  plan_col_header = "Column header",
  plan_stub = "Row label column",
  plan_blanks = "Blank rows",
  plan_paginate_rows = "Page breaks by rows",
  plan_paginate_group = "A page per group",
  plan_paginate_cols = "Column pages",
  plan_row_group = "Row groups",
  plan_columns = "Column widths and alignment",
  plan_style = "Table style",
  plan_cell_style = "Cell styles")

# ... and their main arguments'
.plan_arg_meaning <- list(
  normalize_ard = c(hierarchy = "Hierarchy", overall = "Overall row"),
  table_plan = c(cols = "Columns", rows = "Row layers", label = "Row text column",
                 stat = "Statistic columns", show_keys = "Keys shown"),
  plan_cells = c(value = "Values printed", stats = "Statistics as", na = "Text for missing",
                 default = "Every cell", continuous = "Continuous variables",
                 categorical = "Categorical variables"),
  plan_digits = c(rounding = "Rounding", .rows = "Decimals by row"),
  plan_levels = c(.drop_empty = "Hide empty values"),
  plan_sort = c(keys = "Order", stat = "Statistic counted"),
  plan_total = c(label = "Heading", position = "Place"),
  plan_filter = c(where = "Condition", any = "In any column", parents = "Parent rows"),
  plan_compare = c(by = "By group", position = "Place"),
  plan_stub = c(vars = "Variables", name = "Column name", indent = "Indent",
                group_summary = "Group summary rows", before = "Before the rows"),
  plan_blanks = c(where = "Where", first = "Before the first", last = "After the last",
                  counted = "Count blank rows"),
  plan_paginate_rows = c(max_rows = "Most rows a page", split = "Split rule",
                         page_by = "Page by", heading = "Heading",
                         cont_label = "Continued label", break_before = "Break before rows",
                         min_group_rows = "Fewest rows of a group on a page"),
  plan_paginate_cols = c(every = "Columns a page", fit = "Fit to the page",
                         at = "Break before columns", keep = "Columns on every page",
                         cut_by = "Cut by", order = "Order of the pages",
                         allow_span_break = "A spanning header may break"),
  plan_paginate_group = c(col = "Column", heading = "Heading"),
  plan_col_header = c(header = "Header lines", values = "Values of the tokens",
                      col_header_align = "Alignment"),
  plan_columns = c(widths = "Widths", decimal = "Decimal alignment", row_title = "Row title",
                   auto_width = "Automatic width", sep = "Separator"),
  plan_style = c(align_count_pct = "Align n (%)"))

# a verb's meaning: the schema's label, else the table above; NULL for none
.verb_meaning <- function(fn) {
  lab <- .schema_of(fn)$label
  if (!is.null(lab)) return(lab)
  if (fn %in% names(.plan_meaning)) .plan_meaning[[fn]] else NULL
}

# an argument's meaning, likewise
.arg_meaning <- function(fn, arg) {
  lab <- .schema_of(fn)$args[[arg]]$label
  if (!is.null(lab)) return(lab)
  m <- .plan_arg_meaning[[fn]]
  if (!is.null(m) && arg %in% names(m)) m[[arg]] else NULL
}

# ---- the items ----------------------------------------------------------------

# a function of tflspec's own (the bridge), not exported by it
.tflspec_fn <- function(name) get(name, envir = asNamespace("tflspec"))

# The items of a report's table as the builder shows them, in the plan's
# order: each a list -- `fn`, `args`, `from` ("own": the report says it;
# "defaults": the study's defaults), `path` (the card's and the code's
# name for it: the verb, "#2" for its second call), `revert` (an item of
# the report's own over one of the defaults: "Revert" removes the own),
# `present` (TRUE; FALSE for a card shown though the plan has no such
# item yet).  `args` are the plan's (the report's over the defaults'),
# `own_args` what the report says itself (NULL: nothing).
builder_items <- function(x, output_id) {
  sp <- .spec_object_last(x, table_sheets(), .study_keys$table)
  bridge <- .tflspec_fn(".plan_content_from_sheets")
  get <- function(...) tryCatch(bridge(sp, ...), error = function(e) list())
  res <- get(output_id, own = FALSE)
  own <- get(output_id, own = TRUE)
  dfl <- get(NA)
  fns <- function(l) vapply(l, function(it) it$fn, "")
  own_fn <- fns(own)
  dfl_fn <- fns(dfl)
  seen <- character()
  lapply(res, function(it) {
    fn <- it$fn
    seen <<- c(seen, fn)
    k <- sum(seen == fn)
    # own: the report says it -- table_plan() is always the report's own
    mine <- fn %in% own_fn || identical(fn, "table_plan")
    at <- which(own_fn == fn)
    list(fn = fn, args = it$args %||% list(),
         own_args = if (length(at) >= k) own[[at[k]]]$args %||% list(),
         from = if (mine) "own" else "defaults",
         path = if (k > 1L) paste0(fn, "#", k) else fn,
         revert = mine && fn %in% dfl_fn && fn %in% names(.verb_targets),
         present = TRUE)
  })
}

# The items with the cards shown though the plan has none yet (`always`:
# the verbs whose cards hold a form that makes the item -- the Total
# column's switch, the column header's lines), each placed after the last
# item that comes before it in the schema's order
.builder_items_shown <- function(items, always = character()) {
  have <- vapply(items, function(it) it$fn, "")
  rank <- function(fn) as.numeric(.schema_of(fn)$rank %||% 500)
  for (fn in setdiff(always, have)) {
    it <- list(fn = fn, args = list(), from = "own", path = fn, revert = FALSE,
               present = FALSE)
    r <- rank(fn)
    have <- vapply(items, function(it) it$fn, "")
    before <- which(vapply(have, rank, 0) <= r)
    at <- if (length(before)) max(before) else 0L
    items <- append(items, list(it), after = at)
  }
  items
}

# The items with the ARD's normalization first (normalize_ard(): what the
# plan is given): its hierarchy -- the ARD analysis's, read only -- and the
# any-event row (tables$overall_label), the table's.  Always written, so
# required; its arguments are not edited as R here (the hierarchy is step
# 1's).
.builder_items_normalize <- function(items, hierarchy = character(), overall = NA_character_) {
  args <- list()
  if (length(hierarchy)) args$hierarchy <- as.character(hierarchy)
  if (length(overall) == 1L && !is.na(overall) && nzchar(overall)) args$overall <- overall
  it <- list(fn = "normalize_ard", args = args, from = "", path = "normalize_ard",
             revert = FALSE, present = TRUE, required = TRUE, fixed = TRUE)
  c(list(it), items)
}

# ---- the cards ------------------------------------------------------------------

# One item's arguments as they are, read only: a line an argument, its
# label "meaning (arg)" and its value as R.  The card of a verb the builder
# has no form for yet (the study's layout verbs ...): what the program says
# is on screen, and "Edit as R" changes it.
.item_args_view <- function(it, lang = "en") {
  a <- it$args
  if (!length(a)) {
    return(shiny::div(class = "small text-muted", tr("No arguments: the defaults of the function.", lang)))
  }
  nm <- names(a) %||% rep("", length(a))
  shiny::tags$dl(
    class = "rp-args-view",
    lapply(seq_along(a), function(i) {
      arg <- if (nzchar(nm[[i]])) nm[[i]] else "..."
      shiny::tagList(
        shiny::tags$dt(builder_label(.arg_meaning(it$fn, arg), arg, "arg", lang)),
        shiny::tags$dd(shiny::tags$code(class = "rp-mono", .arg_to_r(a[[i]]))))
    }))
}

# the summary of a closed card: its arguments as R, cut short
.item_summary <- function(it, width = 70L) {
  s <- tryCatch(args_to_r(it$fn, it$args), error = function(e) "")
  s <- gsub("[[:space:]]+", " ", s)
  if (nchar(s) > width) paste0(substr(s, 1L, width - 1L), "\u2026") else s
}

# The two groups of a table's items, by verb: the mapping (what the ARD's
# numbers become: the structure, the cells, the Total, the decimals, the
# values' order and labels, the rows' order) and the finishing (the column
# header, the layout).  A verb not named is the mapping's.
.plan_sections <- list(
  finish = c("plan_col_header", "plan_stub", "plan_blanks", "plan_paginate_rows",
             "plan_paginate_group", "plan_paginate_cols", "plan_row_group",
             "plan_columns", "plan_style", "plan_cell_style"))

.plan_section_labels <- c(mapping = "Mapping", finish = "Finishing")

.plan_section <- function(fn) {
  for (k in names(.plan_sections)) if (fn %in% .plan_sections[[k]]) return(k)
  "mapping"
}

# the cards seldom changed: closed when drawn (the Layers menu reaches them)
.plan_rare <- c("plan_stub", "plan_blanks", "plan_style", "plan_columns", "plan_cell_style",
                "plan_paginate_rows", "plan_paginate_group", "plan_paginate_cols")

# A card's own line above its body, by verb (`r`: the card's Edit as R is
# offered beside it): what the card does not show yet, or where its values go
.plan_card_note <- function(fn, r = FALSE, lang = "en") {
  t <- function(x) tr(x, lang)
  switch(
    fn,
    plan_digits = shiny::div(class = "rp-card-note",
                             t("The decimals are written into plan_cells()'s formats.")),
    plan_levels = shiny::div(
      class = "rp-card-note",
      t("Values and labels: edited in stage 2; for now, edit them as R."),
      if (r) shiny::tags$button(type = "button",
                                class = "btn btn-sm btn-outline-primary py-0 ms-2 rp-tool-r",
                                t("Edit as R"))),
    plan_sort = shiny::div(
      class = "rp-card-note",
      with_tip(builder_label(.arg_meaning("plan_sort", "keys"), "keys", "arg", lang),
               t(paste("The keys, first to last: .overall the any-event row first;",
                       "group1, group2 ... the rows' outer levels, label the innermost",
                       "(the rows' text), each by name (a code list's order when it has one);",
                       ".depth each group's own row above the rows under it;",
                       "-n most frequent first (-n@group1: by the group's own count).")))),
    NULL)
}

# The cards of a table's content, in its order: one builder_card() an
# item, with a thin line before the first item of each group (Mapping,
# Finishing) -- the content's order is kept, the line only says where a
# group starts.  `bodies`: the forms the builder has for some (by path, or
# by verb); the rest show their arguments.  `id`: the card's input id of a
# path.  table_plan() and plan_cells() are required (and an item marked
# `required`: normalize_ard()); the others have a switch only where
# `switches` names the verb (the Total column: its on / off works), none
# elsewhere for now.  An item of the report's own is marked "This table".
# The seldom changed cards (.plan_rare) are closed when drawn.
.table_cards <- function(items, bodies = list(), id = function(path) path,
                         switches = character(), lang = "en") {
  required <- c("table_plan", "plan_cells")
  started <- character()
  out <- list()
  for (it in items) {
    body <- bodies[[it$path]] %||% bodies[[it$fn]]
    own_form <- !is.null(body)
    if (!own_form) body <- .item_args_view(it, lang)
    req <- it$fn %in% required || isTRUE(it$required)
    sw <- it$fn %in% switches
    # an item the content has not (its form would add it): no switch shown
    # on, no mark of its own -- "not in the code yet"
    absent <- !isTRUE(it$present) && !sw && !req
    tools <- if (it$present && !isTRUE(it$fixed)) "r" else character()
    sec <- .plan_section(it$fn)
    if (!sec %in% started) {
      started <- c(started, sec)
      out[[length(out) + 1L]] <- shiny::div(class = "rp-card-sep", `data-section` = sec,
                                            tr(.plan_section_labels[[sec]], lang))
    }
    rare <- it$fn %in% .plan_rare
    note <- .plan_card_note(it$fn, r = "r" %in% tools, lang = lang)
    out[[length(out) + 1L]] <- builder_card(
      id(it$path), it$fn, .verb_meaning(it$fn), from = it$from, required = req,
      on = it$present || req || !sw,
      switchable = sw, revert = isTRUE(it$revert),
      body = if (is.null(note)) body else shiny::tagList(note, body),
      summary = if (it$present) .item_summary(it),
      tools = tools,
      # the study's defaults the report leaves as they are, and the seldom
      # changed: closed
      open = !rare && (req || own_form || !identical(it$from, "defaults")),
      path = it$path, lang = lang,
      own_label = if (isTRUE(it$present) && identical(it$from, "own")) "This table",
      absent = absent, rare = rare, section = sec,
      also = if (identical(it$fn, "plan_digits")) "plan_cells")
  }
  out
}

# The bar above the cards: the Layers menu -- every card in the content's
# order, by group, each a link that opens it and brings it into view (the
# seldom changed ones faint)
.table_cards_bar <- function(items, id = function(path) path, lang = "en") {
  t <- function(x) tr(x, lang)
  started <- character()
  entries <- list()
  for (it in items) {
    sec <- .plan_section(it$fn)
    if (!sec %in% started) {
      started <- c(started, sec)
      entries[[length(entries) + 1L]] <- shiny::tags$li(
        shiny::tags$h6(class = "dropdown-header", t(.plan_section_labels[[sec]])))
    }
    entries[[length(entries) + 1L]] <- shiny::tags$li(shiny::tags$a(
      href = "#", class = paste("dropdown-item rp-layer-go",
                                if (it$fn %in% .plan_rare) "rp-layer-rare"),
      `data-target` = id(it$path),
      builder_label(.verb_meaning(it$fn), it$fn, "fn", lang)))
  }
  shiny::div(
    class = "rp-cards-bar",
    shiny::div(class = "dropdown",
               shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary dropdown-toggle rp-layers-btn",
                                  `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                                  t("Layer list")),
               shiny::tags$ul(class = "dropdown-menu", entries)))
}

# ---- code line -> card -------------------------------------------------------------

# The lines of a report's program that each item wrote: the plan's pipe
# (`plan <- table_plan(...) |> plan_*() ...`), an item's lines from the one
# its verb starts to the next one's, and the code lists at its head
# (`cl_<variable> <- c(...)`, plan_levels()'s).  The k-th call of a verb is
# its k-th item.  A data frame: `line`, `path`.
builder_code_map <- function(lines, items) {
  lines <- unlist(strsplit(paste(lines, collapse = "\n"), "\n", fixed = TRUE))
  empty <- data.frame(line = integer(), path = character(), stringsAsFactors = FALSE)
  start <- grep("^plan <- ", lines)
  if (!length(start)) return(empty)
  start <- start[1L]
  paths <- vapply(items, function(it) it$path, "")
  fns <- vapply(items, function(it) it$fn, "")
  out_l <- integer()
  out_p <- character()
  seen <- character()
  cur <- NULL
  pending <- integer()
  for (i in start:length(lines)) {
    s <- lines[[i]]
    if (!nzchar(trimws(s))) break
    m <- regmatches(s, regexec("^(plan <- [A-Za-z_.]+ \\|> |plan <- |\\s+)([A-Za-z_.]+)\\(", s))[[1L]]
    fn <- if (length(m)) m[3L] else NA_character_
    if (!is.na(fn) && fn %in% fns) {
      seen <- c(seen, fn)
      k <- sum(seen == fn)
      at <- which(fns == fn)
      cur <- if (k <= length(at)) paths[at[k]] else NULL
      if (!is.null(cur) && length(pending)) {
        out_l <- c(out_l, pending)
        out_p <- c(out_p, rep(cur, length(pending)))
      }
      pending <- integer()
    } else if (grepl("^\\s*#", s)) {
      # a note on the item below it ("# (study default)")
      pending <- c(pending, i)
      next
    }
    if (!is.null(cur)) {
      out_l <- c(out_l, i)
      out_p <- c(out_p, cur)
    }
  }
  # the code lists above the plan: plan_levels()'s
  lev <- paths[fns == "plan_levels"]
  if (length(lev)) {
    cl <- grep("^# the code lists", lines)
    cl <- cl[cl < start]
    if (length(cl)) {
      at <- seq.int(cl[length(cl)], start - 1L)
      at <- at[nzchar(trimws(lines[at]))]
      out_l <- c(out_l, at)
      out_p <- c(out_p, rep(lev[[1L]], length(at)))
    }
  }
  # the ARD's normalization above the plan: normalize_ard()'s card
  if ("normalize_ard" %in% paths) {
    na <- grep("normalize_ard\\(", lines)
    na <- na[na < start]
    if (length(na)) {
      i <- na[length(na)]
      at <- i
      # (its call over several lines: to the line that closes it)
      while (i < start - 1L && !grepl("\\)\\s*$", lines[[i]])) {
        i <- i + 1L
        at <- c(at, i)
      }
      out_l <- c(out_l, at)
      out_p <- c(out_p, rep("normalize_ard", length(at)))
    }
  }
  o <- order(out_l)
  data.frame(line = out_l[o], path = out_p[o], stringsAsFactors = FALSE)
}

# ---- writing an item back ------------------------------------------------------------

# Where each verb is written in the sheets: a sheet with one row a report
# (tables, layout, style) by its columns; a sheet of rows (cells,
# col_header ...) by its rows -- `cols` NULL -- or, the variables sheet, by
# its columns of each variable's row.
.verb_targets <- list(
  table_plan = list(tables = c("cols", "rows", "label", "show_keys")),
  plan_sort = list(tables = c("sort", "sort_stat")),
  plan_total = list(tables = c("total", "total_position")),
  plan_stat_groups = list(tables = "stat_groups"),
  plan_cells = list(tables = c("stats", "value", "na"), cells = NULL),
  plan_filter = list(tables = c("keep_where", "keep_any", "keep_parents")),
  plan_compare = list(tables = c("compare", "compare_by", "compare_position")),
  plan_digits = list(digits = NULL),
  plan_levels = list(variables = c("levels", "empty_levels")),
  plan_labels = list(variables = c("label", "order")),
  plan_nest = list(variables = "under"),
  plan_stub = list(layout = c("stub_vars", "stub_name", "stub_indent", "stub_summary",
                              "stub_before")),
  plan_paginate_group = list(layout = c("group_page", "group_col", "group_keep",
                                        "group_heading")),
  plan_row_group = list(layout = c("group_mode", "group_collapse")),
  plan_blanks = list(layout = c("blank_where", "blank_first", "blank_last",
                                "blank_counted")),
  plan_paginate_rows = list(layout = c("pages_max_rows", "pages_split", "pages_break_before",
                                       "pages_min_group_rows", "pages_cont_label",
                                       "pages_page_by", "pages_heading")),
  plan_paginate_cols = list(layout = c("colpages_every", "colpages_at", "colpages_keep",
                                       "colpages_order", "colpages_cut_by", "colpages_fit",
                                       "colpages_allow_span_break")),
  plan_style = list(style = NA),
  plan_cell_style = list(cell_styles = NULL),
  plan_hide = list(columns = "hide"),
  plan_col_header = list(col_header = NULL, tables = "header_n"),
  plan_columns = list(columns = c("rel_width", "decimal_split", "row_title")))

# the columns of a one-row sheet a verb writes (NA: all but those other
# verbs write there)
.target_cols <- function(sheet, cols, d) {
  if (identical(cols, NA)) {
    others <- unlist(lapply(.verb_targets, function(t) {
      v <- t[[sheet]]
      if (is.character(v)) v
    }), use.names = FALSE)
    cols <- setdiff(names(d), c("output_id", "note", others, "col_header_align", "auto_width"))
  }
  intersect(cols, names(d))
}

# The report's own rows of `sheet` with the columns `cols` taken from
# `new` (its own rows the bridge wrote; NULL or none: the columns cleared).
# Rows: a one-row sheet's row; the variables sheet's by variable.
.put_cols <- function(x, id, sheet, cols, new) {
  d <- sheet_rows(x, sheet, id)
  d$output_id <- NULL
  if (is.null(new)) new <- d[0, , drop = FALSE]
  new <- as.data.frame(new, stringsAsFactors = FALSE)
  new$output_id <- NULL
  cols <- .target_cols(sheet, cols, d)
  if (!length(cols)) return(x)
  val <- function(df, i, c) if (c %in% names(df) && i <= nrow(df)) as.character(df[[c]][i]) else NA_character_
  if (identical(sheet, "variables")) {
    for (c in cols) d[[c]] <- as.character(d[[c]])
    for (c in cols) d[[c]][] <- NA_character_
    for (i in seq_len(nrow(new))) {
      v <- new$variable[i]
      j <- match(v, d$variable)
      if (is.na(j)) {
        d[nrow(d) + 1L, ] <- NA
        j <- nrow(d)
        d$variable[j] <- v
      }
      for (c in cols) d[[c]][j] <- val(new, i, c)
    }
  } else {
    if (!nrow(d)) d[1L, ] <- NA
    for (c in cols) d[[c]][1L] <- val(new, 1L, c)
  }
  set_sheet_rows(x, sheet, id, d)
}

# the report's own rows of a sheet of rows, as the bridge wrote them
.put_rows <- function(x, id, sheet, new) {
  d <- sheet_rows(x, sheet, id)
  d$output_id <- NULL
  if (is.null(new) || !nrow(new)) return(set_sheet_rows(x, sheet, id, d[0, , drop = FALSE]))
  new <- as.data.frame(new, stringsAsFactors = FALSE)
  new$output_id <- NULL
  rows <- d[rep(NA_integer_, nrow(new)), , drop = FALSE]
  for (c in intersect(names(new), names(rows))) rows[[c]] <- as.character(new[[c]])
  rownames(rows) <- NULL
  set_sheet_rows(x, sheet, id, rows)
}

# One item of a table written back to the report's sheets: the report's
# own items with the `k`-th item of `fn` given these arguments (added when
# it has none), written through tflspec's bridge into the sheets that verb
# is written from.  What a sheet cannot hold stops it (nothing is written).
.builder_item_write <- function(x, output_id, fn, args, k = 1L) {
  tg <- .verb_targets[[fn]]
  if (is.null(tg)) stop(sprintf("%s: the sheets have no place for it.", fn), call. = FALSE)
  sp <- .spec_object_last(x, table_sheets(), .study_keys$table)
  own <- .tflspec_fn(".plan_content_from_sheets")(sp, output_id, own = TRUE)
  at <- which(vapply(own, function(it) it$fn, "") == fn)
  if (length(at) >= k) {
    own[[at[k]]]$args <- args
  } else {
    own <- c(own, list(tflspec::tfl_item(fn, args)))
  }
  sh <- .tflspec_fn(".plan_sheets_from_content")(own, output_id)
  lost <- attr(sh, "not_converted")
  lost <- lost[startsWith(lost, paste0(fn, ":"))]
  if (length(lost)) stop(paste(lost, collapse = "; "), call. = FALSE)
  for (sheet in names(tg)) {
    new <- sh[[sheet]]
    cols <- tg[[sheet]]
    x <- if (is.null(cols)) .put_rows(x, output_id, sheet, new) else
      .put_cols(x, output_id, sheet, cols, new)
  }
  x
}

# ... and the report's own rows of that verb taken away: the study's
# defaults apply again
.builder_item_revert <- function(x, output_id, fn) {
  tg <- .verb_targets[[fn]]
  if (is.null(tg)) return(x)
  for (sheet in names(tg)) {
    cols <- tg[[sheet]]
    x <- if (is.null(cols)) .put_rows(x, output_id, sheet, NULL) else
      .put_cols(x, output_id, sheet, cols, NULL)
  }
  x
}

# ---- style -----------------------------------------------------------------------------

.builder_content_css <- "
.rp-args-view { display: grid; grid-template-columns: max-content minmax(0, 1fr);
  gap: .1rem .6rem; margin: 0; font-size: 12.5px; }
.rp-args-view dt { font-weight: normal; }
.rp-args-view dd { margin: 0; overflow-wrap: anywhere; }
.rp-args-view code { color: #374151; background: none; padding: 0; font-size: 12px; }
.rp-card-switch .form-check-input:disabled { cursor: default; }
@media (min-width: 992px) {
  /* SPEC | Code | Result beside the cards stays in view as they scroll: the
     column is the sticky box (the tabs inside it are not), its own scroll */
  .rp-b-right { position: sticky; top: .5rem;
    max-height: calc(100vh - 1rem); overflow-y: auto; }
  .rp-b-right .rp-tabs { position: static; max-height: none; overflow-y: visible; }
}
.rp-back-code { padding: 0 .3rem; font-size: 12px; text-decoration: none; }
.rp-b-sec { margin-top: .5rem; }
.rp-b-levels { display: flex; flex-wrap: wrap; gap: .3rem; align-items: center; }
.rp-b-level { font-family: Consolas, 'Courier New', monospace; font-size: 12px; color: #374151;
  background: #f3f4f6; border: 1px dashed #cbd5e1; border-radius: .3rem; padding: 0 .4rem; }
.rp-b-fixed-note { margin-top: .15rem; }
.rp-b-sec > .rp-b-sec-label { font-size: 12.5px; margin-bottom: .15rem; }
/* the chips of an order (the columns' levels ...): each its whole text,
   onto the next line when narrow -- never cut */
.rp-builder-cards .default-sortable.horizontal .rank-list { flex-wrap: wrap; gap: .25rem; }
.rp-builder-cards .default-sortable.horizontal .rank-list-item { flex: 0 0 auto;
  overflow: visible; white-space: nowrap; max-width: 100%; }
"

# the cards' buttons of the table builder, one input each: Revert, Edit as
# R (the card's path)
.builder_content_js <- "
$(document).on('click', '.rp-builder-cards .rp-revert', function() {
  var p = $(this).closest('.rp-card').data('path');
  Shiny.setInputValue('b_card_revert', {path: String(p), t: Date.now()}, {priority: 'event'});
});
$(document).on('click', '.rp-builder-cards .rp-tool-r', function() {
  var p = $(this).closest('.rp-card').data('path');
  Shiny.setInputValue('b_card_r', {path: String(p), t: Date.now()}, {priority: 'event'});
});
"

# The fixed items of a selectize of layers (a hierarchy's levels, the block
# of variables): neither its x nor Backspace removes them -- selectize's
# onDelete refuses -- and their x is not drawn (.fixed_items_css())
.fixed_items_js <- function(fixed) {
  I(sprintf(
    "function(values) { var fixed = %s; return !values.some(function(v) { return fixed.indexOf(v) >= 0; }); }",
    jsonlite::toJSON(as.character(fixed))))
}

# ... their look: no x, a fixed chip (by the input's id and their values)
.fixed_items_css <- function(id, fixed) {
  if (!length(fixed)) return(NULL)
  sel <- function(v, tail = "") sprintf(
    "#%s ~ .selectize-control .item[data-value=\"%s\"]%s", id, v, tail)
  shiny::tags$style(shiny::HTML(paste0(
    paste(vapply(fixed, sel, "", tail = " .remove"), collapse = ", "),
    " { display: none !important; } ",
    paste(vapply(fixed, sel, ""), collapse = ", "),
    " { background: #f3f4f6 !important; color: #374151 !important; border-style: dashed !important; padding-right: 6px !important; }")))
}

# the item a card's path names (NULL for none)
.builder_item_at <- function(items, path) {
  if (is.null(path)) return(NULL)
  for (it in items) if (identical(it$path, as.character(path))) return(it)
  NULL
}
