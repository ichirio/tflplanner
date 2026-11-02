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
  plan_levels = "Level order",
  plan_labels = "Display names",
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
  plan_cell_style = "Cell styles",
  plan_append = "A part under the table")

# a verb's meaning: the schema's label, else the table above; NULL for none
.verb_meaning <- function(fn) {
  lab <- .schema_of(fn)$label
  if (!is.null(lab)) return(lab)
  if (fn %in% names(.plan_meaning)) .plan_meaning[[fn]] else NULL
}

# an argument's meaning: the schema's label (tflspec::tfl_schema()); NULL
# for none
.arg_meaning <- function(fn, arg) .schema_of(fn)$args[[arg]]$label

# ---- the items ----------------------------------------------------------------

# a function of tflspec's own (the bridge), not exported by it
.tflspec_fn <- function(name) get(name, envir = asNamespace("tflspec"))

# The items of a report's table as the builder shows them, in the plan's
# order, the ARD's normalization (normalize_ard()) first: each a list --
# `fn`, `args`, `from` (where the item comes from: "own" the report says
# it; "defaults" the study's defaults; "pattern:<name>" a pattern), `arg_from`
# (each argument's: the same words), `below` (the item under the report's
# own: what a Revert goes back to; NULL none), `path` (the card's and the
# code's name for it: the verb, "#2" for its second call), `revert` (an
# item of the report's own over one of a layer: "Revert" removes the own),
# `present` (TRUE; FALSE for a card shown though the plan has no such item
# yet).  `args` are the plan's (the report's over its layers'), `own_args`
# what the report says itself (NULL: nothing).  Read from the spec (the
# report's YAML over its layers); from the sheets while they hold edits
# the spec has not taken yet.  normalize_ard() is required, and its
# hierarchy step 1's (`meta`: ard_meta(), for the sheets' way).
builder_items <- function(x, output_id, meta = NULL) {
  sp <- .spec_items(x, output_id)
  items <- if (is.null(sp)) .builder_items_sheets(x, output_id, meta) else {
    own_fn <- vapply(sp$own, function(it) it$fn, "")
    seen <- character()
    lapply(sp$items, function(it) {
      fn <- it$fn
      seen <<- c(seen, fn)
      k <- sum(seen == fn)
      at <- which(own_fn == fn)
      from <- if (identical(fn, "table_plan")) "own" else it$from %||% "own"
      list(fn = fn, args = it$args %||% list(),
           own_args = if (length(at) >= k) sp$own[[at[k]]]$args %||% list(),
           from = from, arg_from = it$arg_from, below = it$below,
           path = if (k > 1L) paste0(fn, "#", k) else fn,
           revert = identical(from, "own") && !is.null(it$below),
           present = TRUE)
    })
  }
  .builder_items_ard_first(items)
}

# ... from the sheets (the bridge): the report's and the defaults' items,
# and normalize_ard() from the ARD's hierarchy (`meta`) and the table's
# any-event row (tables$overall_label)
.builder_items_sheets <- function(x, output_id, meta = NULL) {
  sp <- .spec_object_last(x, table_sheets(), .study_keys$table)
  bridge <- .tflspec_fn(".plan_content_from_sheets")
  # the ARD's definition: no plan_levels the ARD already says (tflspec #358)
  ard <- tryCatch(suppressWarnings(.ard_spec(x$ard)), error = function(e) NULL)
  get <- function(...) tryCatch(bridge(sp, ..., ard_spec = ard), error = function(e) list())
  res <- get(output_id, own = FALSE)
  own <- get(output_id, own = TRUE)
  dfl <- get(NA)
  fns <- function(l) vapply(l, function(it) it$fn, "")
  own_fn <- fns(own)
  dfl_fn <- fns(dfl)
  seen <- character()
  items <- lapply(res, function(it) {
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
         revert = mine && fn %in% dfl_fn,
         present = TRUE)
  })
  if (any(fns(items) == "normalize_ard")) return(items)
  args <- list()
  hier <- meta$hierarchy %||% character()
  if (length(hier)) args$hierarchy <- as.character(hier)
  ov <- .rows_for(x, "tables", output_id)$overall_label[1L] %||% NA_character_
  if (length(hier) && !is.na(ov) && nzchar(ov)) args$overall <- ov
  c(list(list(fn = "normalize_ard", args = args, own_args = if (length(args)) args,
              from = if (length(args)) "own" else "defaults", path = "normalize_ard",
              revert = FALSE, present = TRUE)), items)
}

# The items with the ARD's normalization (normalize_ard(): what the plan
# is given) first -- one, required, and its arguments not edited as R
# here (its hierarchy is step 1's); a content with none has the defaults'
.builder_items_ard_first <- function(items) {
  na <- vapply(items, function(it) identical(it$fn, "normalize_ard"), NA)
  it <- if (any(na)) items[[which(na)[1L]]] else
    list(fn = "normalize_ard", args = list(), from = "defaults", path = "normalize_ard",
         present = TRUE)
  it$required <- TRUE
  it$fixed <- TRUE
  it$revert <- FALSE
  c(list(it), items[!na])
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
  # (in the function's order; the ... where the function has it)
  at <- order(match(ifelse(nzchar(nm), nm, "..."), .fn_formals(it$fn, .schema_of(it$fn)),
                    nomatch = length(a) + 1000L), seq_along(a))
  a <- a[at]
  nm <- nm[at]
  shiny::tags$dl(
    class = "rp-args-view",
    lapply(seq_along(a), function(i) {
      arg <- if (nzchar(nm[[i]])) nm[[i]] else "..."
      shiny::tagList(
        shiny::tags$dt(builder_label(.arg_meaning(it$fn, arg), arg, "arg", lang)),
        shiny::tags$dd(shiny::tags$code(class = "rp-mono", .arg_to_r(a[[i]]))))
    }))
}

# The summary of a closed card: its arguments as "meaning (name) = value",
# each value short (a text bare, a vector's values by commas, a map's
# name: value, what is R as its R), each pair and the whole cut short
.item_summary <- function(it, width = 90L, lang = "en") {
  a <- it$args
  if (!length(a)) return("")
  cut <- function(s, n) if (nchar(s) > n) paste0(substr(s, 1L, n - 1L), "\u2026") else s
  nm <- names(a) %||% rep("", length(a))
  pairs <- vapply(seq_along(a), function(i) {
    arg <- if (nzchar(nm[[i]])) nm[[i]] else "..."
    v <- tryCatch(.summary_value(a[[i]]), error = function(e) "")
    paste0(.summary_label(it$fn, arg, lang), " = ", cut(gsub("[[:space:]]+", " ", v), 32L))
  }, "")
  cut(paste(pairs, collapse = "; "), width)
}

# ... an argument's "meaning (name)" as text (the name alone when it has
# no meaning, or its meaning says the name already: builder_label()'s rule)
.summary_label <- function(fn, arg, lang = "en") {
  m <- .label_text(.arg_meaning(fn, arg), lang)
  if (!is.null(m)) {
    for (tail in c(paste0("(", arg, ")"), paste0("\uff08", arg, "\uff09"))) {
      if (endsWith(m, tail)) m <- trimws(substr(m, 1L, nchar(m) - nchar(tail)))
    }
  }
  if (is.null(m) || !nzchar(m) || identical(m, arg)) arg else paste0(m, " (", arg, ")")
}

# ... a value, short
.summary_value <- function(v) {
  if (is.null(v)) return("NULL")
  if (inherits(v, c("tfl_r", "tfl_call"))) return(.arg_to_r(v))
  if (is.list(v)) {
    n <- names(v) %||% rep("", length(v))
    parts <- vapply(seq_along(v), function(i) {
      x <- .summary_value(v[[i]])
      if (nzchar(n[[i]])) paste0(n[[i]], ": ", x) else x
    }, "")
    return(paste(parts, collapse = ", "))
  }
  if (is.atomic(v)) {
    if (is.factor(v)) v <- as.character(v)
    x <- if (is.character(v)) v else format(v, trim = TRUE)
    if (!is.null(names(v)) && any(nzchar(names(v)))) {
      x <- ifelse(nzchar(names(v)), paste0(names(v), ": ", x), x)
    }
    return(paste(x, collapse = ", "))
  }
  .arg_to_r(v)
}

# The two groups of a table's items, by verb, in the order a person
# designs a table: the mapping (ARD -> table: which variables and
# statistics go to which rows, columns and cells -- the structure, the
# cells, the statistic groups, the Total, the comparisons, the row filter)
# and the finishing (the content and the look: the values' order, the
# labels, the nesting, the decimals, the row order, the column header, the
# layout).  normalize_ard() is neither: the ARD made ready for the plan,
# first, with no line of its own.  The schema's `stage` (mapping /
# finishing) says it when tflspec has it; else this table.
.plan_sections <- list(
  mapping = c("table_plan", "plan_cells", "plan_stat_groups", "plan_total", "plan_compare",
              "plan_filter", "plan_append"),
  finish = c("plan_levels", "plan_labels", "plan_nest", "plan_digits", "plan_sort",
             "plan_col_header", "plan_stub", "plan_blanks", "plan_paginate_rows",
             "plan_paginate_group", "plan_paginate_cols", "plan_row_group", "plan_hide",
             "plan_columns", "plan_style", "plan_cell_style"))

# (the words are the strings' (inst/i18n): swapped there)
.plan_section_labels <- c(mapping = "Table skeleton", finish = "Finishing", ard = "ARD")

# each group's line: what it is for, in a line
.plan_section_notes <- c(
  mapping = "Which ARD values go to which rows, columns and cells",
  finish = "Labels, order, decimals, pages: the look")

.plan_section <- function(fn) {
  if (identical(fn, "normalize_ard")) return("ard")
  st <- .schema_of(fn)$stage
  if (identical(st, "mapping")) return("mapping")
  if (identical(st, "finishing")) return("finish")
  for (k in names(.plan_sections)) if (fn %in% .plan_sections[[k]]) return(k)
  "finish"
}

# the cards seldom changed: closed when drawn (the cards' menu reaches them)
.plan_rare <- c("plan_stub", "plan_blanks", "plan_style", "plan_columns", "plan_cell_style",
                "plan_paginate_rows", "plan_paginate_group", "plan_paginate_cols")
