# The table builder's content, part 3: the forms of the layout cards and the
# plan_columns(), plan_digits(), plan_levels(), plan_filter() cards.
# Split from R/builder_content.R (no code change).

# ---- the forms of the layout cards -------------------------------------------------

# The cards with a form of fields by the schema's types (arg_field()): the
# arguments the schema marks `main` on the card, the rest folded under
# "Other arguments"
.plan_form_fns <- c("normalize_ard", "plan_filter", "plan_blanks", "plan_paginate_rows",
                    "plan_paginate_cols", "plan_paginate_group", "plan_row_group", "plan_stub",
                    "plan_columns", "plan_style", "plan_hide", "plan_cell_style")

# a form card's main arguments: those its schema marks `main`
.plan_form_main <- function(fn) {
  a <- .schema_of(fn)$args %||% list()
  m <- names(a)[vapply(a, function(x) isTRUE(x$main), NA)]
  # (in the function's order, as its fields are drawn)
  fo <- tryCatch(names(formals(utils::getFromNamespace(fn, "rtfreporter"))),
                 error = function(e) character())
  .arg_order(m, fo)
}

# the types the form uses over the schema's: a choice of the usual values,
# or R; "widths" a number a column (.widths_field()); a colour a picker
# (no R or map of its own on the card).  plan_cell_style()'s looks are a
# choice of TRUE / FALSE with a blank (left alone: not written), not a
# check (an unticked check would say FALSE).
.plan_cell_look <- "enum[TRUE|FALSE]|logical"
.plan_form_types <- list(
  normalize_ard = c(drop_contexts = "r", stack = "r"),
  plan_cell_style = c(bold = .plan_cell_look, italic = .plan_cell_look,
                      underline = .plan_cell_look, color = "colour", background = "colour"),
  plan_columns = c(widths = "widths", row_title = "list<column>"))

# the arguments of a form card given as R (no field of their own)
.plan_form_r <- list(
  plan_filter = "by",
  plan_cell_style = "border",
  plan_columns = c("cell_format", "column_widths_twips"),
  plan_paginate_cols = "col_header",
  plan_row_group = "group_col")

# a form card's schema: tflspec's, with the types above, its formals the
# function's own (not `plan`) and its visible arguments the main ones
.plan_form_schema <- function(fn) {
  sch <- .schema_of(fn) %||% list()
  for (a in names(.plan_form_types[[fn]])) {
    sch$args[[a]]$type <- .plan_form_types[[fn]][[a]]
  }
  for (a in .plan_form_r[[fn]]) sch$args[[a]]$type <- "r"
  fl <- tryCatch(formals(utils::getFromNamespace(fn, "rtfreporter")), error = function(e) NULL)
  fo <- names(fl)
  # (an argument the schema only names -- a label, no type -- passed by
  # `...`: no field; plan_filter()'s condition is the condition builder's)
  if (length(fo)) {
    typed <- vapply(sch$args, function(x) !is.null(x$type), NA)
    sch$args <- sch$args[typed | names(sch$args) %in% fo]
  }
  # a check's default the function's own (plan_paginate_group(keep = TRUE)):
  # the field ticked as the function would be
  for (a in intersect(fo, names(sch$args))) {
    d <- fl[[a]]
    if (identical(.arg_kind(sch$args[[a]]$type), "logical") && is.null(sch$args[[a]]$default) &&
        is.logical(d) && length(d) == 1L && !is.na(d)) {
      sch$args[[a]]$default <- d
    }
  }
  sch$formals <- setdiff(unique(c(fo, names(sch$args))), c("plan", ".plan", "..."))
  # (no argument is read only: normalize_ard()'s hierarchy is a field too,
  # marked the ARD's while it says the same, warned when it does not)
  sch$fixed <- character()
  # (the main fields in the function's order too)
  sch$visible <- .arg_order(.plan_form_main(fn), sch$formals)
  sch
}

# where an argument's value came from: the report's own, the study's
# defaults, a pattern (the item's arg_from, read from the spec); else a
# value the item has that the report does not say is the defaults'
.arg_from <- function(it, a) {
  af <- it$arg_from
  if (!is.null(af) && a %in% names(af)) return(af[[a]])
  if (a %in% names(it$own_args %||% list())) "own" else if (a %in% names(it$args)) "defaults" else "own"
}

# The function's arguments a card shows, in two groups (each in the
# function's order, a data argument -- x, plan -- none): `upper` those the
# call's code writes -- the required, the item's arguments
# (tflspec::tfl_item_written()), those the ARD decides (`ard`) -- and
# `lower` the rest.  The same for every report: no kind of table shows or
# hides an argument.
.data_args <- c("x", "plan", ".plan", "data", "ard", "...")

.fields_split <- function(fn, it, formals, ard = character()) {
  fo <- setdiff(formals, .data_args)
  fl <- tryCatch(formals(utils::getFromNamespace(sub("^.*::", "", fn), "rtfreporter")),
                 error = function(e) NULL)
  req <- names(fl)[vapply(fl, function(d) is.symbol(d) && !nzchar(as.character(d)), NA)]
  a <- it$args %||% list()
  wr <- if (length(a)) tryCatch(tflspec::tfl_item_written(tflspec::tfl_item(fn, a)),
                                error = function(e) names(a)) else character()
  # (a verb's positional ... keyed by an argument -- plan_hide(cols) -- by its key too)
  up <- unique(c(req, wr, names(a)[nzchar(names(a))], ard))
  list(upper = fo[fo %in% up], lower = fo[!fo %in% up])
}

# A form card's body: the function's arguments as fields (each marked where
# its value came from), those its code writes above, the rest below
# ("Not in the code", folded: a value given there moves its field up).
# `choices`: the columns offered (the row layers, the ARD's keys).
.plan_form_body <- function(cid, fn, it, choices = NULL, lang = "en", cols = NULL,
                            ard = list(), fixed_link = NULL) {
  sch <- .plan_form_schema(fn)
  a <- it$args %||% list()
  sp <- .fields_split(fn, it, sch$formals, ard = names(Filter(length, ard)))
  field <- function(k, value) {
    from <- if (k %in% names(a)) .arg_from(it, k) else "own"
    own <- if (k %in% names(it$own_args %||% list())) "This table"
    # an argument the ARD decides (normalize_ard()'s hierarchy: step 1's
    # analysis): an ordinary field, "From the ARD" while it says the same
    # (blank: the ARD's), a warning when another value is given
    ak <- ard[[k]]
    if (length(ak)) {
      same <- is.null(value) || identical(as.character(unlist(value)), as.character(ak))
      f <- arg_field(cid, k, sch$args[[k]]$type, label = .arg_meaning(fn, k), value = value,
                     default = paste(ak, collapse = ", "), choices = unique(c(ak, choices)),
                     from = if (same) "ard" else from, own = if (!same) own,
                     help = .arg_help(fn, k, sch, lang), lang = lang)
      return(.ard_checked(f, ak, same, fixed_link, lang))
    }
    if (identical(sch$args[[k]]$type, "widths")) {
      return(.widths_field(cid, k, cols %||% .widths_cols(value, choices), value,
                           label = .arg_meaning(fn, k), from = from, own = own,
                           help = .arg_help(fn, k, sch, lang), lang = lang))
    }
    arg_field(cid, k, sch$args[[k]]$type, label = .arg_meaning(fn, k), value = value,
              default = sch$args[[k]]$default, choices = choices,
              from = from, own = own, help = .arg_help(fn, k, sch, lang), lang = lang)
  }
  shiny::div(
    class = "rp-plan-args", `data-card` = cid,
    shiny::div(class = "rp-args-up", `data-formals` = paste(sch$formals, collapse = ","),
               arg_group(NULL, lapply(sp$upper, function(k) field(k, a[[k]])), lang = lang)),
    arg_more(cid, fn, schema = sch, lang = lang, rest = sp$lower,
             fields = lapply(sp$lower, function(k) field(k, NULL)), label = "Not in the code"))
}

# The cards of a form of their own (table_plan()'s roles, plan_cells()'
# rows and formats, plan_col_header()'s lines, plan_sort()'s order): the
# arguments their form says; the function's other arguments are fields as
# a form card's, below the form (#417: every argument on the card)
.special_covered <- list(table_plan = c("cols", "rows", "label"),
                         plan_cells = c("value", "categorical", "continuous", "default"),
                         plan_col_header = c("header", "values"),
                         plan_sort = c("keys", "stat"))
.special_args <- function(fn) {
  setdiff(.plan_form_schema(fn)$formals, c(.data_args, .special_covered[[fn]]))
}
.special_more <- function(cid, fn, it, lang = "en") {
  ex <- .special_args(fn)
  if (!length(ex)) return(NULL)
  sch <- .plan_form_schema(fn)
  a <- it$args %||% list()
  up <- ex[ex %in% names(a)]
  lo <- setdiff(ex, up)
  field <- function(k, value) {
    arg_field(cid, k, sch$args[[k]]$type, label = .arg_meaning(fn, k), value = value,
              default = sch$args[[k]]$default,
              from = if (k %in% names(a)) .arg_from(it, k) else "own",
              own = if (k %in% names(it$own_args %||% list())) "This table",
              help = .arg_help(fn, k, sch, lang), lang = lang)
  }
  shiny::div(
    class = "rp-plan-args rp-special-more", `data-card` = cid,
    shiny::div(class = "rp-args-up", `data-formals` = paste(ex, collapse = ","),
               arg_group(NULL, lapply(up, function(k) field(k, a[[k]])), lang = lang)),
    arg_more(cid, fn, schema = sch, lang = lang, rest = lo,
             fields = lapply(lo, function(k) field(k, NULL)), label = "Not in the code"))
}
# ... read back: those fields' values (`set`) and the fields emptied, or a
# check as the function's default (`unset`); NULL while they are not there
.special_read <- function(input, cid, fn) {
  ex <- .special_args(fn)
  seen <- ex[vapply(ex, function(k) !is.null(.input_value(input, paste0(cid, "__", k))), NA)]
  if (!length(seen)) return(NULL)
  sch <- .plan_form_schema(fn)
  r <- builder_card_read(input, cid, sch, args = seen, logical_false = TRUE)
  out <- list(set = list(), unset = character())
  for (k in seen) {
    v <- r[[k]]
    d <- sch$args[[k]]$default
    empty <- is.null(v) || !length(v) || (length(v) == 1L && (is.na(v) || identical(v, ""))) ||
      (is.logical(v) && (if (is.null(d)) isFALSE(as.logical(v)) else
        identical(as.logical(v), as.logical(d))))
    if (empty) out$unset <- c(out$unset, k) else out$set[[k]] <- .form_list_value(v, sch$args[[k]]$type)
  }
  out
}
# ... put into the item's arguments as they are now
.special_args_put <- function(args, r) {
  args <- args %||% list()
  for (k in r$unset) args[[k]] <- NULL
  for (k in names(r$set)) args[[k]] <- r$set[[k]]
  args
}

# A field of a value the ARD decides: its ARD value on it (`data-ard`, the
# page compares as it is changed), and a warning -- not a stop -- shown when
# the field says another (`same` FALSE), with the way to step 1 (`link`)
.ard_checked <- function(field, ard, same, link = NULL, lang = "en") {
  field <- htmltools::tagAppendAttributes(field, `data-ard` = paste(ard, collapse = "|"))
  field <- htmltools::tagAppendChild(field, shiny::div(
    class = "small text-warning rp-ard-warn", style = if (same) "display: none;",
    sprintf(tr("Not the ARD analysis's (%s): the table may fail or come out otherwise. The analysis is changed in step 1.", lang),
            paste(ard, collapse = " > "))))
  htmltools::tagAppendChild(field, shiny::div(class = "small rp-ard-link", link))
}


# the form read back: the arguments its fields say (an unticked check
# FALSE); NULL while its fields are not there yet.  `cols`: the columns
# of a widths field, as drawn (.widths_cols()).
.plan_form_read <- function(input, cid, fn, cols = NULL) {
  sch <- .plan_form_schema(fn)
  wa <- names(Filter(function(x) identical(x$type, "widths"), sch$args))
  seen <- vapply(.plan_form_main(fn), function(k) {
    if (k %in% wa) {
      return(length(cols) > 0L && !is.null(.input_value(input, paste0(cid, "__", k, "__1"))))
    }
    !is.null(.input_value(input, paste0(cid, "__", k))) ||
      isTRUE(.input_value(input, paste0(cid, "__", k, "__isr")))
  }, NA)
  if (!any(seen)) return(NULL)
  out <- builder_card_read(input, cid, sch, args = setdiff(sch$formals, c(wa, sch$fixed)),
                           logical_false = TRUE)
  for (k in intersect(names(out), names(sch$args))) {
    out[[k]] <- .form_list_value(out[[k]], sch$args[[k]]$type)
    if (identical(sch$args[[k]]$type, "formula")) out[[k]] <- .formula_text(out[[k]])
  }
  for (k in wa) {
    if (isTRUE(.input_value(input, paste0(cid, "__", k, "__isr")))) {
      txt <- .input_value(input, paste0(cid, "__", k, "__r"))
      if (!is.null(txt) && nzchar(trimws(txt))) out[[k]] <- tflspec::tfl_r(trimws(txt))
      next
    }
    w <- .widths_read(input, paste0(cid, "__", k), cols)
    if (length(w)) out[[k]] <- w
  }
  out
}

# A condition typed as R for a one-sided formula's argument
# (plan_cell_style(where = )): its text without the "~" the code writes
# itself (`~ is.na(label)` typed: `is.na(label)`); blank: none
.formula_text <- function(v) {
  if (!inherits(v, "tfl_r")) return(v)
  txt <- trimws(sub("^[[:space:]]*~", "", as.character(unclass(v))))
  if (!nzchar(txt)) NULL else tflspec::tfl_r(txt)
}

# A list field's values as the argument takes them, by its type: whole
# numbers (column positions: plan_paginate_cols(at = c(2, 4))) as
# integers, a lone TRUE / FALSE (plan_row_group(collapse = TRUE)) a
# logical, the rest the columns' names
.form_list_value <- function(v, type) {
  if (!is.character(v) || !length(v) || is.null(type)) return(v)
  alts <- .type_split(type, "|")
  if ("logical" %in% alts && length(v) == 1L && v %in% c("TRUE", "FALSE")) return(as.logical(v))
  if ("list<integer>" %in% alts && all(grepl("^[0-9]+$", v))) return(as.integer(v))
  v
}

# ---- plan_columns(widths = ): a number a column ---------------------------------

# the columns a widths field offers: those the widths name, then the
# columns printed (the row label column, .values -- every value column --
# and the value columns by name)
.widths_cols <- function(widths = NULL, choices = NULL) {
  nm <- if (is.list(widths) || is.numeric(widths)) names(widths) else NULL
  unique(c(nm[!is.na(nm) & nzchar(nm)], as.character(unname(choices))))
}

# A widths field: a small table, a column a row and its relative width (a
# number; blank: rtfreporter's), its `R` switch as every field has
.widths_field <- function(id, arg, cols, value = NULL, label = NULL, from = "own", own = NULL,
                          help = NULL, lang = "en") {
  t <- function(x) tr(x, lang)
  fid <- paste0(id, "__", arg)
  is_r <- inherits(value, "tfl_r")
  r_text <- if (is_r) as.character(unclass(value)) else ""
  w <- if (!is_r && (is.list(value) || is.numeric(value))) value else list()
  rows <- lapply(seq_along(cols), function(i) {
    v <- if (!is.null(names(w)) && cols[[i]] %in% names(w)) w[[cols[[i]]]]
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(cols[[i]])),
      shiny::tags$td(shiny::tags$input(
        type = "number", id = paste0(fid, "__", i), class = "form-control form-control-sm rp-number",
        value = if (!is.null(v) && length(v) && !is.na(v[[1L]])) as.character(v[[1L]]),
        step = "any", min = "0", inputmode = "decimal", style = "width: 7em;")))
  })
  shiny::div(
    class = paste("rp-field rp-field-widths", if (is_r) "rp-field-r",
                  if (length(w) || nzchar(r_text)) "rp-has-value"),
    `data-arg` = arg, `data-field` = fid,
    shiny::tags$label(class = "rp-field-label",
                      builder_label(label, arg, "arg", lang),
                      from_badge(from, lang, own = own), help_tip(help)),
    shiny::div(class = "rp-field-row",
               shiny::div(class = "rp-form-input",
                          shiny::tags$table(class = "rp-widths",
                                            shiny::tags$thead(shiny::tags$tr(
                                              shiny::tags$th(t("Column")),
                                              shiny::tags$th(t("Relative width")))),
                                            shiny::tags$tbody(rows)),
                          shiny::div(class = "small text-muted",
                                     t("Blank: rtfreporter's width. .values: every value column."))),
               shiny::tags$input(type = "text", id = paste0(fid, "__r"),
                                 class = "form-control form-control-sm rp-mono rp-r-input",
                                 value = r_text, placeholder = "R", spellcheck = "false"),
               shiny::tags$label(class = "rp-r-toggle", title = t("As R"),
                                 shiny::tags$input(type = "checkbox", id = paste0(fid, "__isr"),
                                                   class = "rp-isr",
                                                   checked = if (is_r) NA else NULL),
                                 shiny::tags$span("R"))))
}

# ... read back: the columns given a number, in the field's order (a named
# list, as the sheets give widths); none: NULL
.widths_read <- function(input, fid, cols) {
  out <- list()
  for (i in seq_along(cols)) {
    v <- .input_value(input, paste0(fid, "__", i))
    if (is.null(v) || !length(v)) next
    n <- suppressWarnings(as.numeric(v[[1L]]))
    if (length(n) == 1L && !is.na(n)) out[[cols[[i]]]] <- n
  }
  if (length(out)) out else NULL
}

# two values the same (a number as a number: 30 and 30L; a check's FALSE
# and no value)
.same_value <- function(a, b, logical = FALSE) {
  if (logical) return(isTRUE(as.logical(a)) == isTRUE(as.logical(b)))
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  if (is.numeric(a) && is.numeric(b)) return(isTRUE(all.equal(as.numeric(a), as.numeric(b))))
  identical(.arg_to_r(a), .arg_to_r(b))
}

# The item's whole arguments after the form's edit: its arguments (the
# report's over its layers') with what the form says otherwise put in, a
# field emptied taking its argument away (an argument the item does not
# say is the function's default: a check ticked as that default is no
# change).  .builder_item_write() keeps of them what differs from the layers.
.plan_form_args <- function(read, it, fn) {
  sch <- .plan_form_schema(fn)
  eff <- it$args %||% list()
  # (a widths field not drawn -- no columns known -- reads nothing: no change)
  wa <- names(Filter(function(x) identical(x$type, "widths"), sch$args))
  for (a in setdiff(union(names(read), setdiff(intersect(names(eff), sch$formals),
                                              setdiff(wa, names(read)))), sch$fixed)) {
    lg <- identical(.arg_kind(sch$args[[a]]$type), "logical") && !inherits(read[[a]], "tfl_r")
    new <- read[[a]]
    cur <- eff[[a]]
    if (is.null(cur) && lg) cur <- sch$args[[a]]$default
    if (.same_value(new, cur, lg)) next
    eff[a] <- list(new)
    if (is.null(new)) eff[[a]] <- NULL
  }
  eff
}

# ---- plan_digits(), plan_levels(), plan_filter() -----------------------------------

# plan_digits(rounding = ): the study's rounding (one for every report: the
# study sheet's), a choice of the schema's
.rounding_field <- function(cid, value = NULL, lang = "en") {
  ty <- .schema_of("plan_digits")$args$rounding$type %||% "enum[sas|r|iec60559]"
  v <- if (length(value) == 1L && !is.na(value) && nzchar(value)) value
  shiny::tagList(
    arg_field(cid, "rounding", ty, label = .arg_meaning("plan_digits", "rounding"), value = v,
              from = if (!is.null(v)) "defaults" else "own",
              help = tr(paste("How a 5 is rounded: sas half away from zero, r half to even,",
                              "iec60559 as the machine stores the number. The study's: every",
                              "report's (the Study tab's Rounding); blank there: options(rtfreporter.rounding)."),
                        lang),
              lang = lang),
    # (a change here is the study's: every report's -- a mark, its words
    # in its title)
    shiny::span(class = "rp-badge rp-badge-defaults rp-every-report",
                title = tr("Applies to every report (the study's defaults.yml)", lang),
                tr("Applies to every report", lang)),
    shiny::div(class = "small text-muted rp-every-report-note",
               tr("Applies to every report (the study's defaults.yml)", lang)))
}

# Each variable's empty values hidden (plan_levels(.drop_empty = ):
# variables$empty_levels "hide"): a named logical, by variable
.empty_levels_of <- function(vr) {
  if (!is.data.frame(vr) || !nrow(vr) || !"empty_levels" %in% names(vr)) return(list())
  vr <- vr[!is.na(vr$variable) & !duplicated(vr$variable), , drop = FALSE]
  stats::setNames(as.list(!is.na(vr$empty_levels) & tolower(vr$empty_levels) == "hide"),
                  vr$variable)
}

# The plan_levels() card's body: each categorical variable's values in
# their order, dragged (`id(paste0("lv", i))`, the variable's place in the
# builder's variables), and its empty values hidden or not (`"lvdrop"`).
# `vars`: a summary table's block of variables, its items in their order
# (the `variable` entry, dragged: `id("vars")`), first; `cols`: the column
# variables' levels in order (the columns' order, drawn by the page), next.
# `empty`: the variables sheet's rows (.empty_levels_of()).  Each variable
# is a line until opened (.levels_var()).
.levels_body <- function(id, st, codelists = NULL, empty = NULL, lang = "en",
                         vars = NULL, cols = NULL) {
  t <- function(x) tr(x, lang)
  v <- st$variables
  hidden <- .empty_levels_of(empty)
  rows <- lapply(seq_len(nrow(v)), function(i) {
    nm <- v$variable[i]
    lv <- st$levels[[nm]]
    if (!identical(v$kind[i], "categorical") || !length(lv)) return(NULL)
    .levels_var(
      id(paste0("lv", i)), nm, lv,
      sortable::rank_list(text = NULL, labels = .levels_with_labels(lv, nm, codelists),
                          input_id = id(paste0("lv", i)), orientation = "horizontal"),
      ard = st$level_ard[[nm]], from = st$level_from[[nm]], lang = lang,
      extra = shiny::tags$label(
        class = "small rp-levels-drop",
        shiny::tags$input(type = "checkbox", id = id(paste0("lvdrop", i)),
                          class = "form-check-input me-1",
                          checked = if (isTRUE(hidden[[nm]])) NA else NULL),
        with_tip(builder_label(.arg_meaning("plan_levels", ".drop_empty"), ".drop_empty", "arg", lang),
                 t("Ticked: the variable's values no subject has (a code list's value, 0 in every column) are not printed. Unticked: printed with their zeros."))))
  })
  rows <- Filter(Negate(is.null), rows)
  # (the code's order: variable, the column keys, then the categorical
  # variables)
  shiny::tagList(
    if (length(vars)) shiny::div(
      class = "rp-b-sec rp-levels-var rp-levels-block", `data-variable` = "variable",
      shiny::div(class = "rp-b-sec-label",
                 with_tip(builder_label("Analysis variables' order (the levels of variable)", "variable", "arg", lang),
                          t("The order of the analysis variables' blocks, dragged. A variable nested under another's level goes with it."))),
      .levels_state(.levels_block_order(st), intersect(st$variable_ard, .levels_block_order(st)),
                    nm = "variable", lang = lang),
      sortable::rank_list(text = NULL, labels = vars, input_id = id("vars"))),
    cols,
    if (length(rows)) rows else if (!length(vars)) shiny::div(
      class = "small text-muted", t("No categorical variable to order.")))
}

# the analysis variables in their blocks' order, those nested under
# another's level left out (plan_levels()' `variable` names none of them)
.levels_block_order <- function(st) {
  v <- st$variables
  if (!is.data.frame(v) || !nrow(v)) return(character())
  un <- v$under %||% rep(NA_character_, nrow(v))
  v$variable[is.na(un) | !nzchar(un)]
}

# One variable's values on plan_levels()' card (#428): closed, one line --
# "RACE: White, Black or African American, ... (6)  > Reorder" -- and
# open, every value a chip, dragged.  The chips stay drawn while closed,
# cut to no height: never display: none nor a closed <details> -- the
# sortable list reads its items' text, which a list not drawn has not (it
# wrote empty levels).  Under the line, whether the code writes the order
# (.levels_state()).  `input_id`: the list's; `extra`: drawn under the
# chips (the empty values' tick).
.levels_var <- function(input_id, nm, lv, list_ui, ard = NULL, from = NULL,
                        extra = NULL, lang = "en") {
  t <- function(x) tr(x, lang)
  shiny::div(
    class = "rp-b-sec rp-levels-var", `data-variable` = nm, `data-input` = input_id,
    shiny::tags$a(
      href = "#", class = "rp-levels-toggle", role = "button",
      shiny::tags$code(nm), ": ",
      shiny::span(class = "rp-levels-sum", .levels_summary(lv)), " ",
      shiny::span(class = "rp-levels-n", sprintf("(%d)", length(lv))), " ",
      shiny::span(class = "rp-levels-go rp-closed-only", paste("\u25b8", t("Reorder"))),
      shiny::span(class = "rp-levels-go rp-open-only", paste("\u25be", t("Close")))),
    .levels_state(lv, ard, from, nm = nm, lang = lang),
    shiny::div(class = "rp-levels-body", list_ui, extra))
}

# the closed line's values: the first ones up to about 40 characters, then
# "..." (the page writes it again as they are dragged: .builder_content_js)
.levels_summary <- function(lv, width = 40L) {
  lv <- as.character(lv)
  if (!length(lv)) return("")
  n <- cumsum(nchar(lv) + 2L)
  k <- max(1L, sum(n <= width))
  paste(c(lv[seq_len(k)], if (k < length(lv)) "\u2026"), collapse = ", ")
}

# Whether the code writes a variable's order: the ARD's own (`ard`: the
# order the ARD keeps the variable in, a factor -- the code list applied
# when it was made) is not written (tflspec #355); another is, with the
# way back to the ARD's ("Back to the ARD's order": the page sends
# b_lv_reset).  With no order of the ARD's, where it comes from
# (.level_order_text(): the code list's, or as first seen -- check it).
.levels_state <- function(lv, ard = NULL, from = NULL, nm = NULL, lang = "en") {
  t <- function(x) tr(x, lang)
  line <- function(cls, ...) shiny::div(class = paste("small rp-levels-state", cls), ...)
  if (length(ard)) {
    # (both lines drawn, one shown: the page shows the other as the values
    # are dragged, by the ARD's order it is given)
    same <- identical(as.character(lv), as.character(ard))
    hide <- "display: none;"
    return(shiny::div(
      class = "rp-levels-states",
      `data-ard` = jsonlite::toJSON(as.character(ard)),
      shiny::div(class = "small rp-levels-state text-muted rp-st-same", style = if (!same) hide,
                 t("The ARD's order (not written in the code)")),
      shiny::div(class = "small rp-levels-state text-muted rp-st-own", style = if (same) hide,
                 t("This table's order (written in the code)"), " ",
                 shiny::tags$a(href = "#", class = "rp-levels-reset", `data-variable` = nm,
                               t("Back to the ARD's order")))))
  }
  if (is.null(from)) return(NULL)
  src <- from$source %||% "seen"
  if (identical(src, "plan")) return(NULL)
  w <- .level_order_text(from, lang)
  if (identical(src, "seen")) return(line("text-warning", w$text))
  line(if (isTRUE(w$warn)) "text-warning" else "text-muted",
       paste0(t("Written in the code"), if (!is.null(w)) paste0(": ", w$text)))
}

# plan_filter()'s `where` as one condition's R (several joined by &, as
# the sheet keeps them); NULL for none
.filter_where_text <- function(where) {
  if (is.null(where) || !length(where)) return(NULL)
  one <- function(w) if (inherits(w, "tfl_r")) as.character(unclass(w)) else .arg_to_r(w)
  if (inherits(where, "tfl_r")) return(one(where))
  if (is.list(where)) {
    parts <- vapply(where, one, "")
    return(if (length(parts) == 1L) parts else paste0("(", parts, ")", collapse = " & "))
  }
  if (is.character(where)) return(paste(where, collapse = " & "))
  NULL
}

# The columns a row filter's condition reads, for the condition builder:
# the ARD's keys with their values, the variables and their values, and
# each statistic a number (typed in).  `meta`: ard_meta(); NULL: none.
.filter_data <- function(meta) {
  if (is.null(meta)) return(NULL)
  cols <- c(meta$keys %||% list(),
            list(variable = meta$variables$variable %||% character()))
  lv <- unlist(lapply(meta$variables$levels %||% character(), .split_list))
  if (length(lv)) cols$variable_level <- unique(lv)
  cols <- Filter(length, cols)
  n <- max(c(1L, lengths(cols)))
  d <- as.data.frame(lapply(cols, function(x) rep_len(as.character(x), n)),
                     stringsAsFactors = FALSE, check.names = FALSE)
  for (s in setdiff(meta$stats %||% character(), names(d))) d[[s]] <- rep(NA_real_, n)
  d
}
