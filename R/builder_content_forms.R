# The table builder's content, part 3: the older names of the card
# generator (R/spec_card.R), the remaining arguments of the cards with an
# editor of their own, and the plan_columns(), plan_digits(), plan_levels(),
# plan_filter() cards.

# ---- the cards of the generator ----------------------------------------------------

# The cards whose body is the generator's (spec_card(); the verbs:
# .plan_form_fns): their arguments as fields by the definitions' types,
# those the code writes above, the rest folded under "More settings".  The
# names the builder has used for the generator's parts:
.plan_form_schema <- function(fn) spec_card_defs(fn)
.plan_form_main <- function(fn) spec_card_defs(fn)$visible
.plan_form_body <- function(cid, fn, it, choices = NULL, lang = "en",
                            ard = list(), fixed_link = NULL) {
  spec_card(cid, fn, it, lang = lang,
            ctx = list(choices = choices, ard = ard, fixed_link = fixed_link))
}
.plan_form_read <- function(input, cid, fn) spec_card_read(input, cid, fn)
.plan_form_args <- function(read, it, fn) spec_card_args(read, it, fn)

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
  spec_card(cid, fn, it, lang = lang, only = ex, class = "rp-special-more")
}
# ... read back: those fields' values (`set`; "(none)" its toggle on: `!unset`,
# tflspec::tfl_unset()) and the fields emptied, or a check as the function's
# default (`unset`: taken out of the arguments); NULL while they are not there
.special_read <- function(input, cid, fn) {
  ex <- .special_args(fn)
  seen <- ex[vapply(ex, function(k) {
    !is.null(.input_value(input, paste0(cid, "__", k))) || !is.null(.input_value(input, paste0(cid, "__", k, "__none")))
  }, NA)]
  if (!length(seen)) return(NULL)
  sch <- .plan_form_schema(fn)
  r <- builder_card_read(input, cid, sch, args = seen, logical_false = TRUE)
  out <- list(set = list(), unset = character())
  for (k in seen) {
    v <- r[[k]]
    if (.is_unset(v)) {
      out$set[[k]] <- v
      next
    }
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
    sprintf(tr("Not the ARD analysis's (%s): the table may fail or come out otherwise. The analysis is changed in step %s.", lang),
            paste(ard, collapse = " > "), .step_no("ard"))))
  htmltools::tagAppendChild(field, shiny::div(class = "small rp-ard-link", link))
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
  # (digits are whole numbers where the type says positions before
  # columns: plan_paginate_cols(at = c(2, 4)), list<integer>|list<column>;
  # where it says columns first a name of digits stays a name --
  # plan_columns(row_title = "1"), list<column>|list<integer>, is written
  # "1" in the YAML, which tflspec reads as a column, a YAML 1 a position)
  ints <- match("list<integer>", alts)
  cols <- grep("^list<column", alts)
  if (!is.na(ints) && (!length(cols) || ints < min(cols)) && all(grepl("^[0-9]+$", v))) {
    return(as.integer(v))
  }
  v
}

# two values the same (a number as a number: 30 and 30L; a check's FALSE
# and no value)
.same_value <- function(a, b, logical = FALSE) {
  if (logical) return(isTRUE(as.logical(a)) == isTRUE(as.logical(b)))
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  if (is.numeric(a) && is.numeric(b)) return(isTRUE(all.equal(as.numeric(a), as.numeric(b))))
  identical(.arg_to_r(a), .arg_to_r(b))
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
  one <- function(w) {
    if (inherits(w, "tfl_r") || is.character(w)) as.character(unclass(w)) else .arg_to_r(w)
  }
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
