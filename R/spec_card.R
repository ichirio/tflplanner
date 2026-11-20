# The card generator: one entry for every card made from tflspec's
# definitions (tfl_schema(fn) and the table tfl_arg_defs(fn = )).  A card is
# one function call; its fields are the function's arguments, each drawn by
# arg_field() from its definition -- no card has a list of fields of its
# own.  The parts are the kit's (R/builder_cards*.R, R/arg_defs*.R); this
# file only puts them together:
#
#   spec_card_defs(fn)                   the definitions a card is made from
#   spec_card_fields(fn, item, ...)      its fields as a table: name, type,
#                                        tier, group, shown (above the fold)
#   spec_card(id, fn, item, ctx, lang)   the card's body
#   spec_card_read(input, id, fn, ctx)   the body read back: the arguments
#   spec_card_args(read, item, fn)       the item's whole arguments after
#                                        the edit
#
# What the definitions say is all a card needs: the type and the input form
# (the field), the choices, the default, the tier (`key`: shown though it
# has a default), the group (a heading over its fields) and the labels.
# Where tflspec's definitions say no field tflplanner draws yet,
# R/spec_card_fallback.R holds the few types.

# ---- the definitions ---------------------------------------------------------

# The functions whose card is the generator's alone: a table's layout verbs
# and the verbs that had no form (plan_empty(), plan_titles(),
# plan_footnotes(), plan_after()).  The other table verbs have an editor of
# their own (R/srv_builder.R), their remaining arguments the generator's.
.plan_form_fns <- c("normalize_ard", "plan_filter", "plan_blanks", "plan_paginate_rows",
                    "plan_paginate_cols", "plan_paginate_group", "plan_row_group", "plan_stub",
                    "plan_columns", "plan_style", "plan_hide", "plan_cell_style",
                    "plan_empty", "plan_titles", "plan_footnotes", "plan_after")

# the arguments a card never shows: the data the call is given
.data_args <- c("x", "plan", ".plan", "data", "ard", "...")

# the function's own formals (rtfreporter's); NULL when it has none there
.spec_fn_formals <- function(fn) {
  tryCatch(formals(utils::getFromNamespace(sub("^.*::", "", fn), "rtfreporter")),
           error = function(e) NULL)
}

# The columns of tfl_arg_defs() the schema entry does not carry yet, put on
# its arguments when the table has them (P0): `group`, the label in en / ja
# (`label_en`, `label_ja`) and the help in ja (`help_ja`); and the
# function's groups -- their order and headings -- the table's attribute
# "groups" (else the schema's `groups:`).
.spec_defs_columns <- function(sch, defs) {
  if (!is.data.frame(defs) || !nrow(defs)) return(sch)
  cell <- function(col, i) {
    v <- if (col %in% names(defs)) defs[[col]][[i]]
    if (is.null(v) || length(v) != 1L || is.na(v) || !nzchar(as.character(v))) NULL else as.character(v)
  }
  for (i in seq_len(nrow(defs))) {
    a <- defs$name[[i]]
    if (is.null(sch$args[[a]])) next
    g <- cell("group", i)
    if (!is.null(g)) sch$args[[a]]$group <- g
    en <- cell("label_en", i)
    ja <- cell("label_ja", i)
    # (no Japanese in the table: the English, translated by the app's texts)
    if (!is.null(en)) sch$args[[a]]$label <- if (is.null(ja)) en else list(en = en, ja = ja)
    hj <- cell("help_ja", i)
    he <- sch$args[[a]]$help %||% sch$args[[a]]$description
    if (!is.null(hj) && is.character(he)) sch$args[[a]]$help <- list(en = he, ja = hj)
  }
  sch$groups <- .spec_groups_list(attr(defs, "groups", exact = TRUE)) %||% sch$groups
  sch
}

# tfl_arg_defs()'s groups (a data frame id, label_en, label_ja; no rows:
# none) as the schema's `groups:` list, each list(id, label); NULL for none
.spec_groups_list <- function(g) {
  if (!is.data.frame(g)) return(g)
  if (!nrow(g)) return(NULL)
  lapply(seq_len(nrow(g)), function(i) {
    en <- g$label_en[[i]]
    ja <- if ("label_ja" %in% names(g)) g$label_ja[[i]] else NA_character_
    label <- if (is.na(en)) g$id[[i]] else if (is.na(ja)) en else list(en = en, ja = ja)
    list(id = g$id[[i]], label = label)
  })
}

# A positional `...` whose key the schema's arguments do not name
# (plan_titles(): titles; plan_after(): steps): a field of its entries as
# R, as the call writes them ("Title 1", "Title 2"), where the function
# has its `...`; read back as the entries (.spec_dots_read()).  Until P0
# (tfl_arg_defs() has no row for a `...`).
.spec_defs_dots <- function(sch) {
  d <- sch$dots
  if (!identical(d$style, "positional") || is.null(d$key) || is.null(d$type) ||
        d$key %in% names(sch$args)) {
    return(sch)
  }
  sch$args[[d$key]] <- list(type = "r", help = d$description, dots = TRUE, dots_type = d$type)
  sch
}
# ... its value as the field shows it: the entries as R
.spec_dots_text <- function(fn, k, value) {
  if (is.null(value) || inherits(value, c("tfl_r", "tfl_unset"))) return(value)
  txt <- tryCatch(args_to_r(fn, stats::setNames(list(value), k)), error = function(e) NULL)
  if (is.null(txt)) value else tflspec::tfl_r(txt)
}
# ... and the field's R back as the entries (R that is not entries: as typed)
.spec_dots_read <- function(fn, k, value) {
  if (!inherits(value, "tfl_r")) return(value)
  v <- tryCatch(args_from_r(fn, as.character(unclass(value)))[[k]], error = function(e) NULL)
  v %||% value
}

# The definitions a card is made from: tflspec's schema entry with each
# argument's row of tfl_arg_defs() (.schema_of()), the fallback types
# (R/spec_card_fallback.R), an `r` input drawn as R (.spec_input_r()), and
# what the generator works out --
#   formals   the card's fields, in the function's order (a `...` given by
#             a key: the key in its place)
#   visible   the arguments the schema marks `main` (the card's fields are
#             on the page when one of them is)
#   fixed     the arguments not read back (none)
# -- a list as tfl_schema()'s: the function's label, rank, stage, groups,
# and `args` (each: type, input, choices, default, tier, group, label, help).
spec_card_defs <- function(fn) {
  sch <- .schema_of(fn) %||% list()
  sch <- .spec_defs_columns(sch, .arg_defs(fn = fn))
  sch <- .spec_defs_dots(sch)
  sch <- .spec_input_r(.spec_fallback_types(sch, fn))
  fl <- .spec_fn_formals(fn)
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
    d <- .spec_formal_default(fn, fl, a)
    if (identical(.arg_kind(sch$args[[a]]$type), "logical") && is.null(sch$args[[a]]$default) &&
          is.logical(d) && length(d) == 1L && !is.na(d)) {
      sch$args[[a]]$default <- d
    }
  }
  # (the key of a `...` where the function has its `...`)
  key <- names(Filter(function(x) isTRUE(x$dots), sch$args))
  if (length(key) && "..." %in% fo) {
    fo <- setdiff(fo, key)
    fo <- append(fo, key, after = match("...", fo))
  }
  sch$formals <- setdiff(unique(c(fo, names(sch$args))), c("plan", ".plan", "..."))
  # (no argument is read only: normalize_ard()'s hierarchy is a field too,
  # marked the ARD's while it says the same, warned when it does not)
  sch$fixed <- character()
  sch$visible <- .arg_order(.spec_main(sch, fo), sch$formals)
  sch
}

# the arguments a definition marks `main`, in the function's order
.spec_main <- function(sch, formals) {
  a <- sch$args %||% list()
  .arg_order(names(a)[vapply(a, function(x) isTRUE(x$main), NA)], formals)
}

# a formal's default: the function's own; NULL for a NULL formal (the
# verbs that record only what the call says: their defaults are the
# definitions', `default:` in tflspec's schema)
.spec_formal_default <- function(fn, fl, a) {
  # (a required argument's formal is the empty symbol: not evaluated here)
  nul <- vapply(fl[a], function(d) !is.symbol(d) && is.null(d), NA)
  if (length(nul) == 1L && nul) NULL else fl[[a]]
}

# ---- the fields --------------------------------------------------------------

# where an argument's value came from: the report's own, the study's
# defaults, a pattern (the item's arg_from, read from the spec); else a
# value the item has that the report does not say is the defaults'
.arg_from <- function(it, a) {
  af <- it$arg_from
  if (!is.null(af) && a %in% names(af)) return(af[[a]])
  if (a %in% names(it$own_args %||% list())) "own" else if (a %in% names(it$args)) "defaults" else "own"
}

# A card's fields, one row an argument, in the function's order:
#   name, type   the argument and its type (the definition's)
#   tier         the definition's: required / key / usual / other
#   group        the group it belongs to (NA: none)
#   shown        TRUE above the fold: a required argument, one the call's
#                code writes (the item's arguments,
#                tflspec::tfl_item_written()), one the ARD decides (`ard`)
#                and a `key` one -- shown though it has a default; the
#                rest (`usual`, `other`) are folded
# The same for every report: no kind of table shows or hides an argument.
# `only`: the arguments drawn (a card with an editor of its own draws the
# others itself).
spec_card_fields <- function(fn, item = list(), ard = character(), defs = spec_card_defs(fn),
                             only = NULL) {
  fo <- setdiff(defs$formals, .data_args)
  if (!is.null(only)) fo <- fo[fo %in% only]
  fl <- .spec_fn_formals(fn)
  req <- names(fl)[vapply(fl, function(d) is.symbol(d) && !nzchar(as.character(d)), NA)]
  a <- item$args %||% list()
  wr <- if (length(a)) tryCatch(tflspec::tfl_item_written(tflspec::tfl_item(fn, a)),
                                error = function(e) names(a)) else character()
  col <- function(k) vapply(fo, function(n) {
    v <- defs$args[[n]][[k]]
    if (is.character(v) && length(v) == 1L && nzchar(v)) v else NA_character_
  }, "", USE.NAMES = FALSE)
  tier <- col("tier")
  # (a verb's positional ... keyed by an argument -- plan_hide(cols) -- by its key too)
  up <- unique(c(req, wr, names(a)[nzchar(names(a))], ard, fo[tier %in% c("required", "key")]))
  data.frame(name = fo, type = col("type"), tier = tier, group = col("group"),
             shown = fo %in% up, stringsAsFactors = FALSE)
}

# The groups of a card's fields, in the definition's order: each a list
# (`id`, `label`) -- the function's `groups:` ([{id, label: {en, ja}}]),
# then a group only an argument names (its id its heading).  None: NULL
# (the fields in one block, no heading).
.spec_groups <- function(defs, fields) {
  ids <- unique(fields$group[!is.na(fields$group)])
  if (!length(ids)) return(NULL)
  gs <- lapply(defs$groups %||% list(), function(g) {
    if (is.list(g)) list(id = as.character(g$id %||% NA), label = g$label %||% g$id) else
      list(id = as.character(g), label = as.character(g))
  })
  gs <- Filter(function(g) !is.na(g$id) && g$id %in% ids, gs)
  have <- vapply(gs, function(g) g$id, "")
  c(gs, lapply(setdiff(ids, have), function(i) list(id = i, label = i)))
}

# The parts of a field the generator draws in place of arg_field(), by the
# argument's type: `field` (the part) and `read` (its value back; NULL:
# not there).  One today -- "widths", a number a column -- and the place
# the table verbs' own editors are registered as they become fields.
.spec_parts <- list(
  widths = list(
    field = function(fid, arg, value, ctx, ...) {
      .widths_field(fid, arg, ctx$cols %||% .widths_cols(value, ctx$choices), value, ...)
    },
    seen = function(input, fid, arg, ctx) {
      length(ctx$cols) > 0L && !is.null(.input_value(input, paste0(fid, "__", arg, "__1")))
    },
    read = function(input, fid, arg, ctx) {
      if (isTRUE(.input_value(input, paste0(fid, "__", arg, "__isr")))) {
        txt <- .input_value(input, paste0(fid, "__", arg, "__r"))
        return(if (!is.null(txt) && nzchar(trimws(txt))) tflspec::tfl_r(trimws(txt)))
      }
      .widths_read(input, paste0(fid, "__", arg), ctx$cols)
    }))

# the arguments of a definition drawn by a part
.spec_part_args <- function(defs) {
  names(Filter(function(x) is.character(x$type) && length(x$type) == 1L &&
                 x$type %in% names(.spec_parts), defs$args))
}

# One field of a card: the argument `k` of the definitions, its value the
# item's (`value`: NULL below the fold), marked where it came from.  Its
# `R` switch is the definition's: only a type that takes an R expression
# has one (canon 17dfbb8); R for the rest is the card's "Edit as R".
.spec_field <- function(id, fn, k, value, item, defs, ctx = list(), lang = "en") {
  d <- defs$args[[k]]
  a <- item$args %||% list()
  from <- if (k %in% names(a)) .arg_from(item, k) else "own"
  own <- if (k %in% names(item$own_args %||% list())) item$own_word %||% "This table"
  label <- d$label
  help <- .arg_help(fn, k, defs, lang)
  # an argument the ARD decides (normalize_ard()'s hierarchy: step 1's
  # analysis): an ordinary field, "From the ARD" while it says the same
  # (blank: the ARD's), a warning when another value is given
  ak <- ctx$ard[[k]]
  if (length(ak)) {
    same <- is.null(value) || identical(as.character(unlist(value)), as.character(ak))
    f <- arg_field(id, k, d$type, label = label, value = value,
                   default = paste(ak, collapse = ", "), choices = unique(c(ak, ctx$choices)),
                   from = if (same) "ard" else from, own = if (!same) own,
                   help = help, lang = lang, def = d, ctx = ctx$sources)
    return(.ard_checked(f, ak, same, ctx$fixed_link, lang))
  }
  if (isTRUE(d$dots)) value <- .spec_dots_text(fn, k, value)
  part <- if (k %in% .spec_part_args(defs)) .spec_parts[[d$type]]
  if (!is.null(part)) {
    return(part$field(id, k, value, ctx, label = label, from = from, own = own, help = help,
                      lang = lang, r_ok = .type_takes_r(d$def_type %||% d$type)))
  }
  # (the columns offered only to a field of columns -- a colour's field
  # offers its colours, a text's none -- or to a condition)
  # (an argument's own columns, ctx$arg_choices, before the card's: none
  # there is none offered)
  cols <- if (k %in% names(ctx$arg_choices)) {
    as.character(ctx$arg_choices[[k]])
  } else if (.type_has_column(d$def_type %||% d$type) || identical(.def_input(d), "condition")) {
    ctx$choices
  }
  arg_field(id, k, d$type, label = label, value = value, default = d$default,
            choices = cols, from = from, own = own, help = help, lang = lang,
            def = d, ctx = ctx$sources)
}

# does a type's field take a column of the data (column, list<column>,
# map<aes, column|r>, ...)?  The first alternative decides, as it decides
# the field (.arg_kind()): colour|r|map<column, r> is a colour's
.type_has_column <- function(type) {
  if (is.null(type) || length(type) != 1L || is.na(type) || !nzchar(type)) return(FALSE)
  alts <- .type_split(trimws(type), "|")
  alts <- c(setdiff(alts, "r"), intersect(alts, "r"))
  grepl("\\bcolumn\\b", alts[[1L]])
}

# ---- the card ------------------------------------------------------------------

# A card's body from its definitions (canon 04a5cbe): the fields shown on
# top, under their groups' headings where the definitions give groups; the
# folded ones in ONE fold at the card's end, "More settings (n)", split by
# the same headings inside.  Which is which is the definitions' (tier,
# group) and the item's (spec_card_fields()); each field is marked where
# its value came from, and a value given in the fold moves its field up.
#   item   the item: `args` (its arguments), `own_args`, `arg_from`
#   ctx    what the card knows: `choices` (the columns offered),
#          `arg_choices` (an argument's own, by name: in place of
#          `choices` for it), `cols` (a
#          widths field's columns), `ard` (the values the ARD decides, by
#          argument), `fixed_link` (the way to the ARD's step), `sources`
#          (.choice_values()'s ctx, for the definitions' choice words)
#   only   the arguments drawn (default: all); `class`: the body's own
#' @noRd
spec_card <- function(id, fn, item = list(), ctx = list(), lang = "en",
                      defs = spec_card_defs(fn), only = NULL, class = NULL) {
  a <- item$args %||% list()
  fields <- spec_card_fields(fn, item, ard = names(Filter(length, ctx$ard)), defs = defs,
                             only = only)
  draw <- function(k, shown) .spec_field(id, fn, k, if (shown) a[[k]], item, defs, ctx, lang)
  groups <- .spec_groups(defs, fields)
  if (is.null(groups)) {
    upper <- arg_group(NULL, lapply(fields$name[fields$shown], draw, shown = TRUE), lang = lang)
    lower <- lapply(fields$name[!fields$shown], draw, shown = FALSE)
  } else {
    # (the fields of no group first, no heading; then each group's, in the
    # definitions' order -- above and in the fold alike.  A group with no
    # field on one side is there, not seen: a field moved by the page
    # (a value given, a field emptied) goes to its own group)
    of <- function(g, shown) fields$name[fields$shown == shown &
                                           (if (is.null(g)) is.na(fields$group) else
                                             !is.na(fields$group) & fields$group == g$id)]
    all <- c(list(NULL), groups)
    upper <- lapply(all, function(g) {
      htmltools::tagAppendAttributes(
        arg_group(g$label, lapply(of(g, TRUE), draw, shown = TRUE), lang = lang),
        `data-group` = g$id %||% "")
    })
    lower <- c(lapply(of(NULL, FALSE), draw, shown = FALSE),
               lapply(groups, function(g) {
                 shiny::div(class = "rp-more-group", `data-group` = g$id,
                            shiny::div(class = "rp-group-label", .label_text(g$label, lang)),
                            lapply(of(g, FALSE), draw, shown = FALSE))
               }))
  }
  low <- fields$name[!fields$shown]
  shiny::div(
    class = paste(c("rp-plan-args", class), collapse = " "), `data-card` = id,
    shiny::div(class = "rp-args-up",
               `data-formals` = paste(if (is.null(only)) defs$formals else fields$name, collapse = ","),
               upper),
    arg_more(id, fn, schema = defs, lang = lang, rest = low, fields = lower, label = .spec_more_label))
}

# the fold's name, with the number of its fields: "More settings (4)"
.spec_more_label <- "More settings"

# The card read back: the arguments its fields say (an unticked check
# FALSE; "(none)": `!unset`; a field's R: `!r`); NULL while its fields are
# not on the page yet.  `ctx$cols`: the columns of a widths field, as drawn.
#' @noRd
spec_card_read <- function(input, id, fn, ctx = list(), defs = spec_card_defs(fn)) {
  parts <- .spec_part_args(defs)
  # (on the page: one of its main fields -- of all, for a function with none)
  probe <- if (length(defs$visible)) defs$visible else setdiff(defs$formals, .data_args)
  seen <- vapply(probe, function(k) {
    if (k %in% parts) return(isTRUE(.spec_parts[[defs$args[[k]]$type]]$seen(input, id, k, ctx)))
    !is.null(.input_value(input, paste0(id, "__", k))) ||
      isTRUE(.input_value(input, paste0(id, "__", k, "__isr")))
  }, NA)
  if (!any(seen)) return(NULL)
  out <- builder_card_read(input, id, defs, args = setdiff(defs$formals, c(parts, defs$fixed)),
                           logical_false = TRUE)
  for (k in intersect(names(out), names(defs$args))) {
    out[[k]] <- .form_list_value(out[[k]], defs$args[[k]]$type)
    if (identical(defs$args[[k]]$type, "formula")) out[[k]] <- .formula_text(out[[k]])
    if (isTRUE(defs$args[[k]]$dots)) out[[k]] <- .spec_dots_read(fn, k, out[[k]])
    out[[k]] <- .spec_drawn_r_read(defs$args[[k]], out[[k]])
  }
  for (k in parts) {
    v <- .spec_parts[[defs$args[[k]]$type]]$read(input, id, k, ctx)
    if (length(v)) out[[k]] <- v
  }
  out
}

# An argument drawn as R that is not R only (.spec_drawn_as(): its
# definition's type is another, plan_cell_style(border = ) a call): the field's R
# read back as the value it says -- a literal ("AEBODSYS", list(c("a",
# "b"))) its plain value, as it was drawn -- and only other R as R
.spec_drawn_r_read <- function(d, v) {
  if (!inherits(v, "tfl_r") || !"def_type" %in% names(d) || .type_only_r(d$def_type) ||
        !identical(.arg_kind(d$type), "r")) {
    return(v)
  }
  .text_value(as.character(unclass(v)))
}

# The item's whole arguments after the card's edit: its arguments (the
# report's over its layers') with what the card says otherwise put in, a
# field emptied taking its argument away (an argument the item does not
# say is the function's default: a check ticked as that default is no
# change).  .builder_item_write() keeps of them what differs from the layers.
#' @noRd
spec_card_args <- function(read, item, fn, defs = spec_card_defs(fn)) {
  eff <- item$args %||% list()
  # (a part not drawn -- a widths field with no columns known -- reads
  # nothing: no change)
  parts <- .spec_part_args(defs)
  for (a in setdiff(union(names(read), setdiff(intersect(names(eff), defs$formals),
                                              setdiff(parts, names(read)))), defs$fixed)) {
    lg <- identical(.arg_kind(defs$args[[a]]$type), "logical") && !inherits(read[[a]], "tfl_r")
    new <- read[[a]]
    cur <- eff[[a]]
    if (is.null(cur) && lg) cur <- defs$args[[a]]$default
    # (a text field drawn from "" reads back blank -- left out -- as one
    # never set would: "" is a value (plan_stub(heading = "") a blank
    # heading), so the field left blank keeps it)
    if (is.null(new) && identical(cur, "")) next
    if (.same_value(new, cur, lg)) next
    eff[a] <- list(new)
    if (is.null(new)) eff[[a]] <- NULL
  }
  eff
}
