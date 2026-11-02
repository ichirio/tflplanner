# The builder's cards, one set of parts for the tables, the figures and
# the listings: a card is one item of a report's `content:` -- one
# function call -- with its arguments as fields.  Every label is the
# meaning and the name: "Structure (table_plan())", "Columns (cols)", so
# the screen teaches the code it writes.  The parts:
#
#   builder_label(meaning, name, kind)   "meaning (name)"
#   builder_card(id, fn, ...)            the card: its head (the required bar
#                                        or the on/off switch, the label,
#                                        where the item came from, a summary,
#                                        the menu) and its body
#   builder_card_read(input, id, schema) the card's inputs as the item's args
#   arg_field(id, arg, type, ...)        one argument's field, by its type
#   arg_group(label, fields, as)         several fields on one row
#   arg_more(id, fn, used, schema)       the folded "other arguments"
#   from_badge(from)                     the mark of where a value came from
#   code_jump(lines_map, output_id)      Code line <-> card
#   nav_hint(text, where)                a sentence shown in nav mode only
#   add_item_input(id, kinds, schema)    the "add an item" field with its list
#
# An input's ID is `<card id>__<arg>`; a field's R text `<id>__<arg>__r` and
# its switch `<id>__<arg>__isr`; a mapping's entry `<id>__<arg>__<aes>`.
# The schema (tflspec::tfl_schema(fn)) may carry `label` (en / ja),
# `visible`, `groups` and `formals`; without them every argument is
# visible, there are no groups and the label is the name alone.

# ---- labels --------------------------------------------------------------------

# "meaning (name)": the meaning in the app's language, the name in a thin
# monospace; a function's name with its (), a package kept (pkg::fn)
#' @noRd
builder_label <- function(meaning, name, kind = c("fn", "arg"), lang = "en") {
  kind <- match.arg(kind)
  shown <- if (kind == "fn") paste0(name, "()") else name
  meaning <- .label_text(meaning, lang)
  # a meaning that says the name already ("Rounding (rounding)" in a
  # translation): the name once, in its own type
  if (!is.null(meaning)) {
    for (s in unique(c(shown, name))) {
      for (tail in c(paste0("(", s, ")"), paste0("\uff08", s, "\uff09"))) {
        if (endsWith(meaning, tail)) {
          meaning <- trimws(substr(meaning, 1L, nchar(meaning) - nchar(tail)))
        }
      }
    }
    if (!nzchar(meaning)) meaning <- NULL
  }
  if (is.null(meaning) || identical(meaning, name)) {
    return(shiny::tags$span(class = paste("rp-label", paste0("rp-label-", kind)),
                            shiny::tags$code(class = "rp-name rp-name-only", shown)))
  }
  shiny::tags$span(class = paste("rp-label", paste0("rp-label-", kind)),
                   shiny::tags$span(class = "rp-meaning", meaning), " ",
                   shiny::tags$code(class = "rp-name", paste0("(", shown, ")")))
}

# a schema label -- a list by language, or one English text -- in `lang`;
# NULL for none
.label_text <- function(label, lang = "en") {
  if (is.null(label)) return(NULL)
  if (is.list(label)) {
    x <- label[[lang]] %||% label$en %||% NULL
    if (is.null(x)) return(NULL)
    return(as.character(x))
  }
  if (!is.character(label) || !length(label) || !nzchar(label[[1L]])) return(NULL)
  tr(label[[1L]], lang)
}

# ---- the card ------------------------------------------------------------------

#' @noRd
builder_card <- function(id, fn, label = NULL, from = "own", required = FALSE, on = TRUE,
                         body = NULL, summary = NULL,
                         tools = c("up", "down", "copy", "remove", "r"), nav = NULL,
                         open = TRUE, error = NULL, path = id, lang = "en",
                         switchable = TRUE, revert = !identical(from, "own"),
                         own_label = NULL, absent = FALSE, also = NULL, rare = FALSE,
                         section = NULL, tag = NULL, new = FALSE, empty = NULL) {
  t <- function(x) tr(x, lang)
  cls <- c("rp-card", if (required) "rp-card-required", if (!on) "rp-card-off",
           if (isTRUE(absent)) "rp-card-absent", if (isTRUE(rare)) "rp-card-rare",
           if (open) "rp-card-open", if (!is.null(error)) "rp-card-error",
           if (isTRUE(new)) "rp-card-new")
  # the switch only where on / off works (`switchable`): a switch that
  # cannot be turned off would look on and mislead -- such a card has none
  left <- if (required) {
    shiny::tags$span(class = "rp-card-bar", title = t("Required"))
  } else if (isTRUE(switchable)) {
    shiny::tags$span(class = "form-check form-switch rp-card-switch",
                     shiny::tags$input(type = "checkbox", class = "form-check-input",
                                       id = paste0(id, "__on"),
                                       checked = if (on) NA else NULL,
                                       title = t("On or off: off writes nothing")))
  }
  # Revert: an item of the report's own over the defaults' (`revert`)
  revert <- if (isTRUE(revert)) .card_revert(id, lang)
  menu <- .card_menu(id, tools, lang)
  head <- shiny::div(
    class = "rp-card-head",
    left,
    # (its arrow is the CSS's: right when closed, down when open)
    shiny::tags$button(type = "button", class = "rp-card-toggle", `aria-label` = t("Open or close"),
                       `data-card` = id),
    shiny::tags$span(class = "rp-card-title", builder_label(label, fn, "fn", lang)),
    # a required card says so, faint, beside its name
    if (required) shiny::tags$span(class = "rp-card-req", t("Required")),
    # an item the code has not yet (`absent`): what its form would add
    if (isTRUE(absent)) {
      shiny::tags$span(class = "rp-badge rp-badge-absent",
                       title = t("A value set here adds it to the code."), t("Not in the code yet"))
    } else from_badge(from, lang, own = own_label),
    # a card shown with nothing in it yet (`empty`, its words)
    if (!is.null(empty)) shiny::tags$span(class = "rp-badge rp-badge-absent", t(empty)),
    revert,
    if (!is.null(summary)) shiny::tags$span(class = "rp-card-summary", summary),
    if (!is.null(error)) shiny::tags$span(class = "rp-card-err", "\u26a0 ", error),
    # its group (`tag`: Mapping, Finishing ...), faint at the right
    if (!is.null(tag)) shiny::tags$span(class = "rp-card-stage", t(tag)),
    menu)
  shiny::div(
    class = paste(cls, collapse = " "), id = id, `data-card` = id, `data-fn` = fn,
    `data-path` = path,
    # `also`: other items' paths whose code lines its fields light too (the
    # decimals: plan_cells()'s formats hold them); `section`: its group
    `data-also` = if (length(also)) paste(also, collapse = " "),
    `data-section` = section,
    head,
    shiny::div(class = "rp-card-body",
               if (!is.null(nav)) nav_hint(nav, "group", lang),
               body))
}

# a card's Revert button
.card_revert <- function(id, lang = "en") {
  shiny::tags$button(type = "button", id = paste0(id, "__revert"),
                     class = "btn btn-link btn-sm rp-revert action-button",
                     title = tr("Back to the study default", lang), tr("Revert", lang))
}

# the card's menu: up, down, duplicate, remove, edit as R
.card_menu <- function(id, tools, lang = "en") {
  if (!length(tools)) return(NULL)
  t <- function(x) tr(x, lang)
  labels <- c(up = "Move up", down = "Move down", copy = "Duplicate",
              remove = "Remove", r = "Edit as R")
  tools <- intersect(tools, names(labels))
  items <- lapply(tools, function(k) {
    shiny::tags$li(shiny::tags$button(
      type = "button", id = paste0(id, "__", k),
      class = paste("dropdown-item action-button", paste0("rp-tool-", k)), t(labels[[k]])))
  })
  shiny::div(class = "dropdown rp-card-menu",
             shiny::tags$button(type = "button", class = "btn btn-sm btn-link rp-card-dots",
                                `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                                `aria-label` = t("More"), "\u22ee"),
             shiny::tags$ul(class = "dropdown-menu dropdown-menu-end", items))
}

# The card's inputs read back as the item's arguments: a named list, an
# argument left blank left out (not written).  NULL when the card is
# switched off.  `schema` gives the arguments and their types (`args`,
# `formals`, `aes`); an argument whose R switch is on is its R text.
# `logical_false`: an unticked check is FALSE (not left out).
#' @noRd
builder_card_read <- function(input, id, schema = NULL, args = NULL, logical_false = FALSE) {
  get <- function(...) .input_value(input, paste(c(id, ...), collapse = "__"))
  on <- get("on")
  if (isFALSE(on)) return(NULL)
  if (is.null(args)) {
    args <- unique(c(names(schema$args), schema$formals, schema$visible))
  }
  out <- list()
  extra <- list()
  for (a in setdiff(args, "...")) {
    ty <- schema$args[[a]]$type
    kind <- .arg_kind(ty)
    if (isTRUE(get(a, "isr"))) {
      txt <- get(a, "r")
      if (!is.null(txt) && nzchar(trimws(txt))) out[[a]] <- tflspec::tfl_r(trimws(txt))
      next
    }
    if (kind == "map") {
      aes <- .map_keys(schema, a)
      m <- list()
      for (k in aes) {
        if (isTRUE(get(a, k, "isval"))) {
          v <- get(a, k, "val")
          # a constant goes to the argument of that name, not to the mapping
          if (!is.null(v) && nzchar(v)) extra[[k]] <- .coerce_value(v, NULL)
          next
        }
        v <- get(a, k)
        if (!is.null(v) && length(v) && nzchar(v[[1L]])) m[[k]] <- v[[1L]]
      }
      if (length(m)) out[[a]] <- m
      next
    }
    v <- get(a)
    if (is.null(v)) next
    if (kind == "logical") {
      dflt <- schema$args[[a]]$default
      # unticked: FALSE when the default is TRUE, or when every logical is
      # wanted as it is (`logical_false`: a value over the study's default)
      if (isTRUE(v)) out[[a]] <- TRUE else if (isTRUE(dflt) || logical_false) out[[a]] <- FALSE
      next
    }
    if (kind %in% c("number", "integer")) {
      if (length(v) && !is.na(v) && nzchar(as.character(v))) {
        out[[a]] <- if (kind == "integer") as.integer(v) else as.numeric(v)
      }
      next
    }
    v <- v[nzchar(as.character(v))]
    if (!length(v)) next
    if (kind == "list") {
      out[[a]] <- as.character(v)
    } else if (is.null(ty)) {
      # no type known (a formal the schema does not describe): a literal
      # typed is its value, anything else R
      out[[a]] <- .text_value(trimws(as.character(v[[1L]])))
    } else if (kind == "r") {
      out[[a]] <- tflspec::tfl_r(trimws(as.character(v)))
    } else {
      out[[a]] <- .coerce_value(v[[1L]], ty)
    }
  }
  for (k in setdiff(names(extra), names(out))) out[[k]] <- extra[[k]]
  out
}

# text typed where no type is known: a literal is its value (TRUE, 3,
# "a", c(1, 2)), the rest R
.text_value <- function(txt) {
  e <- tryCatch(str2lang(txt), error = function(e) NULL)
  if (is.null(e)) return(tflspec::tfl_r(txt))
  .arg_from_r(e)
}

# an input's value: from a reactivevalues or a plain list (the tests)
.input_value <- function(input, name) {
  if (is.list(input)) return(input[[name]])
  input[[name]]
}

# a scalar typed as text, by its type: a number stays a number, true / false
# a logical, the rest text
.coerce_value <- function(v, ty) {
  kind <- .arg_kind(ty)
  if (kind %in% c("number", "integer")) {
    n <- suppressWarnings(as.numeric(v))
    return(if (is.na(n)) v else if (kind == "integer") as.integer(n) else n)
  }
  if (kind == "logical") return(isTRUE(as.logical(v)))
  if (kind %in% c("string", "enum", "column", "stat", "template", "colour", "shape")) {
    return(as.character(v))
  }
  if (is.character(v) && v %in% c("TRUE", "FALSE")) return(as.logical(v))
  n <- suppressWarnings(as.numeric(v))
  if (!is.na(n) && grepl("^[-+]?[0-9.]+$", v)) return(n)
  as.character(v)
}

# the keys of a map argument (a mapping's aes): the argument's `keys`, the
# schema's `aes`, else the usual ones
.map_keys <- function(schema, a) {
  k <- schema$args[[a]]$keys %||% schema$aes
  if (is.list(k)) k <- names(k)
  if (is.null(k)) k <- .aes_default
  as.character(k)
}

.aes_default <- c("x", "y", "colour", "fill", "group", "shape", "linetype", "label",
                  "ymin", "ymax", "xmin", "xmax")
