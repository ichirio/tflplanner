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
                         section = NULL, tag = NULL, new = FALSE) {
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
  revert <- if (isTRUE(revert)) {
    shiny::tags$button(type = "button", id = paste0(id, "__revert"),
                       class = "btn btn-link btn-sm rp-revert action-button",
                       title = t("Back to the study default"), t("Revert"))
  }
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

# ---- the type notation -> the kind of field ------------------------------------

# "list<number>|r" -> "list"; "enum[a|b]" -> "enum"; "map<aes, column|r>" ->
# "map"; "call<position_dodge>" -> "call"; NULL -> "r".  The first
# alternative decides, `r` last.
.arg_kind <- function(type) {
  if (is.null(type) || !nzchar(type)) return("r")
  alts <- .type_split(trimws(type), "|")
  alts <- c(setdiff(alts, "r"), intersect(alts, "r"))
  first <- alts[[1L]]
  if (grepl("^enum\\[", first)) return("enum")
  if (grepl("^(list|vec)<", first)) {
    inner <- sub("^(list|vec)<(.*)>$", "\\2", first)
    return(if (grepl("^column", inner) || grepl("^string", inner) ||
                 grepl("^number|^integer", inner)) "list" else "r")
  }
  if (grepl("^map<", first)) return("map")
  if (grepl("^call", first)) return("call")
  if (grepl("^rows<", first)) return("r")
  if (first %in% c("string", "integer", "number", "logical", "column", "stat",
                   "template", "colour", "shape", "object", "plot", "r")) {
    return(first)
  }
  "r"
}

# the entries of an enum[a|b]
.type_enum <- function(type) {
  alts <- .type_split(trimws(type), "|")
  e <- alts[grepl("^enum\\[", alts)]
  if (!length(e)) return(character())
  strsplit(sub("^enum\\[(.*)\\]$", "\\1", e[[1L]]), "|", fixed = TRUE)[[1L]]
}

# the function of a call<fn>; NULL for a plain call
.type_call_fn <- function(type) {
  alts <- .type_split(trimws(type), "|")
  e <- alts[grepl("^call<", alts)]
  if (!length(e)) return(NULL)
  fns <- strsplit(sub("^call<(.*)>$", "\\1", e[[1L]]), "|", fixed = TRUE)[[1L]]
  trimws(fns)
}

# split at `sep` outside < > and [ ]
.type_split <- function(x, sep) {
  ch <- strsplit(x, "")[[1L]]
  depth <- 0L
  out <- character()
  cur <- ""
  for (c in ch) {
    if (c %in% c("<", "[")) depth <- depth + 1L
    if (c %in% c(">", "]")) depth <- depth - 1L
    if (c == sep && depth == 0L) {
      out <- c(out, cur)
      cur <- ""
    } else {
      cur <- paste0(cur, c)
    }
  }
  trimws(c(out, cur))
}

# ---- one argument's field -------------------------------------------------------

# One argument as a field chosen by its type: a column is a select of the
# data's columns, an enum a select, a number a number field with the
# default as its placeholder, a logical a check, a string a text field
# with the data's suggestions, a colour a select with swatches, a shape a
# select with the shapes, a call<fn> a small form of that function's
# visible arguments (or R), r a monospace field, map<aes, ...> a row of
# aes fields each with a column / value switch.  Beside every field: the
# `R` switch (the value as R) and the (i) of its help (what the argument
# means).
#' @noRd
arg_field <- function(id, arg, type = NULL, label = NULL, value = NULL, default = NULL,
                      choices = NULL, from = "own", help = NULL, required = FALSE,
                      lang = "en", fn = NULL, aes = NULL, nav = NULL, help_link = NULL,
                      own = NULL) {
  t <- function(x) tr(x, lang)
  kind <- .arg_kind(type)
  fid <- paste0(id, "__", arg)
  is_r <- inherits(value, "tfl_r")
  r_text <- if (is_r) as.character(unclass(value)) else ""
  if (is_r) value <- NULL
  if (inherits(value, "tfl_unset")) value <- NULL
  control <- switch(
    kind,
    column = .field_select(fid, choices, value, multiple = FALSE, lang = lang, placeholder = default),
    list = .field_select(fid, choices, value, multiple = TRUE, lang = lang, placeholder = default),
    enum = .field_enum(fid, .type_enum(type), value, default, lang, labels = .enum_labels[[arg]]),
    number = .field_number(fid, value, default, step = "any"),
    integer = .field_number(fid, value, default, step = "1", min = "0"),
    logical = .field_check(fid, value, default),
    string = .field_text(fid, value, default, choices),
    template = .field_text(fid, value, default, choices, mono = TRUE),
    stat = .field_select(fid, choices, value, multiple = FALSE, lang = lang, placeholder = default),
    object = .field_select(fid, choices, value, multiple = FALSE, lang = lang, placeholder = default),
    plot = .field_select(fid, choices, value, multiple = TRUE, lang = lang, placeholder = default),
    colour = .field_colour(fid, choices, value, default, lang),
    shape = .field_shape(fid, choices, value, default, lang),
    call = .field_call(fid, .type_call_fn(type), value, default, lang),
    map = .field_map(fid, aes %||% .aes_default, value, choices, lang),
    .field_r(fid, value, default))
  r_input <- shiny::tags$input(type = "text", id = paste0(fid, "__r"), class = "form-control form-control-sm rp-mono rp-r-input",
                               value = r_text, placeholder = "R", spellcheck = "false")
  r_switch <- shiny::tags$label(
    class = "rp-r-toggle", title = t("As R"),
    shiny::tags$input(type = "checkbox", id = paste0(fid, "__isr"), class = "rp-isr",
                      checked = if (is_r) NA else NULL),
    shiny::tags$span("R"))
  cls <- c("rp-field", paste0("rp-field-", kind), if (required) "rp-field-required",
           if (is_r) "rp-field-r", if (nzchar(r_text) || .has_value(value)) "rp-has-value")
  shiny::div(
    class = paste(cls, collapse = " "), `data-arg` = arg, `data-field` = fid,
    shiny::tags$label(class = "rp-field-label", `for` = fid,
                      builder_label(label, arg, "arg", lang),
                      if (required) shiny::tags$span(class = "rp-req", title = t("Required"), "*"),
                      from_badge(from, lang, own = own),
                      help_tip(help), help_link),
    shiny::div(class = "rp-field-row",
               shiny::div(class = "rp-form-input", control),
               r_input, r_switch),
    if (!is.null(nav)) nav_hint(nav, "field", lang))
}

.has_value <- function(v) {
  if (is.null(v)) return(FALSE)
  if (is.list(v)) return(length(v) > 0L)
  any(nzchar(as.character(v)) & !is.na(v))
}

# a select of the data's columns (or any choices), one or several
.field_select <- function(fid, choices, value, multiple = FALSE, lang = "en", placeholder = NULL) {
  choices <- .choices_with(choices, value)
  ph <- if (!is.null(placeholder)) as.character(placeholder)[[1L]] else tr("Choose", lang)
  opts <- list(placeholder = ph)
  if (multiple) opts$plugins <- list("remove_button")
  shiny::selectizeInput(fid, NULL, choices = c(stats::setNames("", ""), choices),
                        selected = value, multiple = multiple, width = "100%", options = opts)
}

# a value the choices lack is kept among them
.choices_with <- function(choices, value) {
  if (is.null(choices)) choices <- character()
  if (!is.null(value) && is.atomic(value)) {
    v <- as.character(value)
    v <- v[nzchar(v) & !v %in% choices]
    if (length(v)) choices <- c(choices, stats::setNames(v, v))
  }
  choices
}

# An enum: a select of its entries, the default named so ("(default)") and
# a blank first (blank: the default, not written); a value outside the
# entries (an older spec's) is kept as one of them, not lost
.field_enum <- function(fid, entries, value, default, lang = "en", labels = NULL) {
  v <- if (!is.null(value) && length(value)) as.character(value)[[1L]] else ""
  if (nzchar(v) && !v %in% entries) entries <- c(entries, v)
  # (a value's label where the argument names one: rounding's "sas" SAS)
  map <- labels
  labels <- entries
  if (length(map)) labels[entries %in% names(map)] <- unname(map[entries[entries %in% names(map)]])
  if (!is.null(default) && length(default) == 1L && default %in% entries) {
    labels[entries == default] <- paste0(labels[entries == default], " ", tr("(default)", lang))
  }
  shiny::selectInput(fid, NULL, choices = c(stats::setNames("", ""), stats::setNames(entries, labels)),
                     selected = v, width = "100%", selectize = FALSE)
}

# The labels of an argument's choices where the values are words of their
# own (the YAML keeps the values)
.enum_labels <- list(rounding = c(sas = "SAS", r = "R", iec60559 = "IEC 60559"))

# A number: a number field, the default as its placeholder (blank: the
# default, not written); an integer steps by 1 (`min`: the least it takes)
.field_number <- function(fid, value, default, step = "any", min = NULL) {
  v <- if (!is.null(value) && length(value) && !is.na(value[[1L]])) as.character(value[[1L]])
  shiny::tags$input(type = "number", id = fid, class = "form-control form-control-sm rp-number",
                    value = v,
                    placeholder = if (!is.null(default) && length(default)) as.character(default[[1L]]) else NULL,
                    step = step, min = min,
                    inputmode = if (identical(step, "1")) "numeric" else "decimal")
}

# A logical: a check, ticked as the value says (the default when there is
# none)
.field_check <- function(fid, value, default) {
  on <- if (!is.null(value) && length(value)) isTRUE(as.logical(value[[1L]])) else isTRUE(default)
  shiny::tags$span(class = "form-check form-switch rp-check",
                   shiny::tags$input(type = "checkbox", class = "form-check-input", id = fid,
                                     checked = if (on) NA else NULL))
}

# a text field; the data's suggestions as a datalist
.field_text <- function(fid, value, default, choices = NULL, mono = FALSE) {
  dl <- if (length(choices)) paste0(fid, "__dl")
  shiny::tagList(
    shiny::tags$input(type = "text", id = fid,
                      class = paste("form-control form-control-sm", if (mono) "rp-mono"),
                      value = if (!is.null(value)) as.character(value)[[1L]] else NULL,
                      placeholder = if (!is.null(default)) as.character(default) else NULL,
                      list = dl),
    if (!is.null(dl)) shiny::tags$datalist(id = dl, lapply(unname(choices), function(x) shiny::tags$option(value = x))))
}

.field_r <- function(fid, value, default) {
  v <- if (inherits(value, "tfl_r")) as.character(unclass(value)) else if (!is.null(value)) .arg_to_r(value)
  shiny::tags$input(type = "text", id = fid, class = "form-control form-control-sm rp-mono",
                    value = v, placeholder = if (!is.null(default)) .arg_to_r(default) else NULL,
                    spellcheck = "false")
}

# the usual named colours when none are given (a company palette comes as `choices`)
.colour_default <- c("black", "grey50", "grey70", "red", "blue", "darkgreen", "orange", "purple")

# a select of colours, each with its swatch (selectize draws the option)
.field_colour <- function(fid, choices, value, default, lang = "en") {
  choices <- choices %||% stats::setNames(.colour_default, .colour_default)
  choices <- .choices_with(choices, value)
  render <- I(paste0(
    "{option: function(item, escape) {",
    "  return '<div><span class=\"rp-swatch\" style=\"background:' + escape(item.value) + '\"></span>' + escape(item.label) + '</div>'; },",
    " item: function(item, escape) {",
    "  return '<div><span class=\"rp-swatch\" style=\"background:' + escape(item.value) + '\"></span>' + escape(item.label) + '</div>'; }}"))
  shiny::selectizeInput(fid, NULL, choices = c(stats::setNames("", ""), choices), selected = value,
                        width = "100%",
                        options = list(placeholder = if (!is.null(default)) as.character(default) else tr("Choose", lang),
                                       # (a colour typed: #CC0000)
                                       create = TRUE, render = render))
}

# ggplot2's shapes by number, each shown with its glyph
.shape_choices <- c("16" = "\u25cf circle (16)", "1" = "\u25cb open circle (1)",
                    "15" = "\u25a0 square (15)", "0" = "\u25a1 open square (0)",
                    "17" = "\u25b2 triangle (17)", "2" = "\u25b3 open triangle (2)",
                    "18" = "\u25c6 diamond (18)", "5" = "\u25c7 open diamond (5)",
                    "4" = "\u00d7 cross (4)", "3" = "+ plus (3)", "8" = "\u2733 asterisk (8)")

.field_shape <- function(fid, choices, value, default, lang = "en") {
  if (is.null(choices)) choices <- stats::setNames(names(.shape_choices), .shape_choices)
  choices <- .choices_with(choices, if (!is.null(value)) as.character(value))
  shiny::selectInput(fid, NULL, choices = c(stats::setNames("", ""), choices),
                     selected = if (!is.null(value)) as.character(value) else "",
                     width = "100%", selectize = FALSE)
}

# a call<fn>: the function's visible arguments as a small form (the schema
# knows them), else R
.field_call <- function(fid, fns, value, default, lang = "en") {
  fn <- if (length(fns)) fns[[1L]]
  ent <- if (!is.null(fn)) .schema_of(fn)
  vis <- ent$visible
  if (is.null(fn) || !length(vis)) return(.field_r(fid, value, default))
  args <- if (inherits(value, "tfl_call")) value$args else list()
  shiny::div(class = "rp-call", `data-fn` = fn,
             shiny::tags$code(class = "rp-name", paste0(fn, "(")),
             lapply(vis, function(a) {
               arg_field(fid, a, ent$args[[a]]$type, label = ent$args[[a]]$label,
                         value = args[[a]], default = ent$args[[a]]$default,
                         help = .schema_help(fn, a), lang = lang)
             }),
             shiny::tags$code(class = "rp-name", ")"))
}

# a mapping: one row an aes -- the column, or (the switch) a constant value
.field_map <- function(fid, aes, value, choices, lang = "en") {
  t <- function(x) tr(x, lang)
  value <- value %||% list()
  shiny::div(class = "rp-map", lapply(aes, function(k) {
    v <- value[[k]]
    is_val <- !is.null(v) && !is.null(attr(v, "rp_value", exact = TRUE))
    kid <- paste0(fid, "__", k)
    shiny::div(
      class = paste("rp-map-row", if (is_val) "rp-map-val"), `data-aes` = k,
      shiny::tags$span(class = "rp-map-key", k),
      shiny::div(class = "rp-map-col",
                 .field_select(kid, choices, if (!is_val) v, lang = lang)),
      shiny::tags$input(type = "text", id = paste0(kid, "__val"),
                        class = "form-control form-control-sm rp-map-value",
                        value = if (is_val) as.character(v) else NULL,
                        placeholder = t("Value")),
      shiny::tags$label(class = "rp-map-switch", title = t("A column of the data, or a constant value"),
                        shiny::tags$input(type = "checkbox", id = paste0(kid, "__isval"), class = "rp-isval",
                                          checked = if (is_val) NA else NULL),
                        shiny::tags$span(class = "rp-map-col-lab", t("Column")),
                        shiny::tags$span(class = "rp-map-val-lab", t("Value"))))
  }))
}

# ---- groups and the folded arguments -------------------------------------------

# Several fields on one row: the schema's `groups:` (a label, the arguments,
# `as` -- how they are laid out, a word a field: "min max by").
#' @noRd
arg_group <- function(label = NULL, fields, as = NULL, lang = "en") {
  caps <- if (!is.null(as)) strsplit(trimws(as), "[[:space:]]+")[[1L]]
  fields <- lapply(seq_along(fields), function(i) {
    f <- fields[[i]]
    if (!is.null(caps) && length(caps) == length(fields)) {
      shiny::div(class = "rp-group-cell", `data-as` = caps[[i]], f)
    } else {
      shiny::div(class = "rp-group-cell", f)
    }
  })
  shiny::div(class = "rp-group", `data-as` = as,
             if (!is.null(label)) shiny::div(class = "rp-group-label", .label_text(label, lang)),
             shiny::div(class = "rp-group-row", fields))
}

# The arguments not shown: the function's formals less the visible ones
# and the ones in use, folded under "Other arguments (n)" -- a search
# field, the names in the function's order, each a field labelled
# "meaning (name)" (the name alone when no meaning is known), its (i)
# what the argument means (.arg_help()) or a link to the package's help.
# A field given a value stays shown
# when the fold closes (it is promoted; the next render passes it as used).
#' @noRd
arg_more <- function(id, fn, used = character(), schema = NULL, lang = "en",
                     values = list(), choices = NULL, rest = NULL, fields = NULL,
                     label = "Other arguments") {
  t <- function(x) tr(x, lang)
  schema <- schema %||% .schema_of(fn)
  all <- .fn_formals(fn, schema)
  # (in the function's order, as its formals; `rest` and their `fields`
  # given: those)
  rest <- rest %||% setdiff(all, c(schema$visible, used, "..."))
  if (!length(rest)) return(NULL)
  pkg <- schema$package %||% if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn)
  fields <- fields %||% lapply(rest, function(a) {
    help <- .arg_help(fn, a, schema, lang)
    link <- if (is.null(help) && !is.null(pkg)) {
      shiny::tags$a(class = "rp-help-link", href = .help_url(pkg, fn), target = "_blank",
                    rel = "noopener", title = t("The package's help"),
                    paste0("?", pkg, "::", sub("^.*::", "", fn)))
    }
    arg_field(id, a, schema$args[[a]]$type, label = .arg_meaning(fn, a), value = values[[a]],
              default = schema$args[[a]]$default, help = help, help_link = link,
              choices = choices, lang = lang)
  })
  shiny::div(
    class = "rp-more", `data-card` = id,
    shiny::tags$button(type = "button", class = "rp-more-toggle",
                       shiny::tags$span(class = "rp-more-arrow", "\u25b8"), " ",
                       sprintf("%s (%d)", t(label), length(rest))),
    shiny::div(class = "rp-more-body",
               shiny::tags$input(type = "search", class = "form-control form-control-sm rp-more-search",
                                 id = paste0(id, "__more_q"), placeholder = t("Search arguments")),
               fields))
}

# the function's arguments: the schema's `formals`, else the installed
# function's, else the schema's `args`
.fn_formals <- function(fn, schema = NULL) {
  f <- schema$formals
  if (!is.null(f)) return(if (is.list(f)) names(f) else as.character(f))
  pkg <- if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn) else schema$package
  nm <- sub("^.*::", "", fn)
  if (!is.null(pkg) && requireNamespace(pkg, quietly = TRUE)) {
    ns <- asNamespace(pkg)
    if (exists(nm, envir = ns, inherits = FALSE)) {
      fo <- formals(get(nm, envir = ns))
      if (!is.null(fo)) return(names(fo))
    }
  }
  names(schema$args) %||% character()
}

.help_url <- function(pkg, fn) {
  sprintf("https://rdrr.io/cran/%s/man/%s.html", pkg, sub("^.*::", "", fn))
}

# The arguments `args` in the order of the function's arguments
# (`formals`: .fn_formals()'s), those not among them after, as they came.
# Every card's fields are in this order.
.arg_order <- function(args, formals) {
  args[order(match(args, formals, nomatch = length(formals) + 1L), seq_along(args))]
}

# An argument's help in the (i): what it means, when it is used, an
# example -- tflplanner's own text (.plan_arg_help), else the schema's,
# else tflspec's -- in the language; NULL for none
.arg_help <- function(fn, arg, schema = NULL, lang = "en") {
  own <- .plan_arg_help[[fn]]
  h <- if (!is.null(own) && arg %in% names(own)) own[[arg]]
  h <- h %||% schema$args[[arg]]$description %||% .schema_help(fn, arg)
  if (is.null(h)) NULL else tr(h, lang)
}

# the schema of a function, when tflspec has one
.schema_of <- function(fn) {
  if (is.null(fn) || !requireNamespace("tflspec", quietly = TRUE)) return(NULL)
  tryCatch(tflspec::tfl_schema(fn), error = function(e) NULL)
}

# an argument's help text, NULL for none
.schema_help <- function(fn, arg) {
  if (!requireNamespace("tflspec", quietly = TRUE)) return(NULL)
  h <- tryCatch(tflspec::tfl_schema_help(fn, arg), error = function(e) NA_character_)
  if (is.null(h) || length(h) != 1L || is.na(h) || !nzchar(h)) NULL else h
}

# ---- where a value came from ---------------------------------------------------

# The mark of an item's or a value's source: own has none; `defaults` the
# study's defaults, `standards` the company standards, `pattern:<name>` a
# pattern, `template:<name>` a template, `ard` taken from the ARD (changed,
# it is the report's own, and Revert takes it back to the ARD's).
#' @noRd
from_badge <- function(from, lang = "en", own = NULL) {
  if (is.null(from) || !length(from) || !nzchar(from)) return(NULL)
  t <- function(x) tr(x, lang)
  # the report's own: its mark when the caller names it ("This table"),
  # none otherwise
  if (identical(from, "own")) {
    if (is.null(own)) return(NULL)
    return(shiny::tags$span(class = "rp-badge rp-badge-own", `data-from` = "own", t(own)))
  }
  kind <- sub(":.*$", "", from)
  name <- if (grepl(":", from, fixed = TRUE)) sub("^[^:]*:", "", from) else NULL
  text <- switch(kind,
                 defaults = t("Study default"),
                 standards = t("Company standard"),
                 pattern = paste(t("Pattern"), name),
                 template = t("Template"),
                 ard = t("From the ARD"),
                 from)
  shiny::tags$span(class = paste("rp-badge", paste0("rp-badge-", kind)), `data-from` = from,
                   title = if (!is.null(name)) name else NULL, text)
}

# ---- Code line <-> card -----------------------------------------------------------

# The table of the code's lines to the items that wrote them (what
# tfl_content_code() returns: a line number -> an item's path, "<bundle>/
# <n>" or an id), for the Code tab's view `output_id`: a click on a line
# selects and opens its card, a card's field in focus lights its lines.
# `lines_map`: a data frame (`line`, `path`), or a vector of paths named
# by line.
#' @noRd
code_jump <- function(lines_map, output_id) {
  if (is.data.frame(lines_map)) {
    m <- stats::setNames(as.list(as.character(lines_map$path)), as.character(lines_map$line))
  } else {
    m <- as.list(stats::setNames(as.character(lines_map), names(lines_map)))
  }
  shiny::tags$script(type = "application/json", class = "rp-code-jump", `data-for` = output_id,
                     shiny::HTML(jsonlite::toJSON(m, auto_unbox = TRUE)))
}

# ---- nav mode -----------------------------------------------------------------------

# A sentence of the guided (nav) mode: in the HTML in both modes, shown
# only when <html> has the nav class (no redraw).  At a group's head, or
# under a field.
#' @noRd
nav_hint <- function(text, where = c("group", "field"), lang = "en") {
  where <- match.arg(where)
  if (is.null(text) || !length(text)) return(NULL)
  if (is.character(text)) text <- tr(text, lang)
  shiny::div(class = paste("rp-nav-hint", paste0("rp-nav-", where)), text)
}

# ---- adding an item --------------------------------------------------------------------

# The "add an item" field: a selectize of the schema's functions, narrowed
# to `kinds` (a figure entry's `figure:`, or "plan" for a table's verbs),
# each labelled "meaning (fn)", grouped by kind; `pkg::fn` typed is taken
# as it is.  Enter adds (the button `<id>__add`), and the first field of
# the new card gets the focus.
#' @noRd
add_item_input <- function(id, kinds = NULL, schema = NULL, lang = "en", placeholder = NULL) {
  t <- function(x) tr(x, lang)
  schema <- schema %||% (if (requireNamespace("tflspec", quietly = TRUE)) {
    tryCatch(tflspec::tfl_schema(), error = function(e) NULL)
  })
  entries <- .item_choices(schema, kinds, lang)
  shiny::div(
    class = "rp-add-item", `data-add` = id,
    shiny::selectizeInput(
      id, NULL, choices = c(stats::setNames("", ""), entries), selected = "", width = "100%",
      options = list(create = TRUE, createOnBlur = FALSE, maxItems = 1,
                     placeholder = placeholder %||% t("Add an item (type a function name)"),
                     optgroupField = "kind", labelField = "label", searchField = list("label", "value"))),
    shiny::tags$button(type = "button", id = paste0(id, "__add"),
                       class = "btn btn-sm btn-outline-secondary action-button rp-add-btn", t("Add")))
}

# the functions offered: a named vector (value fn, name "meaning (fn)"),
# in the schema's rank
.item_choices <- function(schema, kinds = NULL, lang = "en") {
  if (!length(schema)) return(character())
  nms <- names(schema)
  keep <- vapply(nms, function(n) {
    e <- schema[[n]]
    if (is.null(kinds)) return(TRUE)
    if ("plan" %in% kinds && grepl("^(table_plan$|plan_)", n)) return(TRUE)
    fig <- e$figure
    !is.null(fig) && any(fig %in% kinds)
  }, NA)
  nms <- nms[keep]
  if (!length(nms)) return(character())
  rank <- vapply(nms, function(n) as.numeric(schema[[n]]$rank %||% 9999), 0)
  nms <- nms[order(rank, nms)]
  labels <- vapply(nms, function(n) {
    m <- .label_text(schema[[n]]$label, lang)
    if (is.null(m)) paste0(n, "()") else paste0(m, " (", n, "())")
  }, "")
  stats::setNames(nms, labels)
}

# ---- style and script -----------------------------------------------------------------------

.builder_cards_css <- "
.rp-card { border: 1px solid var(--bs-border-color, #dee2e6); border-radius: .5rem;
  margin-bottom: .6rem; background: #fff; }
.rp-card.rp-card-required { border-left: 4px solid var(--bs-primary, #0d6efd); }
.rp-card.rp-card-off { opacity: .6; }
.rp-card.rp-card-off .rp-card-body { display: none; }
.rp-card.rp-card-selected { box-shadow: 0 0 0 2px var(--bs-primary, #0d6efd); }
.rp-card.rp-card-error { border-color: #dc3545; }
.rp-card-head { display: flex; align-items: center; gap: .4rem; padding: .35rem .6rem;
  flex-wrap: wrap; }
.rp-card-bar { width: .25rem; align-self: stretch; margin-left: -.25rem; }
.rp-card-switch { margin: 0; padding: 0; min-height: 0; }
.rp-card-switch .form-check-input { margin: 0; cursor: pointer; }
.rp-card-toggle { border: 0; background: none; padding: 0 .2rem; color: #6b7280;
  font-size: 13px; width: 1.1em; }
.rp-card-toggle::before { content: '\\25b8'; }
.rp-card.rp-card-open > .rp-card-head .rp-card-toggle::before { content: '\\25be'; }
.rp-card-title { font-weight: 600; }
.rp-card-req { font-size: 11px; color: #9ca3af; font-weight: normal; }
.rp-card-sep { display: flex; align-items: center; gap: .5rem; margin: .7rem 0 .4rem;
  font-size: 11.5px; color: #6b7280; letter-spacing: .02em; }
.rp-card-sep::after { content: ''; flex: 1 1 auto; border-top: 1px solid #e5e7eb; }
.rp-cards-bar { display: flex; align-items: center; gap: .4rem; margin-bottom: .4rem; }
.rp-cards-bar .dropdown-menu { max-height: 60vh; overflow-y: auto; font-size: 13px; }
.rp-cards-bar .dropdown-item.rp-layer-rare { color: #6b7280; }
.rp-card.rp-card-rare > .rp-card-head { padding-top: .2rem; padding-bottom: .2rem; }
.rp-card-note { font-size: 12.5px; color: #4b5563; margin: .1rem 0 .35rem; }
.rp-card-summary { color: #6b7280; font-size: 12px; margin-left: .2rem; }
.rp-card.rp-card-open .rp-card-summary { display: none; }
.rp-card-err { color: #dc3545; font-size: 12px; }
.rp-card-menu { margin-left: auto; }
.rp-card-dots { color: #6b7280; text-decoration: none; padding: 0 .3rem; font-size: 16px; }
.rp-card-body { padding: .2rem .8rem .6rem; }
.rp-card:not(.rp-card-open) .rp-card-body { display: none; }
.rp-label .rp-name { font-family: Consolas, 'Courier New', monospace; font-size: .85em;
  color: #6b7280; font-weight: normal; background: none; padding: 0; }
.rp-label .rp-name-only { color: #374151; font-size: .95em; }
.rp-badge { display: inline-block; font-size: 11px; line-height: 1.3; padding: 0 .4rem;
  border-radius: .6rem; background: var(--bs-tertiary-bg, #f3f4f6); color: #4b5563;
  border: 1px solid #e5e7eb; white-space: nowrap; vertical-align: middle; }
.rp-badge-own { background: #ecfdf5; border-color: #a7f3d0; color: #047857; }
.rp-badge-absent { background: none; border-style: dashed; color: #6b7280; }
.rp-card.rp-card-absent { border-style: dashed; }
.rp-card.rp-card-absent > .rp-card-head .rp-card-title { color: #6b7280; }
.rp-badge-standards { background: #eef6ff; border-color: #cfe2ff; color: #1d4ed8; }
.rp-badge-template { background: #f5f3ff; border-color: #ddd6fe; color: #6d28d9; }
.rp-badge-ard { background: #fff7ed; border-color: #fed7aa; color: #9a3412; }
.rp-card-stage { font-size: 10.5px; color: #9ca3af; margin-left: auto; white-space: nowrap; }
.rp-card-stage + .rp-card-menu { margin-left: 0; }
.rp-card-sep-note { color: #9ca3af; letter-spacing: 0; }
.rp-revert { padding: 0 .3rem; font-size: 12px; text-decoration: none; }
.rp-field { margin-bottom: .35rem; min-width: 0; }
.rp-field-label { font-size: 12.5px; margin-bottom: .1rem; display: flex; align-items: center;
  gap: .25rem; flex-wrap: wrap; }
.rp-field-label .rp-req { color: #dc3545; font-weight: 600; }
.rp-field-label .rp-help-link { font-family: Consolas, 'Courier New', monospace; font-size: 11px;
  color: #6b7280; text-decoration: none; }
.rp-field-row { display: flex; align-items: center; gap: .3rem; }
.rp-field-row > .rp-form-input { flex: 1 1 auto; min-width: 0; }
.rp-field-row > .rp-r-input { flex: 1 1 auto; min-width: 0; display: none; }
.rp-field.rp-field-r > .rp-field-row > .rp-form-input { display: none; }
.rp-field.rp-field-r > .rp-field-row > .rp-r-input { display: block; }
.rp-r-toggle { margin: 0; cursor: pointer; font-size: 11px; font-family: Consolas, 'Courier New', monospace;
  color: #9ca3af; border: 1px solid #e5e7eb; border-radius: .25rem; padding: 0 .3rem; line-height: 1.5; }
.rp-r-toggle input { display: none; }
.rp-field.rp-field-r .rp-r-toggle { color: #fff; background: var(--bs-primary, #0d6efd);
  border-color: var(--bs-primary, #0d6efd); }
.rp-field .shiny-input-container { width: 100% !important; margin-bottom: 0; }
.rp-field .form-control, .rp-field .form-select, .rp-field .selectize-input {
  padding: 2px 6px; font-size: 13px; min-height: 0; }
.rp-mono { font-family: Consolas, 'Courier New', monospace; }
.rp-check { margin: 0; padding: 0; min-height: 0; }
.rp-check .form-check-input { margin: 0; }
.rp-swatch { display: inline-block; width: .9em; height: .9em; border: 1px solid #9ca3af;
  border-radius: .15rem; margin-right: .35rem; vertical-align: -.1em; }
.rp-call { border-left: 2px solid #e5e7eb; padding-left: .5rem; }
.rp-map-row { display: grid; grid-template-columns: 4.5em minmax(0, 1fr) auto; gap: .3rem;
  align-items: center; margin-bottom: .2rem; }
.rp-map-key { font-family: Consolas, 'Courier New', monospace; font-size: 12px; color: #374151; }
.rp-map-row .rp-map-value { display: none; }
.rp-map-row.rp-map-val .rp-map-col { display: none; }
.rp-map-row.rp-map-val .rp-map-value { display: block; }
.rp-map-switch { margin: 0; cursor: pointer; font-size: 11px; color: #6b7280;
  border: 1px solid #e5e7eb; border-radius: .25rem; padding: 0 .3rem; white-space: nowrap; }
.rp-map-switch input { display: none; }
.rp-map-switch .rp-map-val-lab, .rp-map-row.rp-map-val .rp-map-switch .rp-map-col-lab { display: none; }
.rp-map-row.rp-map-val .rp-map-switch .rp-map-val-lab { display: inline; }
.rp-group { margin-bottom: .35rem; }
.rp-group-label { font-size: 12.5px; font-weight: 600; color: #4b5563; margin-bottom: .1rem; }
.rp-group-row { display: flex; gap: .6rem; flex-wrap: wrap; }
.rp-group-row > .rp-group-cell { flex: 1 1 8em; min-width: 0; }
.rp-more { margin-top: .3rem; }
.rp-more-toggle { border: 0; background: none; padding: 0; font-size: 12.5px; color: #6b7280; }
.rp-more-arrow { display: inline-block; transition: transform .15s; }
.rp-more.rp-open .rp-more-arrow { transform: rotate(90deg); }
.rp-more-body { padding-left: .6rem; margin-top: .2rem; }
.rp-more:not(.rp-open) .rp-more-search { display: none; }
.rp-more:not(.rp-open) .rp-more-body > .rp-field:not(.rp-has-value) { display: none; }
.rp-more-search { margin-bottom: .3rem; max-width: 18em; }
.rp-more .rp-field.rp-hidden { display: none !important; }
.rp-nav-hint { font-size: 12.5px; color: #4b5563; background: var(--bs-tertiary-bg, #f6f7f9);
  border-left: 3px solid #cbd5e1; padding: .3rem .6rem; margin: .2rem 0 .5rem; border-radius: 0 .3rem .3rem 0; }
.rp-nav-hint.rp-nav-field { font-size: 12px; padding: .15rem .5rem; margin: .1rem 0 .3rem; }
html:not(.rp-nav-on):not(.rp-nav) .rp-nav-hint { display: none; }
.rp-code-view pre .rp-ln.rp-ln-hl { background: #fff7cc; }
.rp-code-view pre .rp-ln[data-path] { cursor: pointer; }
.rp-add-item { display: flex; gap: .4rem; align-items: center; margin: .3rem 0 .6rem; }
.rp-add-item .shiny-input-container { flex: 1 1 auto; margin-bottom: 0; }
"

# the cards' behaviour: open / close remembered in this browser, the R
# and the column / value switches, the folded arguments' search and
# promotion, Code line <-> card, Enter adds an item
.builder_cards_js <- "
(function() {
  function store(k, v) { try { window.localStorage.setItem(k, v); } catch (e) {} }
  function read(k) { try { return window.localStorage.getItem(k); } catch (e) { return null; } }
  $(document).on('click', '.rp-card-toggle', function() {
    var card = $(this).closest('.rp-card');
    card.toggleClass('rp-card-open');
    store('rp-card-open:' + card.data('card'), card.hasClass('rp-card-open') ? '1' : '0');
  });
  $(document).on('shiny:value shiny:visualchange', function() {
    setTimeout(function() {
      $('.rp-card').each(function() {
        var v = read('rp-card-open:' + $(this).data('card'));
        if (v === '1') $(this).addClass('rp-card-open');
        if (v === '0') $(this).removeClass('rp-card-open');
      });
    }, 0);
  });
  $(document).on('change', '.rp-card-switch input', function() {
    $(this).closest('.rp-card').toggleClass('rp-card-off', !this.checked);
  });
  $(document).on('change', '.rp-isr', function() {
    $(this).closest('.rp-field').toggleClass('rp-field-r', this.checked);
  });
  $(document).on('change', '.rp-isval', function() {
    $(this).closest('.rp-map-row').toggleClass('rp-map-val', this.checked);
  });
  $(document).on('click', '.rp-more-toggle', function() {
    $(this).closest('.rp-more').toggleClass('rp-open');
  });
  $(document).on('input', '.rp-more-search', function() {
    var q = this.value.toLowerCase();
    $(this).closest('.rp-more-body').find('.rp-field').each(function() {
      var name = ($(this).data('arg') || '').toString().toLowerCase();
      $(this).toggleClass('rp-hidden', q.length > 0 && name.indexOf(q) < 0);
    });
  });
  // a value typed into a folded argument keeps its row shown when the fold closes
  $(document).on('change input', '.rp-more .rp-field input, .rp-more .rp-field select', function() {
    var f = $(this).closest('.rp-field');
    var has = $(this).is(':checkbox') ? this.checked : (this.value || '').length > 0;
    f.toggleClass('rp-has-value', has);
  });
  // a plan card's fields in two groups: those its code writes (above) and
  // those it does not (below, folded).  A value given below moves its
  // field up when the field is left; a field above emptied moves down --
  // each in the function's order (the upper group's data-formals), as the
  // next drawing puts them
  function fieldHas(f) {
    var has = false;
    f.find('input, select, textarea').each(function() {
      if ($(this).hasClass('rp-isr') || this.type === 'search') return;
      if (this.type === 'checkbox' || this.type === 'radio') {
        if (this.checked !== (this.getAttribute('data-init') === '1')) has = true;
      } else if (($(this).val() || '').toString().length > 0) has = true;
    });
    return has;
  }
  function placeField(f, box, wrap, order) {
    var rank = order.indexOf(String(f.data('arg')));
    var el = wrap ? $('<div class=\"rp-group-cell\"></div>').append(f) : f;
    var after = null;
    box.children().each(function() {
      var g = $(this).is('.rp-field') ? $(this) : $(this).find('.rp-field').first();
      var r = order.indexOf(String(g.data('arg')));
      if (r >= 0 && r < rank) after = $(this);
    });
    if (after) after.after(el); else if (wrap) box.prepend(el); else box.children('.rp-more-search').length ? box.children('.rp-more-search').after(el) : box.prepend(el);
  }
  $(document).on('focusout', '.rp-plan-args .rp-field', function() {
    var f = $(this);
    setTimeout(function() {
      if (f[0].contains(document.activeElement)) return;
      var card = f.closest('.rp-plan-args');
      var up = card.find('.rp-args-up .rp-group-row').first();
      var low = card.find('.rp-more-body').first();
      if (!up.length || !low.length) return;
      var order = (card.find('[data-formals]').first().attr('data-formals') || '').split(',');
      var below = f.closest('.rp-more-body').length > 0;
      var has = fieldHas(f);
      if (below && has) {
        placeField(f, up, true, order);
      } else if (!below && !has) {
        var cell = f.parent('.rp-group-cell');
        placeField(f, low, false, order);
        if (cell.length && !cell.children().length) cell.remove();
      }
    }, 0);
  });
  $(document).on('shiny:value', function() {
    setTimeout(function() {
      $('.rp-plan-args input[type=checkbox]:not([data-init])').each(function() {
        this.setAttribute('data-init', this.checked ? '1' : '0');
      });
    }, 0);
  });
  // Code line <-> card
  function jumpMap(pre) {
    var s = document.querySelector('.rp-code-jump[data-for=\"' + pre.id + '\"]');
    if (!s) return null;
    try { return JSON.parse(s.textContent); } catch (e) { return null; }
  }
  // (again whenever an output comes: the code and its map arrive apart)
  function applyJumps() {
    $('.rp-code-view pre').each(function() {
      var pre = this, m = jumpMap(pre);
      if (!m) return;
      $(pre).find('.rp-ln').each(function(i) {
        var p = m[String(i + 1)];
        if (p) this.setAttribute('data-path', p); else this.removeAttribute('data-path');
      });
    });
  }
  $(document).on('shiny:value', function() { setTimeout(applyJumps, 20); });
  // a line's card: selected, opened and scrolled into view in the cards'
  // column only -- the code beside it stays where it is (its column is
  // sticky, its own scroll); narrow, where the code is below the cards, the
  // card offers the way back to the line
  $(document).on('click', '.rp-code-view .rp-ln[data-path]', function() {
    var p = this.getAttribute('data-path'), line = this;
    $('.rp-card').removeClass('rp-card-selected');
    $('.rp-back-code').remove();
    var card = $('.rp-card[data-path=\"' + p + '\"]');
    card.addClass('rp-card-selected rp-card-open');
    if (card.length) {
      var right = $(line).closest('.rp-b-right')[0];
      var keep = right ? right.scrollTop : null;
      card[0].scrollIntoView({block: 'nearest'});
      if (right) right.scrollTop = keep;
      if (window.innerWidth < 992) {
        var txt = card.closest('.rp-builder-cards').data('back') || 'Back to the code';
        var b = $('<button type=\"button\" class=\"btn btn-link btn-sm rp-back-code\"></button>')
          .text('\\u2193 ' + txt);
        b.on('click', function(e) {
          e.stopPropagation();
          line.scrollIntoView({block: 'center', behavior: 'smooth'});
          b.remove();
        });
        card.find('> .rp-card-head .rp-card-title').after(b);
      }
    }
    if (window.Shiny) Shiny.setInputValue('rp_card_selected', p, {priority: 'event'});
  });
  // a card's field in focus lights its lines (and those of the items it
  // names in data-also: the decimals light plan_cells()'s formats)
  $(document).on('focusin', '.rp-card', function() {
    var ps = [this.getAttribute('data-path')]
      .concat((this.getAttribute('data-also') || '').split(' ').filter(Boolean));
    $('.rp-ln').removeClass('rp-ln-hl');
    ps.forEach(function(p) { $('.rp-ln[data-path=\"' + p + '\"]').addClass('rp-ln-hl'); });
  });
  // the Layers menu: a card chosen is opened, selected and brought into view
  $(document).on('click', '.rp-layer-go', function(e) {
    e.preventDefault();
    var card = $('#' + $(this).data('target'));
    if (!card.length) return;
    $('.rp-card').removeClass('rp-card-selected');
    card.addClass('rp-card-selected rp-card-open');
    card[0].scrollIntoView({block: 'start', behavior: 'smooth'});
  });
  // Enter in the add field adds; the new card's first field takes the focus
  var wantFocus = false;
  $(document).on('keydown', '.rp-add-item .selectize-input input', function(e) {
    if (e.key === 'Enter') {
      var box = $(this).closest('.rp-add-item');
      setTimeout(function() { box.find('.rp-add-btn').trigger('click'); }, 50);
    }
  });
  $(document).on('click', '.rp-add-btn', function() { wantFocus = true; });
  $(document).on('shiny:value', function() {
    if (!wantFocus) return;
    setTimeout(function() {
      // the card just added (marked new), else the last
      var card = $('.rp-card.rp-card-new').first();
      if (!card.length) card = $('.rp-card').last();
      if (!card.length) return;
      card[0].scrollIntoView({block: 'nearest'});
      var f = card.find('input:visible, select:visible').first();
      if (f.length) { f.trigger('focus'); wantFocus = false; }
    }, 50);
  });
})();
"
