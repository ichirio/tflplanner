# The type notation and the field of one argument: arg_field() and the
# .field_* helpers.
# Split from R/builder_cards.R by section; no code changed.

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
  # (an argument that is R only -- no other form -- shows its R in its own
  # field, as the others' R: no switch to press, its R mark fixed)
  r_only <- identical(kind, "r")
  is_r <- inherits(value, "tfl_r") && !r_only
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
  r_switch <- if (r_only) shiny::tags$span(class = "rp-r-toggle rp-r-fixed", title = t("As R"), "R") else
    shiny::tags$label(
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
               if (!r_only) r_input, r_switch),
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
  # (several: in order, dragged to reorder -- a hierarchy's levels, the
  # columns of a stub, the keys of an order)
  if (multiple) opts$plugins <- list("remove_button", "drag_drop")
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
