# The type notation and the field of one argument: arg_field() and the
# .field_* helpers.  arg_field() is a thin wrapper of field_ui()
# (R/field_ui.R: the one field dispatcher), which draws with the helpers
# here.

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
                 grepl("^number|^integer|^date", inner)) "list" else "r")
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

# Does a type take an R expression as its value?  One of its alternatives
# is `r`, `formula`, `guard`, a list of R or a call (`call<rtf_border>`: a
# call is written as R); no type known: R is all it can be.
.type_takes_r <- function(type) {
  if (is.null(type) || !length(type) || is.na(type) || !nzchar(type)) return(TRUE)
  alts <- .type_split(trimws(type), "|")
  any(alts %in% c("r", "formula", "guard", "list<r>")) || any(grepl("^call(<|$)", alts))
}

# A logical among other types drawn as a pick (input: pick; plan_listing's
# wrap `logical|string`, rtf_border()'s sides `logical|enum[..]|call<..>`):
# a pick of the definition's values -- TRUE and FALSE among them -- not a
# check, which could say only TRUE or FALSE
.logical_pick <- function(def, type = def$type) {
  if (!identical(.def_input(def), "pick") || !identical(.arg_kind(type), "logical")) return(FALSE)
  alts <- setdiff(.type_split(trimws(type), "|"), "r")
  length(alts) > 1L
}

# a pick's value read back as its type says: TRUE / FALSE a logical where
# the type has one, else the value as the type's other alternatives say
.logical_pick_value <- function(v, type) {
  v <- as.character(v)
  if (v %in% c("TRUE", "FALSE")) return(as.logical(v))
  alts <- setdiff(.type_split(trimws(type), "|"), c("logical", "r"))
  .coerce_value(v, paste(alts, collapse = "|"))
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
# aes fields each with a column / value switch.  Beside a field: the (i)
# of its help (what the argument means) and, only where the definition's
# type takes an R expression (.type_takes_r(); `r_ok` says otherwise), the
# `R` switch (the value as R) -- a value that is R already (`!r` written
# in the YAML by hand) keeps its switch, so it is shown and not lost.  A def (tfl_arg_defs()'s row) and `ctx` (what the card knows,
# .choice_values()) choose the field by its input form; `fixed_rows`: a
# table's rows shown read only on top (the template's ALL row), not read
# back.  The card surface of field_ui() (R/field_ui.R): its arguments
# are field_ctx("card", ...)'s (`ctx` its `sources`; `fn` unused).
#' @noRd
arg_field <- function(id, arg, type = NULL, label = NULL, value = NULL, default = NULL,
                      choices = NULL, from = "own", help = NULL, required = FALSE,
                      lang = "en", fn = NULL, aes = NULL, nav = NULL, help_link = NULL,
                      own = NULL, def = NULL, ctx = NULL, fixed_rows = NULL, nested = FALSE,
                      r_ok = NULL) {
  fc <- field_ctx("card", lang = lang, type = type, label = label, help = help,
                  help_link = help_link, nav = nav, default = default, required = required,
                  nested = nested, r_ok = r_ok, choices = choices, sources = ctx, from = from,
                  own = own, fixed_rows = fixed_rows, aes = aes)
  field_ui(id, arg, def, value, fc)
}

# ---- a def's input form -> the field ----------------------------------------------

# A def's input form (tfl_arg_defs()'s `input`), NULL for none
.def_input <- function(def) {
  f <- def$input
  if (is.null(f) || length(f) != 1L || is.na(f) || !nzchar(f)) NULL else as.character(f)
}

# What "(none)" does for a def (tfl_arg_defs()'s `unset`: "omit",
# "step_off" or "stop"), NULL for none said
.def_unset <- function(def) {
  u <- def[["unset"]]
  if (is.null(u) || length(u) != 1L || is.na(u) || !nzchar(u)) NULL else as.character(u)
}

# The kind of field a def's form draws, from the type's `kind`: a pick of an
# enum or of fixed values a select of them ("pick_enum"; one that takes a
# typed value, or whose values are only some of its choices, a selectize
# "pick_one"), a pick of a list<...> a select of several ("pick_many"), a
# pick of one a select ("pick_one"); a switch, a number field, a colour,
# a shape, a call, a map, R: as the type says (the type decides).  A
# condition of type r its R field with the columns as candidates.  A table:
# the field its type has here; arg_field() draws the table field
# (R/arg_defs_table.R) where the type is a map<K,V> or a rows<map<..>>.  A
# text: as the type says.  An r (an R expression, canon 17dfbb8): the R
# field.  The kind stays one that
# builder_card_read() reads back as the type says.
.def_kind <- function(form, kind, type, def) {
  switch(
    form,
    pick = {
      if (identical(kind, "list")) return("pick_many")
      if (.logical_pick(def, type)) kind <- "enum"
      if (!kind %in% c("string", "column", "stat", "object", "enum")) return(kind)
      words <- vapply(.choice_words(def$choices), function(w) .choice_word_parse(w)$name, "")
      only_fixed <- !length(words) || all(words == "fixed")
      if (length(.def_entries(def, type)) && only_fixed && !isTRUE(def$free)) return("pick_enum")
      "pick_one"
    },
    condition = if (identical(kind, "r")) "condition" else kind,
    # (an R expression: its R field, whatever the type)
    r = "r",
    kind)
}

# the entries of a def's pick: its values, else its type's enum
.def_entries <- function(def, type) {
  v <- as.character(unlist(def$values))
  if (length(v)) v else .type_enum(type)
}

# A condition: its R in a monospace field, the columns (`choices`) offered
# as it is typed
.field_condition <- function(fid, value, default, choices = NULL) {
  dl <- if (length(choices)) paste0(fid, "__dl")
  v <- if (inherits(value, "tfl_r")) as.character(unclass(value)) else if (!is.null(value)) .arg_to_r(value)
  shiny::tagList(
    shiny::tags$input(type = "text", id = fid, class = "form-control form-control-sm rp-mono rp-condition",
                      value = v, placeholder = if (!is.null(default)) .arg_to_r(default) else NULL,
                      spellcheck = "false", list = dl),
    if (!is.null(dl)) shiny::tags$datalist(id = dl, lapply(unique(as.character(unlist(choices))),
                                                         function(x) shiny::tags$option(value = x))))
}

.has_value <- function(v) {
  if (is.null(v)) return(FALSE)
  if (is.list(v)) return(length(v) > 0L)
  any(nzchar(as.character(v)) & !is.na(v))
}

# a select of the data's columns (or any choices), one or several
# (`create`: a value not among the choices may be typed)
.field_select <- function(fid, choices, value, multiple = FALSE, lang = "en", placeholder = NULL,
                          create = FALSE) {
  choices <- .choices_with(choices, value)
  # (a select of several shows its items in its options' order: the values
  # first, in their own order -- a hierarchy's levels, the keys of a page)
  if (multiple && is.atomic(choices) && is.atomic(value) && length(value)) {
    at <- match(as.character(value), as.character(choices), nomatch = 0L)
    at <- unique(at[at > 0L])
    if (length(at)) choices <- c(choices[at], choices[-at])
  }
  ph <- if (!is.null(placeholder)) as.character(placeholder)[[1L]] else tr("Choose", lang)
  opts <- list(placeholder = ph)
  if (create) opts$create <- TRUE
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
    v <- v[nzchar(v) & !v %in% unlist(choices)]
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

.field_r <- function(fid, value, default, placeholder = NULL) {
  v <- if (inherits(value, "tfl_r")) as.character(unclass(value)) else if (!is.null(value)) .arg_to_r(value)
  if (is.null(placeholder) && !is.null(default)) placeholder <- .arg_to_r(default)
  shiny::tags$input(type = "text", id = fid, class = "form-control form-control-sm rp-mono",
                    value = v, placeholder = placeholder, spellcheck = "false")
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

# does a call<fn>'s field have a small form (its function's visible
# arguments, the schema's)?  Without one the call is typed as its R text
.call_has_form <- function(fns) {
  fn <- if (length(fns)) fns[[1L]]
  !is.null(fn) && length(.schema_of(fn)$visible) > 0L
}

# a call<fn>: the function's visible arguments as a small form (the schema
# knows them), else its R text -- the call's own shape as the placeholder,
# rtf_border(...), where there is no default
.field_call <- function(fid, fns, value, default, lang = "en") {
  fn <- if (length(fns)) fns[[1L]]
  if (!.call_has_form(fns)) {
    return(.field_r(fid, value, default, placeholder = if (is.null(default) && !is.null(fn)) paste0(fn, "(...)")))
  }
  ent <- .schema_of(fn)
  vis <- ent$visible
  args <- if (inherits(value, "tfl_call")) value$args else list()
  shiny::div(class = "rp-call", `data-fn` = fn,
             shiny::tags$code(class = "rp-name", paste0(fn, "(")),
             lapply(vis, function(a) {
               arg_field(fid, a, ent$args[[a]]$type, label = ent$args[[a]]$label,
                         value = args[[a]], default = ent$args[[a]]$default,
                         help = .schema_help(fn, a), lang = lang, nested = TRUE, def = ent$args[[a]])
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
