# One field dispatcher: an argument's field drawn from its definition by one
# entry, field_ui(), for every surface (canon PRINCIPLES 458: one
# definition, the data type decides; 464: a few basic types made from the
# arg type).  Step 1 (s1-tools one-dispatcher-plan-2026-11-25.md, PR1): the
# card surface moved here from arg_field() with no change in what is drawn
# -- arg_field() is a thin wrapper of field_ui() -- and the template
# surface still drawn by tpl_widget() (step 2 moves it here).
#
#   field_ctx(surface, ...)               what the surface knows about a
#                                         field: plain values only
#   field_look(def, arg, value, fc)       the renderer's word (the card's
#                                         kind: column, list, pick_one, ...)
#   field_ui(id, arg, def, value, fc)     the field: its control in its frame
#   .field_control(kind, ...)             the control of a kind
#   .field_frame(fid, arg, kind, ...)     the card's frame around it: label,
#                                         badge, help, "(none)" and R switches
#
# field_type() (R/field_type.R) names the basic type a definition has; it
# does not choose the look in step 1.

# What a surface knows about a field, plain values (no reactives), so that
# a field is a function of its arguments alone.  The card surface
# (arg_field()'s arguments):
#   type        the type the field is drawn by (arg_field()'s `type`; it may
#               differ from the definition's -- .spec_drawn_as())
#   label, help, help_link, nav, default, required, from, own
#               the frame's and the control's texts and marks
#   nested      a field inside a call's small form (no "(none)")
#   r_ok        whether the R switch is offered (NULL: the type says)
#   choices     the values offered (the card's columns), NULL: the def's
#   sources     what the card knows for the def's choice words
#               (.choice_values()'s ctx)
#   fixed_rows  a table's rows shown read only on top
#   aes         a mapping's aesthetics
# The template surface (step 2; tpl_widget()'s arguments, not read yet):
#   prm, data, columns, populations, suggest, candidates, none, say_first,
#   params, args, rows
#' @noRd
field_ctx <- function(surface = c("card", "template"), lang = "en", type = NULL, label = NULL,
                      help = NULL, help_link = NULL, nav = NULL, default = NULL, required = FALSE,
                      nested = FALSE, r_ok = NULL, choices = NULL, sources = NULL, from = "own",
                      own = NULL, fixed_rows = NULL, aes = NULL, prm = NULL, data = NULL,
                      columns = NULL, populations = NULL, suggest = NULL, candidates = NULL,
                      none = NULL, say_first = NULL, params = NULL, args = NULL, rows = NULL) {
  list(surface = match.arg(surface), lang = lang, type = type, label = label, help = help,
       help_link = help_link, nav = nav, default = default, required = required, nested = nested,
       r_ok = r_ok, choices = choices, sources = sources, from = from, own = own,
       fixed_rows = fixed_rows, aes = aes, prm = prm, data = data, columns = columns,
       populations = populations, suggest = suggest, candidates = candidates, none = none,
       say_first = say_first, params = params, args = args, rows = rows)
}

# The word a field is drawn by on its surface.  The card's: the type's kind
# (.arg_kind()) refined by the def's input form (.def_kind()), a table where
# the type has one (.tbl_spec()), a pick of text with nothing to pick from
# its text field -- what tests/testthat's field_kind() reads too.
#' @noRd
field_look <- function(def, arg = NULL, value = NULL, fc = field_ctx("card", type = def$type)) {
  .field_prep(def, arg, value, fc)$look
}

# The template surface is tpl_widget()'s until step 2
.field_no_template <- function(what) {
  stop(sprintf("%s: the template surface is drawn by tpl_widget() until step 2", what), call. = FALSE)
}

# The card's look and what it decides on the way: the def's form, the
# choices (the def's source words resolved), the value (a value a table
# cannot hold shown as its R) and the table's spec.  A list(look, form,
# choices, value, tbl).
.field_prep <- function(def, arg, value, fc) {
  if (!identical(fc$surface, "card")) .field_no_template("field_look()")
  type <- fc$type
  choices <- fc$choices
  ctx <- fc$sources
  lang <- fc$lang
  kind <- .arg_kind(type)
  # (a def's input form: the field it says; .def_kind() the kind it draws)
  form <- .def_input(def)
  tbl <- NULL
  # (a Rows field the SPEC keeps as text -- the def's serializer, plan_sort's
  # keys: its rows, and whether its switch writes the type's false)
  rt <- .rows_text_read(value, def)
  value <- rt$value
  if (!is.null(form)) {
    if (is.null(choices) && length(def$choices)) {
      choices <- .choice_values(def$choices, ctx, lang, values = def$values)
    }
    # (a logical among other types: its values -- TRUE, FALSE and the
    # others the definition gives -- offered, a value of one's own typed
    # where the definition is free)
    if (is.null(choices) && .logical_pick(def, type)) choices <- .def_entries(def, type)
    kind0 <- kind
    kind <- .def_kind(form, kind, type, def)
    # (a table: the table field of a map<K,V> or a rows<map<..>>, else the
    # field its type has)
    tbl <- if (identical(form, "table")) {
      .tbl_spec(type, def, ctx, value = if (!inherits(value, "tfl_r")) value, lang = lang,
                choices = choices)
    }
    if (!is.null(tbl)) kind <- "table"
    # (a value the table cannot show as it is -- a cell deeper than its
    # lines, normalize_ard(stack = )'s cross layer -- is shown as its R,
    # so that nothing of it is lost)
    if (!is.null(tbl) && !.tbl_holds(tbl, value)) value <- tflspec::tfl_r(.arg_to_r(value))
    # (a pick of text with nothing to choose from: the text field)
    if (identical(kind, "pick_one") && identical(kind0, "string") && !length(choices) &&
          !isTRUE(def$free)) {
      kind <- kind0
    }
  }
  list(look = kind, form = form, choices = choices, value = value, tbl = tbl, false = rt$false)
}

# One argument's field on its surface: its control (.field_control()) in
# the surface's frame (.field_frame()).  `def`: tfl_arg_defs()'s row (NULL:
# none, the type alone decides); `fc`: field_ctx().  The card's field is
# arg_field()'s (R/builder_cards_fields.R), drawn as it always was.
#' @noRd
field_ui <- function(id, arg, def = NULL, value = NULL, fc = field_ctx()) {
  if (identical(fc$surface, "template")) .field_no_template("field_ui()")
  fid <- paste0(id, "__", arg)
  p <- .field_prep(def, arg, value, fc)
  kind <- p$look
  value <- p$value
  type <- fc$type
  r_ok <- fc$r_ok
  # (a list whose entries are not each one plain value -- plan_titles(pages
  # = ), a page a list of lines -- cannot be a select of entries: shown as
  # its R, so that nothing of it is lost)
  if (kind %in% c("list", "pick_many") && is.list(value) && !inherits(value, c("tfl_r", "tfl_unset")) &&
        !all(vapply(value, function(x) is.atomic(x) && length(x) == 1L, NA))) {
    value <- tflspec::tfl_r(.arg_to_r(value))
  }
  # (a value that is not one plain value in a field of one -- a call,
  # plan_style(border = rtf_border(...)), in its text field -- is shown as
  # its R, so that nothing of it is lost)
  one_kinds <- c("string", "column", "stat", "template", "number", "integer", "enum", "pick_one",
                 "pick_enum", "object", "colour", "shape")
  if (kind %in% one_kinds && !is.null(value) && !inherits(value, c("tfl_r", "tfl_unset")) &&
        !(is.atomic(value) && length(value) == 1L)) {
    value <- tflspec::tfl_r(.arg_to_r(value))
  }
  # (an argument that is R only -- no other form -- shows its R in its own
  # field, as the others' R: no switch to press, its R mark fixed)
  r_only <- kind %in% c("r", "condition")
  is_r <- inherits(value, "tfl_r") && !r_only
  # (the R switch: the definition's type -- not the one the field is drawn
  # by -- takes R, or the value is R)
  r_ok <- !r_only && (is_r || (r_ok %||% .type_takes_r(def$def_type %||% type)))
  r_text <- if (is_r) as.character(unclass(value)) else ""
  if (is_r) value <- NULL
  # (`!unset`: the field says "(none)", and the control is hidden behind it)
  none <- .is_unset(value)
  if (none) value <- NULL
  # (a string in an argument that takes only R is that R's text; one only
  # drawn as R -- the definition's type is another, .spec_drawn_as()
  # -- is a value, shown as R source: "AEBODSYS", quotes kept)
  only_r <- .type_only_r(if ("def_type" %in% names(def)) def$def_type else type)
  if (only_r && !is.null(value)) value <- .only_r_value(value)
  control <- .field_control(kind, fid, arg, value, def, fc, p)
  .field_frame(fid, arg, kind, def, fc, control,
               st = list(value = value, r_only = r_only, r_ok = r_ok, is_r = is_r, r_text = r_text,
                         none = none))
}

# The control of a look (`kind`): the card's switch of today's helpers.
# `p`: .field_prep()'s list (the form, the choices, the table's spec).
.field_control <- function(kind, fid, arg, value, def, fc, p) {
  if (!identical(fc$surface, "card")) .field_no_template(".field_control()")
  type <- fc$type
  lang <- fc$lang
  default <- fc$default
  aes <- fc$aes
  fixed_rows <- fc$fixed_rows
  choices <- p$choices
  form <- p$form
  tbl <- p$tbl
  switch(
    kind,
    column = .field_select(fid, choices, value, multiple = FALSE, lang = lang, placeholder = default),
    # (a list typed -- a def's text form, normalize_ard(drop_contexts = ):
    # the choices offered, another value typed)
    list = .field_select(fid, choices, value, multiple = TRUE, lang = lang, placeholder = default,
                         create = identical(form, "text")),
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
    # (a def's forms)
    pick_enum = .field_enum(fid, .def_entries(def, type), value, default, lang,
                            labels = .enum_labels[[arg]]),
    pick_one = .field_select(fid, choices, value, multiple = FALSE, lang = lang,
                             placeholder = default, create = isTRUE(def$free)),
    pick_many = .field_select(fid, choices, value, multiple = TRUE, lang = lang,
                              placeholder = default, create = isTRUE(def$free)),
    condition = .field_condition(fid, value, default, choices),
    table = shiny::tagList(.field_table(fid, tbl, value, fixed_rows, lang),
                           .rows_false_switch(fid, def, p$false, lang)),
    .field_r(fid, value, default))
}

# The card's frame around a control: the label with its required mark, its
# badge and help; the "(none)" and R switches on the label's line; the R
# box beside the control; the "in order" hint and the nav hint below.
# `st`: the field's state -- the value as the control shows it, whether
# the field is R only (r_only), takes R (r_ok), is R now (is_r, r_text),
# says "(none)" (none).
.field_frame <- function(fid, arg, kind, def, fc, control, st) {
  lang <- fc$lang
  t <- function(x) tr(x, lang)
  label <- fc$label
  help <- fc$help
  help_link <- fc$help_link
  nav <- fc$nav
  required <- fc$required
  nested <- fc$nested
  from <- fc$from
  own <- fc$own
  value <- st$value
  r_only <- st$r_only
  r_ok <- st$r_ok
  is_r <- st$is_r
  r_text <- st$r_text
  none <- st$none
  r_input <- shiny::tags$input(type = "text", id = paste0(fid, "__r"), class = "form-control form-control-sm rp-mono rp-r-input",
                               value = r_text, placeholder = "R", spellcheck = "false")
  r_switch <- if (r_only) shiny::tags$span(class = "rp-r-toggle rp-r-fixed", title = t("As R"), "R") else
    if (r_ok) shiny::tags$label(
    class = "rp-r-toggle", title = t("As R"),
    shiny::tags$input(type = "checkbox", id = paste0(fid, "__isr"), class = "rp-isr",
                      checked = if (is_r) NA else NULL),
    shiny::tags$span("R"))
  # (the "(none)" toggle, as the R switch: not on a required argument, nor
  # on a field inside a call's small form -- `nested` -- nor where the
  # definition's `unset` is "stop" (tflspec: the argument needs a value;
  # "omit" and "step_off" offer it))
  none_ok <- !required && !nested && !identical(.def_unset(def), "stop")
  none <- none && none_ok
  none_switch <- if (none_ok) shiny::tags$label(
    class = "rp-none-toggle", title = t("None here (!unset)"),
    shiny::tags$input(type = "checkbox", id = paste0(fid, "__none"), class = "rp-isnone",
                      `data-init` = if (none) "1", checked = if (none) NA else NULL),
    shiny::tags$span(t(.none_word)))
  # (a def's form keeps its type's class: a pick of several is a list's)
  # (an R field's kind class is rp-field-rexpr: rp-field-r is the state "R
  # switch on", which hides the field's own input -- an R-only field's
  # input is its R box, so it would be hidden with nothing in its place)
  cls_kind <- switch(kind, pick_many = "list", pick_one = "column", pick_enum = "enum",
                     r = "rexpr", condition = "rexpr rp-field-condition", kind)
  cls <- c("rp-field", paste0("rp-field-", cls_kind), if (required) "rp-field-required",
           if (is_r) "rp-field-r", if (none) "rp-field-none",
           if (none || nzchar(r_text) || .has_value(value)) "rp-has-value")
  shiny::div(
    class = paste(cls, collapse = " "), `data-arg` = arg, `data-field` = fid,
    # (the "(none)" and R switches on the label's line, right: the input
    # keeps the field's whole width)
    shiny::div(class = "rp-field-head",
               shiny::tags$label(class = "rp-field-label", `for` = fid,
                                 builder_label(label, arg, "arg", lang),
                                 if (required) shiny::tags$span(class = "rp-req", title = t("Required"), "*"),
                                 from_badge(from, lang, own = own),
                                 help_tip(help), help_link),
               if (!is.null(none_switch) || !is.null(r_switch)) {
                 shiny::span(class = "rp-field-switches", none_switch, r_switch)
               }),
    shiny::div(class = "rp-field-row",
               shiny::div(class = "rp-form-input", control),
               if (r_ok) r_input, if (none_ok) shiny::div(class = "rp-none-show", t(.none_word))),
    if (identical(kind, "pick_many") && isTRUE(def$ordered)) {
      shiny::div(class = "rp-field-hint rp-ordered-hint", t("In order, top to bottom"))
    },
    if (!is.null(nav)) nav_hint(nav, "field", lang))
}
