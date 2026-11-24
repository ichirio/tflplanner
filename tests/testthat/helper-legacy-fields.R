# The card's field as it was drawn before the one field dispatcher
# (R/field_ui.R): a verbatim copy of arg_field() at tflplanner origin/next
# c3468894 (R/builder_cards_fields.R lines 112-261), renamed
# .legacy_arg_field().  test-field-dispatch-card.R draws every field with
# both and expects the same HTML.  It calls the .field_* helpers as they
# are (unchanged by the dispatcher).  Deleted in step 3 of the plan (s1-tools
# one-dispatcher-plan-2026-11-25.md), when the look is meant to change.
# Do not edit the copy: it is the reference.
.legacy_arg_field <- function(id, arg, type = NULL, label = NULL, value = NULL, default = NULL,
                      choices = NULL, from = "own", help = NULL, required = FALSE,
                      lang = "en", fn = NULL, aes = NULL, nav = NULL, help_link = NULL,
                      own = NULL, def = NULL, ctx = NULL, fixed_rows = NULL, nested = FALSE,
                      r_ok = NULL) {
  t <- function(x) tr(x, lang)
  kind <- .arg_kind(type)
  fid <- paste0(id, "__", arg)
  # (a def's input form: the field it says; .def_kind() the kind it draws)
  form <- .def_input(def)
  tbl <- NULL
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
  control <- switch(
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
    table = .field_table(fid, tbl, value, fixed_rows, lang),
    .field_r(fid, value, default))
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
