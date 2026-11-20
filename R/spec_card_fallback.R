# What the card generator (R/spec_card.R) needs and tflspec's
# definitions -- tfl_schema(fn), tfl_arg_defs(fn = ) -- do not say yet.
# Each entry goes when tflspec says the same; where tflspec says a value
# (the labels, the defaults, the input forms), tflspec's is used.
# Nothing else in tflplanner says how a card's field is drawn.

# plan_cell_style()'s looks: a choice of TRUE / FALSE with a blank (left
# alone: not written), not a check (an unticked check would say FALSE)
.spec_cell_look <- "enum[TRUE|FALSE]|logical"

.spec_fallback <- list(
  # The type a field is drawn by where tflspec's input form has no field
  # here yet: "r" a value typed as R -- a list of text or of whole numbers typed in
  # one box (input: text); "widths" a number a column (the part
  # .spec_parts$widths); plan_cell_style()'s looks the schema's type drawn
  # as an enum (tfl_arg_defs() gives a union's enum no values).  The
  # definition's input form is not used for these.
  type = list(
    plan_cell_style = c(bold = .spec_cell_look, italic = .spec_cell_look),
    plan_columns = c(widths = "widths", column_widths_twips = "r"),
    plan_titles = c(pages = "r"),
    plan_footnotes = c(pages = "r")),
  # The arguments tflspec gives no type whose value is R only (a function
  # or a list of them, a header object): typed "r", an R field as before
  # (tflspec's own definition reads them as text)
  r = list(
    plan_columns = "cell_format",
    plan_paginate_cols = "col_header"))

# the definitions with the fallback's types (the field the type says, not
# the definition's input form)
.spec_fallback_types <- function(sch, fn) {
  ty <- .spec_fallback$type[[fn]]
  for (a in intersect(names(ty), names(sch$args))) sch <- .spec_drawn_as(sch, a, ty[[a]])
  for (a in intersect(.spec_fallback$r[[fn]], names(sch$args))) {
    if (is.null(sch$args[[a]]$type)) sch$args[[a]]$type <- "r"
  }
  sch
}

# an argument drawn by another type than its definition's: the type the
# field is drawn by, the definition's own kept as `def_type` (whether the
# field takes R is its say), no input form
.spec_drawn_as <- function(sch, a, type) {
  sch$args[[a]]["def_type"] <- list(sch$args[[a]]$type)
  sch$args[[a]]$type <- type
  sch$args[[a]]$input <- NULL
  sch
}

# The arguments whose input form is `r` (tflspec's: an R expression, canon
# 17dfbb8) drawn as R: an R-expression field, its R mark fixed.  A type
# that is R only is drawn so already; another (plan_cell_style(border = ),
# a call<rtf_border>) keeps its type as `def_type`, its value shown as R
# source and read back as the value it says (.spec_drawn_r_read()).
.spec_input_r <- function(sch) {
  for (a in names(sch$args)) {
    d <- sch$args[[a]]
    if (identical(.def_input(d), "r") && !.type_only_r(d$type)) sch <- .spec_drawn_as(sch, a, "r")
  }
  sch
}
