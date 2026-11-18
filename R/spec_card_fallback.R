# UNTIL P0.  What the card generator (R/spec_card.R) needs and tflspec's
# definitions -- tfl_schema(fn), tfl_arg_defs(fn = ) -- do not say yet.
# Each table goes when tflspec has the column named beside it; where
# tflspec says a value, tflspec's is used (the labels, the defaults).
# Nothing else in tflplanner says how a card's field is drawn.

# The verbs' meanings, the card's label beside its name: the schema's
# function-level `label: {en, ja}` when tflspec has it (.verb_meaning());
# in English, translated by tr().  [P0: `label` on every plan verb]
.plan_meaning <- c(
  normalize_ard = "Normalize the ARD",
  table_plan = "Table structure",
  plan_cells = "Statistics in the cells",
  plan_digits = "Decimals and rounding",
  plan_levels = "Level order",
  plan_labels = "Display names",
  plan_nest = "Rows under another value",
  plan_total = "Total column",
  plan_compare = "Comparison columns",
  plan_stat_groups = "Groups of statistics",
  plan_sort = "Row order",
  plan_filter = "Row filter",
  plan_hide = "Hidden columns",
  plan_col_header = "Column header",
  plan_stub = "Row label column",
  plan_blanks = "Blank rows",
  plan_paginate_rows = "Page breaks by rows",
  plan_paginate_group = "A page per group",
  plan_paginate_cols = "Column pages",
  plan_row_group = "Row groups",
  plan_columns = "Column widths and alignment",
  plan_style = "Table style",
  plan_cell_style = "Cell styles",
  plan_append = "A part under the table")

# plan_cell_style()'s looks: a choice of TRUE / FALSE with a blank (left
# alone: not written), not a check (an unticked check would say FALSE)
.spec_cell_look <- "enum[TRUE|FALSE]|logical"

.spec_fallback <- list(
  # The type a field is drawn by where it is not the schema's: "r" an
  # argument given as R only (no field of its own yet); "widths" a number
  # a column (the part .spec_parts$widths); a list of text has no field
  # of its own yet (R); plan_cell_style()'s looks are the schema's type
  # drawn as an enum (tfl_arg_defs() gives a union's enum no values).  The
  # definition's input form is not used for these.
  # [P0: the type, or the field part, in the schema -- see the PR's list]
  type = list(
    normalize_ard = c(drop_contexts = "r", stack = "r", combine = "r"),
    plan_filter = c(by = "r"),
    plan_cell_style = c(bold = .spec_cell_look, italic = .spec_cell_look,
                        underline = .spec_cell_look, border = "r"),
    plan_columns = c(widths = "widths", row_title = "list<column>", cell_format = "r",
                     column_widths_twips = "r"),
    plan_paginate_cols = c(col_header = "r"),
    plan_row_group = c(group_col = "r"),
    plan_titles = c(pages = "r"),
    plan_footnotes = c(pages = "r")),
  # The defaults of the plan verbs whose formals are NULL (rtfreporter#949:
  # a verb records only what its call says, the defaults are applied when
  # the layers are merged).  A card shows these, as it showed the formals
  # before; a value equal to its default is not written.
  # [P0: `default:` on these arguments in the schema]
  default = list(
    plan_stub = list(before = FALSE),
    plan_paginate_group = list(keep = TRUE),
    plan_sort = list(keep = TRUE),
    plan_empty = list(when = "auto", position = "last")))

# the definitions with the fallback's types (the field the type says, not
# the definition's input form)
.spec_fallback_types <- function(sch, fn) {
  ty <- .spec_fallback$type[[fn]]
  for (a in names(ty)) {
    # (the definition's own type kept: whether the field takes R is its say)
    sch$args[[a]]["def_type"] <- list(sch$args[[a]]$type)
    sch$args[[a]]$type <- ty[[a]]
    sch$args[[a]]$input <- NULL
  }
  sch
}
