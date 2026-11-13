# ONE field for both kinds of report.  A code SPEC argument is drawn by
# arg_field() from the type notation of tflspec's schema ("column",
# "list<column>", "enum[a|b]", "r" ...).  A template parameter (tfl_param's
# type / of / choices / multiple) is first turned into the same notation
# here, by one table, and then drawn by the same arg_field() -- so the
# widget, the "meaning (name)" label, the R switch, the source badge, the
# help mark, the locked state and the "not set" state are one part.
#
# Notation added to the schema's: dataset (a dataset of the study),
# population (an analysis set), level (a value of a variable; typed values
# are kept), list<level>, list<enum[a|b]> (several of fixed values),
# list<string> (several texts, typed), rows<name|text> (lines of a small
# table, drawn as R text).

# The table: the template's input (what .tpl_type() says) -> the notation,
# and the widget the field gets.  `levels` is `level` or, with
# multiple = TRUE, `list<level>`; `text` is `r` for a filter's expression.
.field_input_types <- data.frame(
  input = c("population", "data", "object", "var", "vars", "levels", "levels_many",
            "flag", "number", "text", "expr", "chips", "choice", "choices", "list", "columns"),
  type = c("population", "dataset", "object", "column", "list<column>", "level", "list<level>",
           "logical", "number", "string", "r", "list<string>", "enum[...]", "list<enum[...]>",
           "rows<name|text>", "rows<column|string|number>"),
  widget = c("select (analysis sets)", "select (datasets)", "select (objects)",
             "select (columns)", "multi select, drag to order (columns)",
             "select (values, typed allowed)", "multi select (values, typed allowed)",
             "switch", "number input", "text input", "monospace R input",
             "multi select (typed texts)", "select (fixed values)",
             "multi select (fixed values)", "monospace R input (lines)",
             "monospace R input (lines)"),
  stringsAsFactors = FALSE)

# A text parameter that holds an R expression (a filter): by its name
.tpl_expr_names <- c("subset", "where", "filter", "condition")

# A template parameter -> list(input, type, choices): the input of the
# table, its type notation (an enum with the parameter's choices), and the
# choices the notation itself carries (NULL: the caller gives the options,
# the columns or the values).
.tpl_field_type <- function(prm, columns = character()) {
  input <- .tpl_type(prm, columns)
  if (identical(input, "levels") && isTRUE(prm$multiple)) input <- "levels_many"
  if (identical(input, "text") && prm$name %in% .tpl_expr_names) input <- "expr"
  ty <- .field_input_types$type[match(input, .field_input_types$input)]
  ch <- as.character(prm$choices %||% character())
  if (input %in% c("choice", "choices")) {
    if (length(ch)) {
      ty <- sub("...", paste(ch, collapse = "|"), ty, fixed = TRUE)
    } else {
      ty <- if (input == "choice") "string" else "list<string>"
    }
  }
  list(input = input, type = ty, choices = if (length(ch)) ch else NULL)
}

# One template parameter as the shared field: the parameter's `name` is the
# argument, `id` the card's (the input is `<id>__<name>`).  `options`: the
# columns, datasets, values or analysis sets to choose from (a named vector
# shows its names); `value`: the report's own value (NULL: not set, the
# default is shown); `from`: where the default came from (the source
# badge); `locked`: the template fixes it.
#' @noRd
tpl_field <- function(id, prm, value = NULL, options = NULL, from = "own", locked = FALSE,
                      label = NULL, help = NULL, lang = "en", own = NULL, columns = character()) {
  ft <- .tpl_field_type(prm, columns)
  arg_field(id, gsub("[^A-Za-z0-9_]", "_", prm$name), ft$type, label = label %||% prm$name,
            value = value, default = prm$default,
            choices = if (ft$input %in% c("choice", "choices")) NULL else options,
            from = from, help = help, required = isTRUE(prm$required), lang = lang,
            own = own, locked = locked)
}
