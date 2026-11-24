# The basic type of an argument's field: one of eight words, made from the
# definition's data type and input form only (canon PRINCIPLES 458: one
# definition, the data type decides; 464: a few basic types, made from the
# arg type).  Step 1 of the one field dispatcher (s1-tools
# one-dispatcher-plan-2026-11-25.md section 1): the word is data only --
# no field is drawn by it yet; arg_field() and tpl_widget() draw as before.
#
#   text       a line of text (string, template, date)
#   number     a number (number, integer; number|enum[fit] too)
#   yesno      a switch (a plain logical)
#   pick_one   one of a list (enum, column, level, stat, colour, shape,
#              object; a string picked; a logical among other words)
#   pick_many  several of a list (list<column|level|stat|string|...>)
#   rows       a small table (map, vec, rows, list<map>; an input table)
#   r          R code (r, formula, guard, a condition; for now also the
#              types no other word holds: a bare call, step<..>,
#              list<vec<..>>, a rows<..> with no table)
#   group      a function's own arguments as fields (call<fn>)

# the eight words
.field_types <- c("text", "number", "yesno", "pick_one", "pick_many", "rows", "r", "group")

# One definition's basic type: the word (.field_type_word(), from `type`
# and `input` alone) with three attributes, data only in step 1 --
#   many      a member of the type is a list<..> (several values)
#   ordered   the definition says the order matters (`ordered: true`)
#   free      the definition takes a value not among its choices (`free`)
#' @noRd
field_type <- function(def) {
  type <- def$type
  word <- .field_type_word(type, .def_input(def))
  ok <- !is.null(type) && length(type) == 1L && !is.na(type) && nzchar(trimws(type))
  alts <- if (ok) .type_split(trimws(type), "|") else character()
  structure(word, many = any(grepl("^list<", alts)), ordered = isTRUE(def$ordered),
            free = isTRUE(def$free))
}

# The word of a type and an input form.  The first member that is not `r`
# decides (a type of `r` alone is R); the first rule that matches wins.
.field_type_word <- function(type, input = NULL) {
  if (is.null(type) || length(type) != 1L || is.na(type) || !nzchar(trimws(type))) return("r")
  alts <- .type_split(trimws(type), "|")
  m <- setdiff(alts, "r")
  first <- if (length(m)) m[[1L]] else "r"
  is_input <- function(f) !is.null(input) && identical(input, f)
  # 1 a table the planner can draw
  if (is_input("table") && !is.null(.tbl_type(type))) return("rows")
  # 2 R: the input says so, or the type is R code
  if (is_input("condition") || is_input("r")) return("r")
  if (first %in% c("r", "formula", "guard") || grepl("^list<r(\\||>)", first)) return("r")
  # 3 a call of a known function: its arguments
  if (grepl("^call<", first)) return("group")
  # 4 the types no basic word holds yet (plan Q6): R
  if (identical(first, "call") || grepl("^step<", first) || grepl("^list<(vec|list)<", first) ||
        (grepl("^rows<", first) && is.null(.tbl_type(type)))) {
    return("r")
  }
  # 5-6 a logical: a pick among its other words, else a switch
  if (identical(first, "logical")) return(if (length(m) > 1L && is_input("pick")) "pick_one" else "yesno")
  # 7 a number
  if (first %in% c("number", "integer")) return("number")
  # 8-9 one of a list
  if (grepl("^enum\\[", first) || first %in% c("column", "level", "stat", "colour", "shape", "object")) {
    return("pick_one")
  }
  if (identical(first, "string") && is_input("pick")) return("pick_one")
  # 10 text
  if (first %in% c("string", "template", "date")) return("text")
  # 11 several of a list
  if (grepl("^list<(column|level|stat|string|number|integer|date|enum)", first)) return("pick_many")
  # 12 records
  if (grepl("^(map|vec|rows)<", first) || grepl("^list<map<", first)) return("rows")
  # 13 anything else: R
  "r"
}
