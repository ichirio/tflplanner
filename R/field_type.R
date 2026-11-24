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
#              types no other word holds: step<content>, a list<call>,
#              a rows<..> with no table)
#   group      a function's own arguments as fields (call<fn>)

# the eight words
.field_types <- c("text", "number", "yesno", "pick_one", "pick_many", "rows", "r", "group")

# One definition's basic type: the word (.field_type_info(), from `type`
# and `input` alone, and a step<data>'s `columns:`) with attributes, data
# only until step 3 draws by it --
#   many      a member of the type is a list<..> (several values)
#   ordered   the definition says the order matters (`ordered: true`)
#   free      the definition takes a value not among its choices (`free`;
#             always for a bare call: its names are the def's `values:`)
#   detail    the function names of a call<fn> member the field opens as
#             "Specify in detail" (a Group of that function's arguments);
#             "*" for a bare call, the function the name chosen
#   of        a list<call<fn>>'s function: the rows' columns are its args
#' @noRd
field_type <- function(def) {
  type <- def$type
  info <- .field_type_info(type, .def_input(def), def)
  ok <- !is.null(type) && length(type) == 1L && !is.na(type) && nzchar(trimws(type))
  alts <- if (ok) .type_split(trimws(type), "|") else character()
  structure(info$word, many = any(grepl("^list<", alts)), ordered = isTRUE(def$ordered),
            free = isTRUE(def$free) || isTRUE(info$free), detail = info$detail, of = info$of)
}

# The word of a type and an input form (`def`: step<data> needs its
# `columns:`).
.field_type_word <- function(type, input = NULL, def = NULL) {
  .field_type_info(type, input, def)$word
}

# The members of a type that are plain values a person picks or counts: a
# column, a list of columns, numbers, levels or statistics.
.field_plain_member <- function(m) {
  grepl("^(list<)?(column|level|stat|number|integer)>?$", m)
}

# The word of a type and an input form, with what it opens.  The first
# member that is not `r` decides (a type of `r` alone is R) -- except a
# call<fn> with a plain member beside it, which that member decides; the
# first rule that matches wins.  A list(word, detail, free, of).
.field_type_info <- function(type, input = NULL, def = NULL) {
  out <- function(word, detail = character(), free = FALSE, of = NULL) {
    list(word = word, detail = detail, free = free, of = of)
  }
  if (is.null(type) || length(type) != 1L || is.na(type) || !nzchar(trimws(type))) return(out("r"))
  alts <- .type_split(trimws(type), "|")
  m <- setdiff(alts, "r")
  first <- if (length(m)) m[[1L]] else "r"
  is_input <- function(f) !is.null(input) && identical(input, f)
  # 0 records of records: a list of vectors, a list of calls of a function
  # of the schema -- rows, the columns its own (G4, G5)
  if (grepl("^list<(vec|list)<", first)) return(out("rows"))
  if (grepl("^list<call<[A-Za-z._][A-Za-z0-9._]*>>$", first)) {
    fn <- sub("^list<call<(.*)>>$", "\\1", first)
    if (!is.null(.field_schema_fn(fn))) return(out("rows", of = fn))
  }
  # 1 a table the planner can draw
  if (is_input("table") && !is.null(.tbl_type(type))) return(out("rows"))
  # 2 R: the input says so, or the type is R code
  if (is_input("condition") || is_input("r")) return(out("r"))
  if (first %in% c("r", "formula", "guard") || grepl("^list<r(\\||>)", first)) return(out("r"))
  # 3 a call of a known function: its arguments (a group) -- unless a plain
  # member stands beside it (facets, expansion): that member decides, the
  # call is the "detail" opener (G6)
  if (grepl("^call<", first)) {
    fns <- .field_call_fns(m)
    plain <- m[.field_plain_member(m)]
    if (length(plain)) {
      w <- .field_type_info(plain[[1L]], "pick")
      return(out(w$word, detail = fns))
    }
    return(out("group", detail = fns))
  }
  # 4 a bare call with the def's names (`values:`): one of them, a value of one's own too;
  # its arguments are the detail (G1).  step<data>: a group of its
  # `columns:` (G3).  The types no basic word holds yet: R
  if (identical(first, "call") && .field_has_names(def)) return(out("pick_one", detail = "*", free = TRUE))
  if (identical(first, "call")) return(out("r"))
  if (identical(first, "step<data>") && length(def$columns)) return(out("group"))
  if (grepl("^step<", first) || grepl("^list<call", first) ||
        (grepl("^rows<", first) && is.null(.tbl_type(type)))) {
    return(out("r"))
  }
  # 5-6 a logical: a pick among its other words, else a switch
  detail <- .field_call_fns(m)
  if (identical(first, "logical")) {
    return(out(if (length(m) > 1L && is_input("pick")) "pick_one" else "yesno", detail = detail))
  }
  # 7 a number
  if (first %in% c("number", "integer")) return(out("number", detail = detail))
  # 8-9 one of a list
  if (grepl("^enum\\[", first) || first %in% c("column", "level", "stat", "colour", "shape", "object")) {
    return(out("pick_one", detail = detail))
  }
  if (identical(first, "string") && is_input("pick")) return(out("pick_one", detail = detail))
  # 10 text
  if (first %in% c("string", "template", "date")) return(out("text", detail = detail))
  # 11 several of a list
  if (grepl("^list<(column|level|stat|string|number|integer|date|enum)", first)) {
    return(out("pick_many", detail = detail))
  }
  # 12 records
  if (grepl("^(map|vec|rows)<", first) || grepl("^list<map<", first)) return(out("rows"))
  # 13 anything else: R
  out("r")
}

# The functions of a type's call<fn, fn2> members (a bare call: none)
.field_call_fns <- function(m) {
  c <- m[grepl("^call<", m)]
  if (!length(c)) return(character())
  fns <- trimws(unlist(strsplit(sub("^call<(.*)>$", "\\1", c), ",")))
  unique(fns[nzchar(fns)])
}

# A function of tflspec's schema (its entry), NULL when it is none
.field_schema_fn <- function(fn) {
  if (!requireNamespace("tflspec", quietly = TRUE)) return(NULL)
  tryCatch(tflspec::tfl_schema(fn), error = function(e) NULL)
}

# Whether a def lists names to pick: its `values:` beyond the TRUE / FALSE a
# logical member beside others gets
.field_has_names <- function(def) {
  length(setdiff(as.character(unlist(def$values)), c("TRUE", "FALSE"))) > 0L
}
