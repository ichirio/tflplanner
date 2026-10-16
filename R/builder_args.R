# A card's arguments as R text, and back: the `R` of a card shows its
# arguments as one line of R (`cols = "TRT", rows = c(group = "variable")`)
# and reads one back.  tflspec's tfl_args_to_r() / tfl_args_from_r() do
# it for the functions its schema knows (the plan verbs, a figure's
# functions): the R as the function takes it, read back into the values as
# the spec holds them.  Two rules are the app's own, kept here: a
# tfl_unset() -- the layers' "back to the default" -- is no argument at all,
# and NULL is `!r NULL`.  A function the schema does not know (and one
# value, in .arg_to_r()) is written and read by the app's own conversion
# below.

# The arguments of one item as R text: "name = value" pieces joined with
# ", ".  A tfl_r() is its text as it is (NULL is `!r NULL`); a tfl_unset()
# is no argument at all.
#' @noRd
args_to_r <- function(fn, args) {
  args <- args[!vapply(args, inherits, NA, "tfl_unset")]
  if (!length(args)) return("")
  if (.args_known(fn)) return(tflspec::tfl_args_to_r(fn, args))
  .args_to_r_own(fn, args)
}

# the app's own: each value by .arg_to_r()
.args_to_r_own <- function(fn, args) {
  nm <- names(args) %||% rep("", length(args))
  pieces <- vapply(seq_along(args), function(i) {
    v <- .arg_to_r(args[[i]])
    if (nzchar(nm[[i]])) paste0(.arg_name(nm[[i]]), " = ", v) else v
  }, "")
  paste(pieces, collapse = ", ")
}

# does tflspec's schema know the function (its conversion applies)?
.args_known <- function(fn) {
  if (is.null(fn) || !is.character(fn) || length(fn) != 1L || !nzchar(fn)) return(FALSE)
  !is.null(.schema_of(fn))
}

# one value as R text
.arg_to_r <- function(v) {
  if (is.null(v)) return("NULL")
  if (inherits(v, "tfl_r")) return(as.character(unclass(v)))
  if (inherits(v, "tfl_call")) {
    return(paste0(v$fn, "(", args_to_r(v$fn, v$args), ")"))
  }
  if (is.list(v)) {
    return(paste0("list(", .args_to_r_own(NULL, v), ")"))
  }
  if (is.atomic(v)) {
    if (is.factor(v)) v <- as.character(v)
    return(paste(deparse(v, width.cutoff = 500L), collapse = " "))
  }
  paste(deparse(v, width.cutoff = 500L), collapse = " ")
}

# a name as R writes it: back-quoted when it is no plain name
.arg_name <- function(x) {
  if (make.names(x) == x) x else paste0("`", gsub("`", "\\\\`", x), "`")
}

# The other way: the text, read as the arguments of a call -- each value
# as the spec holds it, the check's problems as the attribute `problems`
# (tflspec's; a data frame, no rows for none).  A literal (a string, a
# number, TRUE, a c() or list() of them) is its value; NULL is `!r NULL`
# (an !unset means "back to the layers' default", not NULL); anything else
# stays R.  Arguments with no name go to the function's positional `...`
# (plan_sort's `keys`) when the schema names it.
#' @noRd
args_from_r <- function(fn, text) {
  text <- trimws(paste(text, collapse = "\n"))
  if (!nzchar(text)) return(list())
  if (is.null(tryCatch(str2lang(paste0("f(", text, ")")), error = function(e) NULL))) {
    stop(sprintf("The arguments cannot be read as R: %s", text), call. = FALSE)
  }
  if (.args_known(fn)) {
    return(tryCatch(tflspec::tfl_args_from_r(fn, text), error = function(e) {
      stop(sub("^tfl_args_from_r[(][)]: ", "", conditionMessage(e)), call. = FALSE)
    }))
  }
  .args_from_r_own(fn, text)
}

# the app's own reading (a function the schema does not know)
.args_from_r_own <- function(fn, text) {
  cl <- str2lang(paste0("f(", text, ")"))
  exprs <- as.list(cl)[-1L]
  nm <- names(exprs) %||% rep("", length(exprs))
  out <- list()
  pos <- list()
  for (i in seq_along(exprs)) {
    v <- .arg_from_r(exprs[[i]])
    if (nzchar(nm[[i]])) out[[nm[[i]]]] <- v else pos[[length(pos) + 1L]] <- v
  }
  if (length(pos)) {
    key <- .args_positional_key(fn)
    if (is.null(key)) {
      stop(sprintf("%s: an argument with no name, which the form cannot place.",
                   fn %||% "The item"), call. = FALSE)
    }
    v <- if (length(pos) == 1L) pos[[1L]] else .args_simplify(pos)
    out[[key]] <- v
  }
  out
}

# The problems tflspec's check found in arguments read back from R (its
# `problems`, a data frame of fn, arg, level, message) as lines of text;
# none: character()
.args_problems <- function(args) {
  pr <- attr(args, "problems")
  if (!is.data.frame(pr) || !nrow(pr)) return(character())
  arg <- ifelse(is.na(pr$arg) | !nzchar(pr$arg), "", paste0("`", pr$arg, "` "))
  paste0(pr$level, ": ", arg, pr$message)
}

# the arguments with the check's problems left off (to compare or write)
.args_plain <- function(args) {
  attr(args, "problems") <- NULL
  args
}

# the name of the function's positional `...` (plan_sort: keys), from the
# schema; NULL when it has none
.args_positional_key <- function(fn) {
  if (is.null(fn) || !requireNamespace("tflspec", quietly = TRUE)) return(NULL)
  ent <- tryCatch(tflspec::tfl_schema(fn), error = function(e) NULL)
  d <- ent$dots
  if (!is.null(d) && identical(d$style, "positional")) d$key else NULL
}

# one expression: its value when it is a literal, else the text as R
.arg_from_r <- function(e) {
  if (is.null(e)) return(tflspec::tfl_r("NULL"))
  if (is.atomic(e)) return(e)
  if (is.call(e)) {
    head <- as.character(e[[1L]])[1L]
    # a sign on a number: -1, +0.5
    if (head %in% c("-", "+") && length(e) == 2L && is.numeric(e[[2L]])) {
      return(if (head == "-") -e[[2L]] else e[[2L]])
    }
    if (head == "c" && length(e) > 1L) {
      parts <- lapply(as.list(e)[-1L], .arg_from_r)
      if (all(vapply(parts, function(p) is.atomic(p) && !is.null(p) && !is.object(p), NA))) {
        v <- unlist(parts, recursive = FALSE)
        names(v) <- .args_names(e, parts)
        if (is.null(names(v)) || !any(nzchar(names(v)))) names(v) <- NULL
        return(v)
      }
      return(tflspec::tfl_r(.arg_text(e)))
    }
    if (head == "list") {
      parts <- lapply(as.list(e)[-1L], .arg_from_r)
      nms <- names(e)[-1L] %||% rep("", length(parts))
      if (any(nzchar(nms))) names(parts) <- nms
      return(parts)
    }
  }
  tflspec::tfl_r(.arg_text(e))
}

# the names of a c()'s entries: the call's, each spread over the length
# of its entry (c(a = "x", "y") -> a, "")
.args_names <- function(e, parts) {
  nms <- names(e)[-1L]
  if (is.null(nms)) return(NULL)
  unlist(Map(function(n, p) rep(n, length(p)), nms, parts), use.names = FALSE)
}

# several arguments with no name: one vector when they are all scalars of
# a kind, else a list
.args_simplify <- function(pos) {
  if (all(vapply(pos, function(p) is.atomic(p) && length(p) == 1L && !is.object(p), NA))) {
    return(unlist(pos, use.names = FALSE))
  }
  pos
}

.arg_text <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")
