# Analyses run together: an analysis whose method is cards::ard_stack()
# (the parent) runs the analyses that name it in their `parent` column, on
# its data, analysis set, condition and groups, in one call (tflspec writes
# it).  Here: the outline of a report's analyses, and grouping, ungrouping,
# taking one out, adding one inside, moving one -- on the ARD definition's
# analyses sheet, which is not changed otherwise.  ard_strata() and
# ard_pairwise() parents are read the same way; the screen offers
# ard_stack() first.

.stack_fn <- "cards::ard_stack"
.stack_wrappers <- c("cards::ard_stack", "cards::ard_strata", "cards::ard_pairwise")
# what a row inside takes from its parent (it must leave them blank)
.stack_own <- c("dataset", "population_id", "where", "by", "strata")
# the methods that cannot run inside one
.stack_not_inside <- c("subjects", "custom", .stack_wrappers)
# ard_stack()'s own switches the form ticks: cards' default.  `.overall`
# (each analysis again with no groups) is not one of them: a table may make
# its Total column as well, so it stays among the other arguments
.stack_flags <- c(.by_stats = TRUE, .total_n = FALSE, .missing = FALSE,
                  .attributes = FALSE)

.is_blank <- function(x) is.null(x) || length(x) == 0L || is.na(x) || !nzchar(trimws(x))

# A report's analyses in the order the outline shows them: each analysis
# not inside another in the sheet's order, the ones inside a parent right
# after it (in the sheet's order); `depth` 1 for those.  A row naming a
# parent the report has not got is shown at the top level.
.an_outline <- function(a) {
  if (is.null(a) || !nrow(a)) {
    return(data.frame(analysis_id = character(), depth = integer(),
                      parent = character(), stringsAsFactors = FALSE))
  }
  par <- if ("parent" %in% names(a)) a$parent else rep(NA_character_, nrow(a))
  par[!is.na(par) & !nzchar(trimws(par))] <- NA
  ids <- a$analysis_id
  inside <- !is.na(par) & par %in% ids
  out <- integer()
  depth <- integer()
  for (i in which(!inside)) {
    out <- c(out, i)
    depth <- c(depth, 0L)
    kids <- which(inside & par == ids[i])
    out <- c(out, kids)
    depth <- c(depth, rep(1L, length(kids)))
  }
  data.frame(analysis_id = ids[out], depth = depth,
             parent = ifelse(depth == 1L, par[out], NA_character_),
             row = out, stringsAsFactors = FALSE)
}

# The rows inside a parent, in the sheet's order
.stack_kids <- function(a, parent) {
  par <- if ("parent" %in% names(a)) a$parent else rep(NA_character_, nrow(a))
  which(!is.na(par) & par == parent)
}

# An id not yet used in the report: base, base2, base3 ...
.stack_free_id <- function(a, base = "STACK") {
  id <- base
  k <- 1L
  while (id %in% a$analysis_id) {
    k <- k + 1L
    id <- paste0(base, k)
  }
  id
}

# ard_stack()'s switches as an analysis's args say them (cards' defaults
# where it says nothing), and what the args keep besides
.stack_flags_of <- function(args) {
  f <- .stack_flags
  other <- character()
  if (!.is_blank(args)) {
    e <- tryCatch(parse(text = paste0("f(", args, "\n)"), keep.source = FALSE)[[1L]],
                  error = function(e) NULL)
    if (is.null(e)) return(list(flags = f, other = args))
    parts <- as.list(e)[-1L]
    nm <- names(parts) %||% rep("", length(parts))
    for (k in seq_along(parts)) {
      if (nm[k] %in% names(f) && is.logical(parts[[k]]) && length(parts[[k]]) == 1L) {
        f[[nm[k]]] <- isTRUE(parts[[k]])
      } else if (!identical(nm[k], ".shuffle")) {
        other <- c(other, if (nzchar(nm[k])) paste(nm[k], "=", deparse1(parts[[k]]))
                   else deparse1(parts[[k]]))
      }
    }
  }
  list(flags = f, other = paste(other, collapse = ", "))
}

# The args for ard_stack()'s switches: only those not at cards' default,
# then what else the args said
.stack_args <- function(flags, other = "") {
  f <- .stack_flags
  f[names(flags)] <- as.logical(flags)
  set <- names(f)[f != .stack_flags]
  parts <- c(if (length(set)) paste(set, "=", ifelse(f[set], "TRUE", "FALSE")),
             if (!.is_blank(other)) trimws(other))
  if (length(parts)) paste(parts, collapse = ", ") else NA_character_
}

# Why an analysis cannot go into a stack with the first one, or NA: the
# same data, analysis set, condition and groups; not a subject count, own
# code or another stack; no variable another one in it has already
.stack_reason <- function(a, i, with, vars_in = character()) {
  r <- a[i, ]
  w <- a[with, ]
  same <- function(cn) identical(if (.is_blank(r[[cn]])) NA else r[[cn]],
                                 if (.is_blank(w[[cn]])) NA else w[[cn]])
  if (!.is_blank(r$parent %||% NA)) return("inside")
  # the subjects per group or in all: the stack can count them itself
  if (length(.stack_group_n(a[i, , drop = FALSE], w))) return("group_n")
  if (r$method %in% "total_n" && same("dataset") && same("population_id")) return("total_n")
  if (r$method %in% .stack_not_inside) return("method")
  if (!same("dataset") || !same("population_id")) return("data")
  if (!same("where")) return("where")
  if (!same("by")) return("by")
  if (!same("strata")) return("strata")
  if (!same("denominator")) return("denominator")
  if (any(.split_bar(r$variables) %in% vars_in)) return("variable")
  NA_character_
}

# The report's analyses that can go into a stack with analysis `id`: one row
# each, `reason` NA when it can (the analysis itself first)
stack_candidates <- function(x, output_id, id) {
  a <- ard_rows(x, "analyses", output_id)
  w <- match(id, a$analysis_id)
  if (is.na(w)) stop("'", id, "' is not an analysis of ", output_id, ".", call. = FALSE)
  others <- setdiff(seq_len(nrow(a)), w)
  vars <- .split_bar(a$variables[w])
  reason <- character()
  for (i in others) {
    rs <- .stack_reason(a, i, w, vars)
    reason <- c(reason, rs)
  }
  data.frame(analysis_id = c(id, a$analysis_id[others]),
             reason = c(.stack_reason(a, w, w), reason),
             stringsAsFactors = FALSE)
}

# Run analyses together: a new ard_stack() parent (`STACK`, the next free
# id) with the data, analysis set, condition and groups they share, and
# the analyses inside it (those columns left blank)
stack_group <- function(x, output_id, ids, parent = NULL, label = NA_character_) {
  a <- ard_rows(x, "analyses", output_id)
  a$output_id <- NULL
  rows <- match(ids, a$analysis_id)
  if (anyNA(rows) || !length(rows)) stop("Choose analyses of ", output_id, ".", call. = FALSE)
  for (i in rows[-1L]) {
    rs <- .stack_reason(a, i, rows[1L])
    if (!is.na(rs)) stop("'", a$analysis_id[i], "' cannot run with '", ids[1L],
                         "' (", rs, ").", call. = FALSE)
  }
  vars <- unlist(lapply(rows, function(i) .split_bar(a$variables[i])))
  if (anyDuplicated(vars)) {
    stop("A variable is in two of them: ", paste(unique(vars[duplicated(vars)]), collapse = ", "),
         ".", call. = FALSE)
  }
  if (is.null(parent)) parent <- .stack_free_id(a)
  first <- a[rows[1L], ]
  p <- a[0L, , drop = FALSE]
  p[1L, ] <- NA
  p$analysis_id <- parent
  p$label <- label
  p$method <- .stack_fn
  for (cn in .stack_own) p[[cn]] <- first[[cn]]
  # the report counts the subjects per group already (BIGN): the stack
  # does not count them again -- the column headers would read two N
  if (length(.stack_group_n(a, first))) p$args <- ".by_stats = FALSE"
  if (!"parent" %in% names(a)) a$parent <- NA_character_
  for (i in rows) {
    a$parent[i] <- parent
    for (cn in .stack_own) a[[cn]][i] <- NA
  }
  # the parent where the first of them was; the others after it
  at <- min(rows)
  inside <- a[rows, , drop = FALSE]
  rest <- a[-rows, , drop = FALSE]
  before <- rest[seq_len(sum(seq_len(nrow(a))[-rows] < at)), , drop = FALSE]
  after <- rest[setdiff(seq_len(nrow(rest)), seq_len(nrow(before))), , drop = FALSE]
  set_ard_rows(x, "analyses", output_id, rbind(before, p, inside, after))
}

# The report's analyses that count the subjects per group of `r` (its by
# variable counted, by nothing, on the same data): BIGN
.stack_group_n <- function(a, r) {
  by <- .split_bar(r$by)
  if (!length(by)) return(integer())
  same <- function(x, y) identical(if (.is_blank(x)) NA else x, if (.is_blank(y)) NA else y)
  which(vapply(seq_len(nrow(a)), function(i) {
    a$method[i] %in% c("categorical", "subjects") &&
      identical(.split_bar(a$variables[i]), by) && .is_blank(a$by[i]) &&
      .is_blank(a$parent[i] %||% NA) &&
      same(a$dataset[i], r$dataset) && same(a$population_id[i], r$population_id)
  }, NA))
}

# An analysis that leaves its parent: it gets the parent's data, analysis
# set, condition and groups back (so it computes what it did, but a group's
# missing rows: ard_stack() leaves them out)
.stack_out_row <- function(r, p) {
  for (cn in .stack_own) if (.is_blank(r[[cn]])) r[[cn]] <- p[[cn]]
  r$parent <- NA_character_
  r
}

# The rows that keep what a stack's parent gave the table besides its
# analyses: the subjects per group (`BIGN`: the group counted) and the
# total N (`TOTAL`), when its switches gave them
.stack_n_rows <- function(a, p) {
  f <- .stack_flags_of(p$args)$flags
  out <- a[0L, , drop = FALSE]
  # what the report counts already is not made again (two N break the
  # column headers)
  has_bign <- length(.stack_group_n(a, p)) > 0L
  has_total <- any(a$method %in% "total_n" & .is_blank_v(a$parent) &
                     .same_v(a$dataset, p$dataset) & .same_v(a$population_id, p$population_id))
  add <- function(id, method, variables, by) {
    r <- a[0L, , drop = FALSE]
    r[1L, ] <- NA
    r$analysis_id <- .stack_free_id(rbind(a, out), id)
    r$method <- method
    r$dataset <- p$dataset
    r$population_id <- p$population_id
    r$where <- p$where
    r$variables <- variables
    r$by <- by
    r$label <- if (id == "BIGN") "Subjects per group" else "Total N"
    out <<- rbind(out, r)
  }
  by <- .split_bar(p$by)
  if (isTRUE(f[[".by_stats"]]) && length(by) && !has_bign) {
    add("BIGN", "categorical", paste(by, collapse = " | "), NA)
  }
  if (isTRUE(f[[".total_n"]]) && !has_total) add("TOTAL", "total_n", NA, NA)
  out
}

.is_blank_v <- function(x) is.na(x) | !nzchar(trimws(x))
.same_v <- function(x, y) {
  y <- if (.is_blank(y)) NA_character_ else y
  ifelse(.is_blank_v(x), is.na(y), !is.na(y) & x == y)
}

# A report's analyses that count the subjects per group twice: for each
# group, the rows counting it (BIGN: the group counted by nothing; a stack
# by it that counts them, `.by_stats`), when there are two or more on the
# same data -- a column header's N reads two and shows none.  One row a
# report's analysis: `analysis_id`, `with` (the others).
stack_n_twice <- function(a) {
  if (is.null(a) || !nrow(a)) return(data.frame(analysis_id = character(), with = character()))
  if (!"parent" %in% names(a)) a$parent <- NA_character_
  key <- rep(NA_character_, nrow(a))
  for (i in seq_len(nrow(a))) {
    r <- a[i, ]
    if (!.is_blank(r$parent)) next
    d <- paste(if (.is_blank(r$dataset)) "" else r$dataset,
               if (.is_blank(r$population_id)) "" else r$population_id, sep = "|")
    if (r$method %in% c("categorical", "subjects") && .is_blank(r$by) &&
        length(.split_bar(r$variables)) == 1L) {
      key[i] <- paste(d, .split_bar(r$variables))
    } else if (identical(r$method, .stack_fn) && length(.split_bar(r$by)) &&
               isTRUE(.stack_flags_of(r$args)$flags[[".by_stats"]])) {
      key[i] <- paste(d, paste(.split_bar(r$by), collapse = " | "))
    }
  }
  dup <- !is.na(key) & key %in% key[duplicated(key) & !is.na(key)]
  data.frame(analysis_id = a$analysis_id[dup],
             with = vapply(which(dup), function(i)
               paste(a$analysis_id[setdiff(which(key == key[i]), i)], collapse = ", "), ""),
             stringsAsFactors = FALSE)
}

# Undo a stack: each analysis inside gets the parent's data back, the
# parent goes; with `keep_n`, rows that keep the subjects per group and the
# total N it gave (the column headers' N)
stack_ungroup <- function(x, output_id, parent, keep_n = TRUE) {
  a <- ard_rows(x, "analyses", output_id)
  a$output_id <- NULL
  pi <- match(parent, a$analysis_id)
  if (is.na(pi)) stop("'", parent, "' is not an analysis of ", output_id, ".", call. = FALSE)
  p <- a[pi, ]
  for (k in .stack_kids(a, parent)) a[k, ] <- .stack_out_row(a[k, ], p)
  n <- if (keep_n) .stack_n_rows(a, p) else a[0L, , drop = FALSE]
  a <- rbind(a[seq_len(pi - 1L), , drop = FALSE], n,
             a[setdiff(seq_len(nrow(a)), seq_len(pi)), , drop = FALSE])
  set_ard_rows(x, "analyses", output_id, a)
}

# One analysis out of its stack, as one of its own; the stack, left with
# none, goes (with `keep_n`, as stack_ungroup())
stack_take_out <- function(x, output_id, id, keep_n = TRUE) {
  a <- ard_rows(x, "analyses", output_id)
  a$output_id <- NULL
  i <- match(id, a$analysis_id)
  if (is.na(i) || .is_blank(a$parent[i])) return(x)
  parent <- a$parent[i]
  pi <- match(parent, a$analysis_id)
  if (length(.stack_kids(a, parent)) == 1L) {
    return(stack_ungroup(x, output_id, parent, keep_n = keep_n))
  }
  a[i, ] <- .stack_out_row(a[i, ], a[pi, ])
  set_ard_rows(x, "analyses", output_id, a)
}

# A parent deleted: its analyses ungrouped first (the default), or deleted
# with it
stack_remove <- function(x, output_id, parent, how = c("ungroup", "all"), keep_n = TRUE) {
  how <- match.arg(how)
  if (how == "ungroup") return(stack_ungroup(x, output_id, parent, keep_n = keep_n))
  a <- ard_rows(x, "analyses", output_id)
  a$output_id <- NULL
  drop <- c(match(parent, a$analysis_id), .stack_kids(a, parent))
  set_ard_rows(x, "analyses", output_id, a[-drop[!is.na(drop)], , drop = FALSE])
}

#' Delete one analysis of a report
#'
#' Takes an analysis out of a report's ARD definition: one of its own, or
#' one inside a stack (the stack keeps the others).  A stack itself is
#' deleted with [stack_remove()], which says what becomes of the analyses
#' inside it.
#'
#' @param x A `tflplanner`.
#' @param output_id The report.
#' @param id The analysis.
#' @return `x` without the analysis.
#' @export
remove_analysis <- function(x, output_id, id) {
  a <- ard_rows(x, "analyses", output_id)
  a$output_id <- NULL
  i <- match(id, a$analysis_id)
  if (is.na(i)) stop("'", id, "' is not an analysis of ", output_id, ".", call. = FALSE)
  if (a$method[i] %in% .stack_wrappers || length(.stack_kids(a, id))) {
    stop("'", id, "' is a stack: delete it with stack_remove() (its Delete button), ",
         "which says what becomes of the analyses inside.", call. = FALSE)
  }
  set_ard_rows(x, "analyses", output_id, a[-i, , drop = FALSE])
}

# A new analysis inside a parent (after its last one): its id, method and
# variables; nothing of its own on the data
stack_add_inside <- function(x, output_id, parent, id = NULL, method = "continuous",
                             variables = NA_character_) {
  a <- ard_rows(x, "analyses", output_id)
  a$output_id <- NULL
  pi <- match(parent, a$analysis_id)
  if (is.na(pi)) stop("'", parent, "' is not an analysis of ", output_id, ".", call. = FALSE)
  if (!identical(a$method[pi], .stack_fn) && length(.stack_kids(a, parent))) {
    stop(a$method[pi], "() runs one analysis.", call. = FALSE)
  }
  if (is.null(id)) id <- .stack_free_id(a, "AN")
  r <- a[0L, , drop = FALSE]
  r[1L, ] <- NA
  r$analysis_id <- id
  r$parent <- parent
  r$method <- method
  r$variables <- variables
  kids <- .stack_kids(a, parent)
  at <- max(c(pi, kids))
  a <- rbind(a[seq_len(at), , drop = FALSE], r,
             a[setdiff(seq_len(nrow(a)), seq_len(at)), , drop = FALSE])
  set_ard_rows(x, "analyses", output_id, a)
}

# An analysis inside a stack one place up or down among the others in it:
# the ARD keeps their order, and so does a table whose variables have no
# order of their own
stack_move <- function(x, output_id, id, by = -1L) {
  a <- ard_rows(x, "analyses", output_id)
  a$output_id <- NULL
  i <- match(id, a$analysis_id)
  if (is.na(i) || .is_blank(a$parent[i])) return(x)
  kids <- .stack_kids(a, a$parent[i])
  k <- match(i, kids) + by
  if (k < 1L || k > length(kids)) return(x)
  j <- kids[k]
  a[c(i, j), ] <- a[c(j, i), ]
  set_ard_rows(x, "analyses", output_id, a)
}

# A parent renamed: the analyses inside follow
stack_rename <- function(x, output_id, from, to) {
  a <- ard_rows(x, "analyses", output_id)
  if (identical(from, to) || !"parent" %in% names(a)) return(x)
  a$output_id <- NULL
  a$parent[!is.na(a$parent) & a$parent == from] <- to
  set_ard_rows(x, "analyses", output_id, a)
}

# the words of stack_n_twice() on screen (translated by the app)
.n_twice_words <- "%s counts the subjects per group, and so does %s: a column header's N shows none then. Keep one of them."
