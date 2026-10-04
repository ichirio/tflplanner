# The ARD tab's analysis form: what to compute (the company's keywords and
# every ard_*() function of tflspec's catalog, by category) and the fields of
# the chosen function's own arguments, written to the analysis row's `args`
# column (the ARD definition is not changed: method, by, variables, strata,
# denominator and statistics keep their columns).

# The functions an analysis can name, one row each: `value` (what the
# `method` column takes), `label`, `description`, `category`, `call`,
# `state` ("ok", "missing" = its package is not installed, "later" = a
# function that runs others, not yet declared in the form, "old" = an old
# name and "out" = a function the builder does not offer, each kept only for
# the row that uses it).  `keywords` is
# .std_ard_methods(); `functions` tfl_ard_functions().
.ard_fn_entries <- function(keywords, functions, current = NA_character_,
                            company = "Company standard") {
  kw <- data.frame(value = keywords$method, label = keywords$label,
                   label_en = keywords$label,
                   description = keywords$note %||% NA_character_,
                   category = company, call = keywords$call,
                   state = "ok", stringsAsFactors = FALSE)
  f <- functions
  # out of the builder's scope (the 2026-10-04 decision, Q9): the catalog's
  # `offered` column says so (survey designs, ard_formals() ...) -- shown,
  # like an old name, only for the analysis that names one
  out_of_scope <- if ("offered" %in% names(f)) !f$offered %in% TRUE else
    f$category %in% "Survey designs" | f$call %in% "cards::ard_formals"
  state <- ifelse(!f$installed, "missing",
                  ifelse(!is.na(f$replaced_by) & nzchar(f$replaced_by), "old",
                         ifelse(out_of_scope, "out",
                                ifelse(f$shape %in% "wrapper", "later", "ok"))))
  fn <- data.frame(value = f$call, label = f$label, label_en = f$label,
                   description = f$description, category = f$category,
                   call = f$call, state = state, stringsAsFactors = FALSE)
  out <- rbind(kw, fn)
  # an old name only for the row that uses it
  out <- out[!out$state %in% c("old", "out") | out$value %in% current, , drop = FALSE]
  # a method the catalog does not know (a study's own function): kept
  if (!is.na(current) && nzchar(current) && !current %in% out$value) {
    out <- rbind(out, data.frame(value = current, label = current,
                                 label_en = current,
                                 description = NA_character_,
                                 category = "Own and code", call = current,
                                 state = "ok", stringsAsFactors = FALSE))
  }
  rownames(out) <- NULL
  out
}

# The function an analysis method calls ("cards::ard_summary"), or NA for a
# keyword with no function (subjects, custom code)
.ard_method_call <- function(method, keywords) {
  if (is.null(method) || is.na(method) || !nzchar(method)) return(NA_character_)
  k <- match(method, keywords$method)
  cl <- if (is.na(k)) method else keywords$call[k]
  if (grepl("^[(]", cl) || !grepl("::", cl, fixed = TRUE)) NA_character_ else cl
}

# The arguments the form asks for: those the catalog writes to `args`, but
# not the data, the statistics, the denominator (their own fields) nor the
# formatting and labels (the table's)
.ard_form_fields <- function(call) {
  if (is.na(call)) return(NULL)
  a <- tryCatch(tflspec::tfl_ard_args(call), error = function(e) NULL)
  if (is.null(a) || !nrow(a)) return(NULL)
  a[a$column %in% "args" &
      !a$kind %in% c("data", "statistics", "denominator") &
      !a$arg %in% c("fmt_fun", "stat_label"), , drop = FALSE]
}

# `args` ("method = \"wilson\", conf.level = 0.9") into the fields' values
# (a named list of character vectors) and what the fields cannot hold, kept
# as R (`other`): nothing written by hand is lost
.ard_args_parse <- function(args, fields) {
  out <- list(values = list(), other = "")
  if (is.null(args) || is.na(args) || !nzchar(trimws(args))) return(out)
  e <- tryCatch(parse(text = paste0("f(", args, "\n)"), keep.source = FALSE)[[1L]],
                error = function(e) NULL)
  if (is.null(e)) {
    out$other <- args
    return(out)
  }
  ex <- as.list(e)[-1L]
  nm <- names(ex) %||% rep("", length(ex))
  rest <- character()
  for (i in seq_along(ex)) {
    k <- if (nzchar(nm[i]) && !is.null(fields)) match(nm[i], fields$arg) else NA
    v <- if (!is.na(k)) .ard_arg_value(ex[[i]], fields$kind[k])
    if (is.null(v)) {
      d <- paste(deparse(ex[[i]], width.cutoff = 500L), collapse = " ")
      rest <- c(rest, if (nzchar(nm[i])) paste(nm[i], "=", d) else d)
    } else {
      out$values[[nm[i]]] <- v
    }
  }
  out$other <- paste(rest, collapse = ", ")
  out
}

# one argument's expression as its field's value, or NULL when the field
# cannot show it
.ard_arg_value <- function(x, kind) {
  is_const <- function(x, f) !is.call(x) && !is.name(x) && length(x) == 1L && f(x)
  switch(kind,
    choice = , text = if (is_const(x, is.character)) x,
    number = if (is_const(x, is.numeric)) format(x),
    logical = if (is_const(x, is.logical) && !is.na(x)) as.character(x),
    column = if (is.name(x)) as.character(x) else
      if (is_const(x, is.character)) x,
    columns = {
      if (is.name(x)) as.character(x)
      else if (is.call(x) && identical(x[[1L]], as.name("c")) &&
               all(vapply(as.list(x)[-1L], is.name, NA)))
        vapply(as.list(x)[-1L], as.character, "")
    },
    # one level for every variable: everything() ~ "Y"
    levels = {
      if (is.call(x) && identical(x[[1L]], as.name("~")) && length(x) == 3L &&
          identical(x[[2L]], quote(everything())) &&
          !is.call(x[[3L]]) && !is.name(x[[3L]]) && length(x[[3L]]) == 1L)
        as.character(x[[3L]])
    },
    formula = , code = paste(deparse(x, width.cutoff = 500L), collapse = " "),
    NULL)
}

# the fields' values (and `other`) back into `args`, in the catalog's order;
# NA when there is nothing.  `values`: a named list, "" or NULL = not set.
.ard_args_build <- function(values, fields, other = "") {
  parts <- character()
  for (i in seq_len(nrow(fields %||% data.frame()))) {
    a <- fields$arg[i]
    v <- values[[a]]
    v <- v[!is.na(v) & nzchar(trimws(v))]
    if (!length(v)) next
    v <- trimws(v)
    s <- switch(fields$kind[i],
      choice = , text = encodeString(v[1L], quote = "\""),
      number = v[1L],
      logical = v[1L],
      column = v[1L],
      columns = if (length(v) == 1L) v else
        paste0("c(", paste(v, collapse = ", "), ")"),
      levels = {
        num <- suppressWarnings(!is.na(as.numeric(v[1L])))
        paste0("everything() ~ ", if (num) v[1L] else encodeString(v[1L], quote = "\""))
      },
      v[1L])
    parts <- c(parts, paste(a, "=", s))
  }
  other <- trimws(other %||% "")
  if (nzchar(other)) parts <- c(parts, other)
  if (!length(parts)) NA_character_ else paste(parts, collapse = ", ")
}

# Do two `args` say the same (the order of the arguments aside)?  Then the
# row keeps what was written.
.ard_args_same <- function(a, b) {
  norm <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character())
    e <- tryCatch(parse(text = paste0("f(", x, "\n)"), keep.source = FALSE)[[1L]],
                  error = function(e) NULL)
    if (is.null(e)) return(x)
    ex <- as.list(e)[-1L]
    nm <- names(ex) %||% rep("", length(ex))
    d <- vapply(ex, function(z) paste(deparse(z, width.cutoff = 500L), collapse = " "), "")
    sort(paste(nm, d, sep = "="))
  }
  identical(norm(a), norm(b))
}

# The levels a level field offers for the analysis's variables: the study's
# code lists first (in their order), then the data's factor levels, then the
# values the data has
.level_choices <- function(vars, codelists, data) {
  out <- character()
  if (!is.null(codelists) && nrow(codelists)) {
    cl <- codelists[codelists$variable %in% vars, , drop = FALSE]
    ord <- suppressWarnings(as.numeric(cl$order))
    out <- c(out, cl$value[order(cl$variable, is.na(ord), ord)])
  }
  for (v in intersect(vars, names(data %||% list()))) {
    x <- data[[v]]
    out <- c(out, if (is.factor(x)) levels(x) else
      sort(unique(as.character(x[!is.na(x)]))))
  }
  unique(out[!is.na(out) & nzchar(out)])
}

# What a field's empty choice says: the default, named when it is one value
# ("(default: waldcc)": the first of a choice's values)
.ard_default_label <- function(kind, default, choices, plain, named) {
  d <- NA_character_
  if (kind == "choice" && length(choices)) d <- choices[1L]
  else if (!is.na(default) && nchar(default) <= 30 && !grepl("[(]", default))
    d <- gsub('^"|"$', "", default)
  if (is.na(d) || !nzchar(d) || d == "NULL") plain else sprintf(named, d)
}
