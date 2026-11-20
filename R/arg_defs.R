# The argument definitions of the cards: tflspec's tfl_arg_defs() (one
# table for every card -- a schema function's arguments, a data: row's and
# an analysis item's keys, a template's params), read once a session, and
# the choices of a "pick" resolved from its source words.
#
#   .arg_defs(fn = , template = )    the table, NULL when tflspec has none
#   .arg_def(defs, name)             one row as a list
#   .choice_values(sources, ctx)     the source words -> the choices offered
#                                    (ctx: vars, levels, ..., args, arg_names, params)
#   .tpl_params_from_defs(defs)      a template's defs as its card's params

# ---- the table, cached -------------------------------------------------------

.arg_defs_cache <- new.env()

# tflspec's definitions of a card's arguments: `fn` a schema function, "data"
# or "analysis"; `template` "<series>/<template>".  NULL when tflspec has no
# tfl_arg_defs() or the call stops (a template with no series file yet).
# Read once a session (.arg_defs_reset() forgets them).
.arg_defs <- function(fn = NULL, template = NULL) {
  if (is.null(fn) == is.null(template)) return(NULL)
  key <- if (!is.null(fn)) paste0("fn:", fn) else paste0("template:", template)
  if (exists(key, envir = .arg_defs_cache, inherits = FALSE)) {
    return(get(key, envir = .arg_defs_cache, inherits = FALSE))
  }
  f <- if (requireNamespace("tflspec", quietly = TRUE)) {
    get0("tfl_arg_defs", envir = asNamespace("tflspec"), inherits = FALSE)
  }
  out <- if (is.function(f)) {
    tryCatch({
      d <- if (!is.null(fn)) f(fn = fn) else f(template = template)
      if (is.data.frame(d)) d
    }, error = function(e) NULL)
  }
  assign(key, out, envir = .arg_defs_cache)
  out
}

.arg_defs_reset <- function() {
  rm(list = ls(.arg_defs_cache, all.names = TRUE), envir = .arg_defs_cache)
  invisible()
}

# One argument's row as a list (its list columns unwrapped); NULL when the
# table has no such argument
.arg_def <- function(defs, name) {
  if (!is.data.frame(defs) || !nrow(defs) || is.null(name)) return(NULL)
  i <- match(name, defs$name)
  if (is.na(i)) return(NULL)
  row <- lapply(names(defs), function(k) {
    v <- defs[[k]][[i]]
    if (is.factor(v)) v <- as.character(v)
    v
  })
  names(row) <- names(defs)
  row
}

# The fields a def adds to a schema argument (.schema_of())
.arg_def_fields <- c("input", "choices", "values", "free", "ordered", "columns", "step",
                     "tier", "required", "unset", "default", "default_kind")

# A schema entry with its arguments' definitions: each argument gains the
# def's fields (.arg_def_fields) -- its label and help stay the schema's,
# the def's description the help when the schema has none; a default the
# schema gives is kept.  No defs: the entry as it was.
.schema_with_defs <- function(ent, defs) {
  if (is.null(ent) || !is.data.frame(defs) || !nrow(defs) || !length(ent$args)) return(ent)
  # (not `...`: tflspec's row of the dots describes them, a card has no
  # field of them)
  for (a in setdiff(intersect(names(ent$args), defs$name), "...")) {
    d <- .arg_def(defs, a)
    x <- ent$args[[a]] %||% list()
    for (k in setdiff(.arg_def_fields, c("default", "default_kind"))) {
      v <- d[[k]]
      if (length(v) && !(length(v) == 1L && is.atomic(v) && is.na(v))) x[[k]] <- v
    }
    dv <- d[["default"]]
    if (is.null(x[["default"]]) && !is.null(dv) && !inherits(dv, "tfl_tagged")) x[["default"]] <- dv
    # (its kind only beside a default: `$default` matches no default_kind)
    if (!is.null(x[["default"]]) && length(d$default_kind) == 1L && !is.na(d$default_kind)) {
      x$default_kind <- d$default_kind
    }
    if (is.null(x$help) && is.null(x$description) && length(d$description) &&
          !is.na(d$description) && nzchar(d$description)) {
      x$help <- d$description
    }
    ent$args[[a]] <- x
  }
  ent
}

# ---- the choices of a pick ---------------------------------------------------

# the closed list of choice sources (tflspec's canon 11-14)
.choice_sources <- c("vars", "levels", "codelist", "populations", "report_data",
                     "study_data", "ard", "columns", "study_info", "tokens",
                     "stats", "fixed", "styles", "from_args")

# the name of a source's group, where several make the list
.choice_group_labels <- c(
  vars = "Columns", levels = "Values", codelist = "Code list", populations = "Analysis sets",
  report_data = "This report's data", study_data = "Study data", ard = "ARD",
  columns = "Columns", study_info = "Study information", tokens = "Tokens",
  stats = "Statistics", fixed = "Fixed values", styles = "Styles", from_args = "Arguments")

# "report_data, study_data" or c("vars(from)", "from_args(a, b)") -> the
# words, a comma inside brackets kept with its word
.choice_words <- function(sources) {
  if (is.null(sources) || !length(sources)) return(character())
  sources <- as.character(unlist(sources))
  sources <- sources[!is.na(sources)]
  if (!length(sources)) return(character())
  chars <- strsplit(paste(sources, collapse = ", "), "", fixed = TRUE)[[1L]]
  depth <- 0L
  for (i in seq_along(chars)) {
    if (chars[i] == "(") depth <- depth + 1L
    if (chars[i] == ")") depth <- depth - 1L
    if (chars[i] == "," && depth > 0L) chars[i] <- "\001"
  }
  parts <- trimws(strsplit(paste(chars, collapse = ""), ",", fixed = TRUE)[[1L]])
  parts <- gsub("\001", ",", parts, fixed = TRUE)
  parts[nzchar(parts)]
}

# a word's name and what is in its brackets: "vars(adsl)" -> list("vars",
# "adsl"); "vars(from_args(from))" -> list("vars", "from_args(from)") (a
# comma inside inner brackets kept with its part)
.choice_word_parse <- function(w) {
  name <- trimws(sub("[(].*$", "", w))
  inner <- if (grepl("(", w, fixed = TRUE)) sub("^[^(]*[(](.*)[)]\\s*$", "\\1", w) else ""
  args <- .choice_words(inner)
  list(name = name, args = args[nzchar(args)])
}

# what the ctx holds for `key` (a vector, a function of no argument or of
# `what`, a list by name); NULL when it cannot say
.choice_ctx <- function(ctx, key, what = NULL) {
  x <- ctx[[key]]
  if (is.null(x)) return(NULL)
  if (is.function(x)) {
    return(tryCatch(if (length(formals(x))) x(what) else x(), error = function(e) NULL))
  }
  if (is.list(x) && !is.data.frame(x)) {
    if (is.null(what)) return(NULL)
    return(x[[what]])
  }
  x
}

# What a word's brackets name (tflspec#821's rule): a name is itself (the
# data in vars(adsl), the variable in levels(SEX)); from_args(<arg>) is the
# value of the card's argument <arg> (vars(from_args(from)): the columns of
# the data `from` names).  NULL when that argument has no single value.
.choice_ref <- function(ctx, name) {
  if (!grepl("^from_args\\s*[(].*[)]\\s*$", name)) return(name)
  a <- .choice_word_parse(name)$args
  v <- if (length(a)) ctx$args[[a[[1L]]]]
  if (is.atomic(v) && length(v) == 1L && !is.na(v) && nzchar(v)) as.character(v)
}

# a vector of choices (names its labels), the blanks out
.choice_vec <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  if (is.data.frame(x)) x <- names(x)
  if (is.list(x)) x <- unlist(x)
  nm <- names(x)
  out <- as.character(x)
  names(out) <- if (is.null(nm)) out else ifelse(is.na(nm) | !nzchar(nm), out, nm)
  keep <- !is.na(out) & nzchar(out) & !duplicated(out)
  out[keep]
}

# One source word's choices: a named character vector (value, name its label)
.choice_one <- function(w, ctx, values = NULL) {
  p <- .choice_word_parse(w)
  ref <- function() if (length(p$args)) .choice_ref(ctx, p$args[[1L]])
  x <- switch(
    p$name,
    vars = if (length(p$args)) {
      what <- ref()
      if (!is.null(what)) .choice_ctx(ctx, "vars", what)
    } else .choice_ctx(ctx, "vars"),
    levels = .choice_ctx(ctx, "levels", ref()),
    codelist = .choice_ctx(ctx, "codelist", if (length(p$args)) p$args[[1L]]),
    columns = .choice_ctx(ctx, "columns", ref()),
    populations = , report_data = , study_data = , ard = , study_info = , tokens = ,
    stats = , styles = .choice_ctx(ctx, p$name),
    fixed = values,
    from_args = {
      # (from_args(params): the names of the template's params)
      if (identical(p$args, "params")) {
        ctx$params %||% ctx$arg_names %||% names(ctx$args)
      } else if (length(p$args)) {
        unique(unlist(lapply(ctx$args[p$args], function(v) if (is.atomic(v)) as.character(v))))
      } else {
        ctx$arg_names %||% names(ctx$args)
      }
    },
    stop(sprintf("Unknown choice source \"%s\"; the sources are %s.", p$name,
                 paste(.choice_sources, collapse = ", ")), call. = FALSE))
  .choice_vec(x)
}

# The choices of a pick from its source words (`sources`: tfl_arg_defs()'s
# `choices`), for .field_select(): one source a vector, several a list of
# groups in the words' order (each a list, its name the source's label).  A
# source the ctx cannot answer gives nothing; an unknown word stops.
#
# `ctx`, what the card knows (each a vector, or a function; by name a list):
#   vars       the columns, by data (vars(from): the data `from` names)
#   levels     the values, by variable
#   codelist   the code lists, by name
#   columns    an object's columns, by object
#   populations, report_data, study_data, ard, stats, styles, tokens,
#   study_info the lists themselves
#   args       the card's current arguments (from_args(a), and the data or
#              variable a word names); arg_names: the arguments from_args
#              alone offers (default names(args))
# `values`: the def's fixed values (`fixed`).
.choice_values <- function(sources, ctx = list(), lang = "en", values = NULL) {
  words <- .choice_words(sources)
  if (!length(words)) return(character())
  names(words) <- vapply(words, function(w) .choice_word_parse(w)$name, "")
  bad <- setdiff(names(words), .choice_sources)
  if (length(bad)) {
    stop(sprintf("Unknown choice source \"%s\"; the sources are %s.", bad[[1L]],
                 paste(.choice_sources, collapse = ", ")), call. = FALSE)
  }
  ctx <- ctx %||% list()
  got <- lapply(words, .choice_one, ctx = ctx, values = values)
  got <- got[lengths(got) > 0L]
  if (!length(got)) return(character())
  if (length(got) == 1L) return(got[[1L]])
  labs <- tr(unname(.choice_group_labels[names(got)]), lang)
  # (two groups of one label -- vars(a), vars(b) -- named apart)
  labs <- make.unique(labs, sep = " ")
  stats::setNames(lapply(got, as.list), labs)
}

# ---- a template's params from its defs ---------------------------------------

# A def's type as a template parameter's (tfl_param()'s type)
.tpl_param_type <- function(d) {
  type <- d$type %||% "string"
  # (a table of a map or rows: the table field, R/arg_defs_table.R)
  if (identical(d$input, "table") && !is.null(.tbl_type(type))) return("table")
  if (length(d$values) && identical(d$input, "pick") && !grepl("^list<", type)) return("choice")
  if (grepl("^enum\\[", type)) return("choice")
  if (grepl("^(list<)?column>?$", type)) return("vars")
  if (grepl("^(list<)?level>?$", type)) return("levels")
  # (a pick of names its source words give -- report_data, study_data,
  # populations, ...: chosen from them, tflspec#821)
  if (identical(d$input, "pick") && grepl("^(list<)?string>?$", type) &&
      length(.choice_words(d$choices))) return("pick")
  if (identical(type, "logical")) return("flag")
  if (type %in% c("number", "integer")) return("number")
  if (grepl("^list<", type)) return("list")
  "text"
}

# A template's defs as the parameters its card reads (the fields of
# tflspec's tfl_param(), and the def's own): a default tagged !study is the
# study key it reads, one tagged !fixed a parameter the card does not show
# (`hidden`), a !series or !unset one no default the card can show.
.tpl_params_from_defs <- function(defs) {
  if (!is.data.frame(defs) || !nrow(defs)) return(list())
  out <- lapply(defs$name, function(n) {
    d <- .arg_def(defs, n)
    kind <- d$default_kind
    kind <- if (length(kind) && !is.na(kind)) kind else "none"
    dv <- d[["default"]]
    tagged <- inherits(dv, "tfl_tagged")
    vals <- d$values
    list(name = n,
         default = if (identical(kind, "value")) (if (tagged) dv$value else dv),
         required = isTRUE(d$required),
         doc = if (length(d$description) && !is.na(d$description) && nzchar(d$description)) d$description,
         study = if (identical(kind, "study") && tagged) as.character(dv$value),
         type = .tpl_param_type(d),
         multiple = grepl("^list<", d$type %||% ""),
         choices = if (length(vals)) as.character(unlist(vals)),
         tier = d$tier,
         step = if (identical(d$step, "data")) "data" else "call",
         hidden = identical(kind, "fixed"),
         arg_step = d$step, input = d$input, sources = d$choices,
         free = d$free, ordered = d$ordered, default_kind = if (kind != "none") kind,
         # (the def itself: a table's type, columns and choices)
         def = d,
         # (a table's rows the template writes itself -- forest_hr's ALL row
         # before its conditions: tfl_arg_defs()'s fixed_rows, tflspec#824;
         # shown read only, never written)
         fixed_rows = .tpl_fixed_rows(d))
  })
  stats::setNames(out, defs$name)
}

# A def's fixed rows (a data frame, its attribute "position" "before" or
# "after" the report's rows); NULL for none, or a tflspec that has no such
# column yet
.tpl_fixed_rows <- function(d) {
  x <- d$fixed_rows
  if (is.list(x) && !is.data.frame(x) && length(x) == 1L && is.data.frame(x[[1L]])) x <- x[[1L]]
  if (is.data.frame(x) && nrow(x)) x
}

# the editor section of a def's step
.tpl_step_section <- c(data = "data", analysis = "ard", content = "content", page = "page")
