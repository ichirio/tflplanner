# Template reports (demo, _archive/template-spec-input-design-2026-10-20.md):
# a report whose YAML file says `template: {name, args}` -- one of
# tflspec's templates (tflspec::tfl_templates()) and the values of its
# parameters (tflspec::tfl_template_params()).  The view (the sheets) has
# the template's items, as tflspec::tfl_sheets_from_spec() expands them, so
# the ARD, the preview and the program are made as for any report; the
# report's file keeps the template and its arguments only.

# one line on each template (tflspec's templates; the app translates it)
.tpl_desc <- c(
  ae_overview = "Overview of adverse events",
  ae_soc_pt = "Adverse events by SOC and PT",
  demog = "Demographics",
  disposition = "Disposition",
  hierarchical = "A hierarchy's counts",
  km_simple = "Kaplan-Meier curves",
  km_table = "Kaplan-Meier estimates",
  lab_shift = "Shift table of a lab parameter",
  listing = "A listing",
  orr = "Objective response",
  shift = "Shift table",
  summary = "Continuous variables by a column key",
  vs_change = "A vital sign's change")

# The templates of a kind of report: name and kind
.tpl_list <- function(kind) {
  x <- tryCatch(tflspec::tfl_templates(), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(data.frame(name = character(), kind = character()))
  x <- x[x$kind %in% kind, c("name", "kind"), drop = FALSE]
  rownames(x) <- NULL
  x
}

# A template's parameters, named, in its order (none: a template unknown)
.tpl_params <- function(name) {
  p <- tryCatch(tflspec::tfl_template_params(name), error = function(e) list())
  if (length(p) && is.null(names(p))) {
    names(p) <- vapply(p, function(x) x$name, "")
  }
  for (a in names(p)) {
    if (!nzchar(p[[a]]$doc %||% "") && !is.na(.tpl_doc_fill[a])) p[[a]]$doc <- .tpl_doc_fill[[a]]
  }
  p
}

# A report's template, as its file says it (NULL: none)
.tpl_of <- function(p, output_id) {
  o <- p$spec$outputs[[output_id]]
  if (is.null(o) || is.null(o$template$name)) return(NULL)
  list(name = as.character(o$template$name), args = o$template$args %||% list())
}

# The study's datasets: their columns, by dataset (upper case), read once
# a study folder
.tpl_cols_cache <- new.env()
.tpl_data_cols <- function(root) {
  if (is.null(root)) return(list())
  key <- normalizePath(root, "/", mustWork = FALSE)
  if (!is.null(.tpl_cols_cache[[key]])) return(.tpl_cols_cache[[key]])
  files <- list.files(file.path(root, "data"), "\\.(rds|RDS)$", recursive = TRUE, full.names = TRUE)
  out <- list()
  for (f in files) {
    d <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.data.frame(d)) next
    out[[toupper(sub("\\.[^.]*$", "", basename(f)))]] <- d
  }
  .tpl_cols_cache[[key]] <- out
  out
}

# The columns a parameter chooses from: the dataset's the report reads
# (its `data` argument or default), else every dataset's
.tpl_columns <- function(data, dataset = NULL) {
  ds <- toupper(as.character(dataset %||% "")[1L])
  if (!is.na(ds) && nzchar(ds) && !is.null(data[[ds]])) return(names(data[[ds]]))
  unique(unlist(lapply(data, names)))
}

.tpl_is_varlike <- function(x) {
  is.character(x) && length(x) > 0L && is.null(names(x)) &&
    all(grepl("^[A-Z][A-Z0-9_]*$", x))
}

# The input a parameter gets, guessed from its default and its doc (until
# tfl_param(type = ) says it): population, data, param, flag, number,
# vars (columns, several), var (one column), chips (values), text, list
# (read only)
.tpl_type <- function(prm, columns = character()) {
  d <- prm$default
  n <- prm$name
  doc <- prm$doc %||% ""
  if (identical(n, "population")) return("population")
  if (identical(n, "data")) return("data")
  if (is.logical(d) && length(d) == 1L) return("flag")
  if (is.numeric(d) && length(d) == 1L && is.null(names(d))) return("number")
  if (identical(n, "param") && grepl("PARAMCD", doc)) return("param")
  several <- n %in% c("continuous", "categorical", "hierarchy", "variables", "params")
  if (.tpl_is_varlike(d) && (!length(columns) || all(d %in% columns))) {
    return(if (length(d) > 1L || several) "vars" else "var")
  }
  if (is.null(d)) {
    if (grepl("variable|flag", doc, ignore.case = TRUE)) return("var")
    return("text")
  }
  if (is.character(d) && is.null(names(d))) {
    return(if (length(d) == 1L) "text" else "chips")
  }
  "list"
}

# A value as one line to read (a default; a list's value)
.tpl_show <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.character(x) && length(x) == 1L && is.null(names(x))) return(x)
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
}

# What an input gives, as the argument's value (NULL: none -- the default)
.tpl_value <- function(v, type) {
  if (is.null(v)) return(NULL)
  switch(type,
    flag = isTRUE(v),
    number = {
      v <- suppressWarnings(as.numeric(v))
      if (!length(v) || is.na(v)) NULL else v
    },
    vars = , chips = {
      v <- as.character(v)
      v <- v[nzchar(trimws(v))]
      if (length(v)) v else NULL
    },
    {
      v <- trimws(paste(as.character(v), collapse = ""))
      if (nzchar(v)) v else NULL
    })
}

# The args with one argument set (NULL, or the default itself: taken away)
.tpl_set_arg <- function(args, prm, value) {
  args <- args %||% list()
  if (is.null(value) || (!isTRUE(prm$required) && identical(value, prm$default)) ||
      (!isTRUE(prm$required) && !is.null(prm$default) &&
       identical(unname(as.vector(value)), unname(as.vector(prm$default))))) {
    args[[prm$name]] <- NULL
  } else {
    args[[prm$name]] <- value
  }
  # (in the template's order)
  args
}

# The required arguments still empty
.tpl_missing <- function(name, args) {
  prm <- .tpl_params(name)
  req <- names(prm)[vapply(prm, function(x) isTRUE(x$required), NA)]
  req[!vapply(req, function(a) length(args[[a]]) > 0L && any(nzchar(as.character(args[[a]]))), NA)]
}

# A report's file with its template's arguments set
.tpl_output <- function(p, output_id, args) {
  o <- p$spec$outputs[[output_id]]
  prm <- .tpl_params(o$template$name)
  args <- args[c(intersect(names(prm), names(args)), setdiff(names(args), names(prm)))]
  o$template$args <- args
  if (!is.null(args$population)) o$population <- as.character(args$population)
  o
}

# a meaning for the parameters whose template says none (their doc empty)
.tpl_doc_fill <- c(
  hierarchy = "the hierarchy's variables (SOC, PT)",
  where = "the records' condition",
  data_label = "the analysis data's label",
  analysis_id = "the analysis' id",
  analysis_label = "the analysis' label",
  overall = "the any-event row's label")
