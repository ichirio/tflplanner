# Suggested values for a template report's required variable-set arguments,
# from the study's data (inst/extdata/tpl_suggest.yml, a first draft the user
# edits).  The suggestion for a required argument that has no value and no
# tflspec default; nothing is filled in at run time.

# The candidate list used by default (a name in the table's `max:`/`stf:`
# lists): the one place to switch it.
.tpl_suggest_list <- "max"

.tpl_suggest_table <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      f <- system.file("extdata", "tpl_suggest.yml", package = "tflplanner")
      cache <<- if (nzchar(f)) tryCatch(yaml::read_yaml(f), error = function(e) list()) else list()
    }
    cache
  }
})

# The candidates present in `data_cols`, in candidate order; for a
# single-value argument the first present.  character(): none, or the
# template / argument / list is not in the table.
.tpl_suggest <- function(name, arg, data_cols, list = .tpl_suggest_list, study = NULL) {
  # (`by`, of any template: the column variable, the study's settings.treatment
  # of the study spec; none if it has none.  Not in the yml)
  if (identical(arg, "by")) {
    tr <- as.character(study$study$settings$treatment %||% character())
    return(if (.tpl_blank(tr)) character() else tr[1L])
  }
  tp <- .tpl_suggest_table()[[name]]
  if (!is.list(tp[[arg]])) return(character())
  cand <- as.character(unlist(tp[[arg]][[list]]))
  if (!length(cand)) return(character())
  out <- cand[cand %in% as.character(data_cols)]
  if (arg %in% as.character(unlist(tp$single))) utils::head(out, 1L) else out
}

# Empty: NULL, no element, or only blank text
.tpl_blank <- function(x) length(x) == 0L || !any(nzchar(trimws(as.character(unlist(x)))))

# The suggestion for a template's argument: the columns of the dataset the
# report reads (its `data` argument, the template's default, else the table's
# `data:`), taken as the add-report dialog takes them (.tpl_columns).
.tpl_suggest_for <- function(name, arg, data, args = list(), prm = .tpl_params(name), study = NULL) {
  if (!identical(arg, "by") && is.null(.tpl_suggest_table()[[name]][[arg]])) return(character())
  dataset <- args$data %||% prm$data$default %||% .tpl_suggest_table()[[name]]$data
  .tpl_suggest(name, arg, .tpl_columns(data, dataset), study = study)
}
