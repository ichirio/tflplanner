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

# ---- the add-report dialog's field -----------------------------------------
# A field with no value and no tflspec default shows the suggestion as its grey
# placeholder and at the top of its choices; nothing is filled in.  Only a value
# the person picks counts.  One selectize field for every type that can take a
# suggestion (vars, var, chips, levels): a single pick lists the suggested values
# as a group "Suggested"; a multi-pick has one item "Add all suggested (...)" there,
# which selectize's onItemAdd swaps for the suggested values, in order.

# The value of the "Add all suggested" item
.tpl_suggest_all <- "__tpl_suggest_all__"

# The suggestion as one line: the first few, then "..."
.tpl_suggest_line <- function(sugg, n = 5L) {
  paste0(paste(utils::head(sugg, n), collapse = ", "), if (length(sugg) > n) ", ...")
}

# A picked "Add all suggested" item taken for the suggested values, in its place
.tpl_suggest_expand <- function(v, sugg) {
  if (is.null(v) || !.tpl_suggest_all %in% v) return(v)
  at <- match(.tpl_suggest_all, v)
  unique(c(v[seq_len(at - 1L)], sugg, v[-seq_len(at)]))
}

# The selectize field of an argument that has a suggestion.  `choices`: the
# plain ones (an unnamed or named character vector; a blank "" first stays
# first).  `several`: a multi-pick.  `options`: the field's own selectize options.
.tpl_suggest_select <- function(input_id, lab, choices, sugg, several, options, t) {
  nm <- names(choices)
  if (is.null(nm)) nm <- choices
  choices <- stats::setNames(as.character(choices), nm)
  top <- if (several) {
    stats::setNames(.tpl_suggest_all, sprintf(t("Add all suggested (%s)"), .tpl_suggest_line(sugg)))
  } else stats::setNames(sugg, sugg)
  blank <- choices[choices == ""]
  rest <- choices[choices != "" & !(!several & choices %in% sugg)]
  all_choices <- c(as.list(blank), stats::setNames(list(as.list(top)), t("Suggested")), as.list(rest))
  options$placeholder <- sprintf(t("Suggested: %s"), .tpl_suggest_line(sugg))
  if (several) {
    options$onItemAdd <- I(sprintf(
      paste0("function(value, $item) { if (value !== '%s') return; var all = %s;",
             " this.removeItem(value); for (var i = 0; i < all.length; i++) this.addItem(all[i]); }"),
      .tpl_suggest_all, as.character(jsonlite::toJSON(as.character(sugg)))))
  }
  shiny::selectizeInput(input_id, lab, all_choices, selected = if (several) character() else "",
                        multiple = several, width = "100%", options = options)
}
