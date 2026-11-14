# The card model of a template report's step (the DM template screen's
# base): what the step's card shows, one row per argument of the template
# in that step, and the write-back of the screen's values into the report's
# `template: args:`.  Pure R, no UI.
#
# A row: list(name, type, label, help, value, default, from, inherited,
# required, key, locked, group).
#   type       the field's type notation (.tpl_field_type())
#   label      the meaning, an i18n key ("Rows per page"); the screen writes
#              it as "meaning (name)".  The argument's name when none.
#   help       tflspec's doc of the argument ("" when none)
#   value      the report's own value; NULL: not written
#   default    the template's default
#   from       where the value used when not set comes from: "template",
#              "config" (the page settings) or "study" (the study settings)
#   inherited  that value when from is "config" or "study", else NULL
#   required   the template requires it
#   key        tier "key": stays on top
#   locked     the template fixes its value (fixed_value)
#   group      "top" (required, key or set) or "bottom" (the collapsed
#              "left at default (n)")
# `population` is never a row (the report's common `population:` field);
# a hidden argument (the template takes it from the study standard) is not
# one either.

.card_steps <- c("data", "ard", "content", "page")

# The template's arguments a card may show, in definition order: no
# population, no hidden one
.card_params <- function(template) {
  prm <- tflspec::tfl_template_params(template)
  Filter(function(p) !identical(p$name, "population") && !isTRUE(p$hidden), prm)
}

# The names of a step's arguments, in definition order
.card_step_names <- function(template, step, prm = .card_params(template)) {
  step <- match.arg(step, .card_steps)
  nm <- names(prm)
  nm[vapply(nm, function(n) identical(.tpl_arg_section(template, n), step), NA)]
}

# A study reference's value in the study settings: "settings.<name>" is
# study$<name> (or study$settings$<name>), "<verb>.<argument>"
# study$<verb>$<argument>; the first key that has one
.card_study_value <- function(study, prm) {
  if (!length(study)) return(NULL)
  for (k in as.character(prm$study %||% character())) {
    if (is.na(k) || !grepl(".", k, fixed = TRUE)) next
    root <- sub("[.].*$", "", k)
    arg <- sub("^[^.]*[.]", "", k)
    v <- if (identical(root, "settings")) study[[arg]] %||% study$settings[[arg]] else
      if (is.list(study[[root]])) study[[root]][[arg]]
    if (!is.null(v)) return(v)
  }
  NULL
}

# The meaning of an argument (.tpl_arg_labels, .tpl_doc_labels), else its name
.card_label <- function(prm) {
  doc <- as.character(prm$doc %||% "")[1L]
  if (!is.na(doc) && doc %in% names(.tpl_doc_labels)) return(.tpl_doc_labels[[doc]])
  lab <- .tpl_arg_labels[prm$name]
  if (is.na(lab)) prm$name else unname(lab)
}

.card_row <- function(prm, args, ctx) {
  n <- prm$name
  value <- args[[n]]
  cfg <- ctx$config[[n]]
  sv <- if (is.null(cfg)) .card_study_value(ctx$study, prm)
  from <- if (!is.null(cfg)) "config" else if (!is.null(sv)) "study" else "template"
  key <- identical(prm$tier, "key")
  required <- isTRUE(prm$required)
  doc <- as.character(prm$doc %||% "")[1L]
  list(name = n,
       type = .tpl_field_type(prm)$type,
       label = .card_label(prm),
       help = if (is.na(doc)) "" else doc,
       value = value,
       default = prm$default,
       from = from,
       inherited = cfg %||% sv,
       required = required,
       key = key,
       locked = isTRUE(prm$fixed_value),
       group = if (required || key || !is.null(value)) "top" else "bottom")
}

#' @noRd
.card_model <- function(template, args, step, ctx = list()) {
  step <- match.arg(step, .card_steps)
  args <- args %||% list()
  prm <- .card_params(template)
  rows <- lapply(.card_step_names(template, step, prm), function(n) .card_row(prm[[n]], args, ctx))
  all_prm <- names(tflspec::tfl_template_params(template))
  out <- list(template = template, step = step, rows = rows)
  attr(out, "unknown") <- as.character(setdiff(names(args), all_prm))
  out
}

# The report's args with one step's values from the screen: `values` the
# step's arguments by name (NULL: back to the default, the name dropped);
# the other steps' names kept; template arguments in definition order,
# then the names that are no template's argument, as they were.
#' @noRd
.card_args_write <- function(args, step, template, values) {
  step <- match.arg(step, .card_steps)
  args <- args %||% list()
  mine <- .card_step_names(template, step)
  bad <- setdiff(names(values), mine)
  if (length(bad)) {
    stop(sprintf("Not an argument of the %s step of %s: %s", step, template,
                 paste(bad, collapse = ", ")), call. = FALSE)
  }
  for (n in names(values)) {
    v <- values[[n]]
    if (is.null(v)) args[n] <- NULL else args[[n]] <- v
  }
  defs <- names(tflspec::tfl_template_params(template))
  ord <- c(intersect(defs, names(args)), setdiff(names(args), defs))
  args[ord]
}
