# A report's argument that holds R code saved as text.  An old build of the
# template card (before 2026-11-10) showed the arguments with deparse(), so
# a chain such as list(tfl_r("n == 0 ~ ..."), "{n}") could be saved back as
# ONE string that starts with `list(structure(`; the report then prints
# that text in its cells (rtfreporter#958).

# TRUE for a single character value that is such text
.is_deparsed_text <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^(list\\()?structure\\(", trimws(x))
}

# The places in `x` (a template's arguments, a content item's arguments: a
# list, any depth) where such a text is: "a.b" style paths
.deparsed_paths <- function(x, path) {
  if (.is_deparsed_text(x)) return(path)
  if (!is.list(x) || inherits(x, "tfl_r")) return(character())
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  nms[is.na(nms) | !nzchar(nms)] <- seq_along(x)[is.na(nms) | !nzchar(nms)]
  unlist(lapply(seq_along(x), function(i)
    .deparsed_paths(x[[i]], paste0(path, ".", nms[[i]]))), use.names = FALSE)
}

# The reports (`outputs`: a named list of tflspec tfl_output) whose template
# arguments or content items' arguments hold such a text, as a data frame
# (output_id, where); no rows when there is none
.deparsed_args <- function(outputs) {
  id <- character()
  where <- character()
  for (o in outputs) {
    tp <- o$template
    w <- if (is.list(tp)) .deparsed_paths(tp$args, "template.args") else character()
    items <- if (is.list(o$content) && !inherits(o$content, "tfl_r")) o$content else list()
    for (it in items) {
      if (!is.list(it)) next
      fn <- as.character(it$fn %||% "item")[1L]
      w <- c(w, .deparsed_paths(it$args, paste0("content.", fn)))
    }
    id <- c(id, rep(as.character(o$id), length(w)))
    where <- c(where, w)
  }
  data.frame(output_id = id, where = where, stringsAsFactors = FALSE)
}

# The one warning for what .deparsed_args() found
.warn_deparsed_args <- function(found) {
  if (!nrow(found)) return(invisible())
  loc <- paste0(found$output_id, ": ", found$where)
  more <- if (length(loc) > 10L) sprintf(" and %d more", length(loc) - 10L) else ""
  warning(
    "Report(s) ", paste(unique(found$output_id), collapse = ", "), ": argument(s) ",
    paste(utils::head(loc, 10L), collapse = ", "), more,
    " hold R code saved as text (`list(structure(...` -- written by tflplanner's ",
    "card before 2026-11-10); their cells print that text. Re-run the AI step for ",
    "these reports, or restore the values from the AI's YAML files ",
    "(spec/outputs/<id>.yml as the AI wrote it).", call. = FALSE)
}
