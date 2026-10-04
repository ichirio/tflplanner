# What went wrong inside the analyses while an ARD was made: cards does not
# stop when one analysis fails (a test that cannot be run, a statistic
# whose function stops or warns); it keeps the message in the ARD's error /
# warning columns.  tflspec::tfl_ard_conditions() lists them; here, for the
# study ARD as it was last made and for the ARDs reports take in.  (A
# program that stopped as a whole is the study ARD's state, not here.)

.no_conditions <- function() {
  data.frame(output_id = character(), analysis_id = character(),
             variable = character(), groups = character(),
             level = character(), message = character(),
             statistics = character(), source = character(),
             stringsAsFactors = FALSE)
}

# One ARD's conditions in the shape of .no_conditions(), the report's id
# filled in when the ARD has none (an ARD taken in may not say it)
.ard_conditions_of <- function(a, output_id = NA_character_, source = "") {
  none <- .no_conditions()
  if (is.null(a) || !nrow(a)) return(none)
  d <- tryCatch(tflspec::tfl_ard_conditions(a), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(none)
  for (cn in setdiff(names(none), names(d))) d[[cn]] <- NA_character_
  if (!is.na(output_id)) d$output_id[is.na(d$output_id)] <- output_id
  d$source <- source
  d <- d[names(none)]
  rownames(d) <- NULL
  d
}

#' What went wrong inside the analyses while a study's ARD was made
#'
#' The errors and warnings the study ARD (`output/ard/ard.rds`, as last
#' made) keeps, and those of the ARDs reports take in (`input/ard/`), one
#' row each, errors first ([tflspec::tfl_ard_conditions()]).  An analysis
#' with an error has no statistics in the ARD: its cells are blank in the
#' table.
#'
#' @param study An `rtfstudy`.
#' @return A data frame (`output_id`, `analysis_id`, `variable`, `groups`,
#'   `level`, `message`, `statistics`, `source`: `""` for the study ARD,
#'   the file's name for an ARD taken in); no rows when nothing went wrong
#'   or there is no ARD yet.
#' @export
study_ard_conditions <- function(study) {
  out <- .ard_study_value(study$planner$ard, "output", "output/ard/ard.rds")
  f <- file.path(study$path, out)
  own <- if (file.exists(f)) {
    a <- tryCatch(readRDS(f), error = function(e) NULL)
    .ard_conditions_of(a)
  } else .no_conditions()
  # the reports that take in an ARD: their rows come from that file
  ids <- study$planner$outputs$output_id
  taken <- Filter(Negate(is.null), stats::setNames(
    lapply(ids, function(id) .ard_import_of(study$planner, id)), ids))
  own <- own[!own$output_id %in% names(taken), , drop = FALSE]
  more <- lapply(names(taken), function(id) {
    p <- file.path(.import_dir(study), taken[[id]])
    a <- if (file.exists(p)) tryCatch(tflspec::tfl_read_ard(p), error = function(e) NULL)
    if (!is.null(a) && "output_id" %in% names(a) && any(a$output_id %in% id)) {
      a <- a[a$output_id %in% id, , drop = FALSE]
    }
    .ard_conditions_of(a, id, taken[[id]])
  })
  d <- do.call(rbind, c(list(own), more))
  d <- d[order(d$level != "error"), , drop = FALSE]
  rownames(d) <- NULL
  d
}
