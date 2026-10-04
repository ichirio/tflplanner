# What went wrong while the study ARD was made: cards does not stop when
# one analysis fails (a test that cannot be run, a statistic whose function
# stops or warns); it keeps the message in the ARD's error / warning
# columns.  tflspec::tfl_ard_conditions() lists them; here, for the study's
# ARD as it was last made.

#' What went wrong while a study's ARD was made
#'
#' The errors and warnings the study ARD (`output/ard/ard.rds`, as last
#' made) keeps, one row each, errors first ([tflspec::tfl_ard_conditions()]).
#'
#' @param study An `rtfstudy`.
#' @return A data frame (`output_id`, `analysis_id`, `variable`, `groups`,
#'   `level`, `message`, `statistics`), no rows when nothing went wrong or
#'   there is no study ARD yet.
#' @export
study_ard_conditions <- function(study) {
  none <- data.frame(output_id = character(), analysis_id = character(),
                     variable = character(), groups = character(),
                     level = character(), message = character(),
                     statistics = character(), stringsAsFactors = FALSE)
  out <- .ard_study_value(study$planner$ard, "output", "output/ard/ard.rds")
  f <- file.path(study$path, out)
  if (!file.exists(f)) return(none)
  a <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(a) || !nrow(a)) return(none)
  d <- tryCatch(tflspec::tfl_ard_conditions(a), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(none)
  for (cn in setdiff(names(none), names(d))) d[[cn]] <- NA_character_
  d <- d[names(none)]
  rownames(d) <- NULL
  d
}
