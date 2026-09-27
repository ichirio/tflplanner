# The sample study that ships with tflplanner.
#
# inst/sample/SAMPLE-01 holds what a study folder needs to be registered
# (study.yml, spec/, data/), made by data-raw/make-sample-study.R from the
# CDISC pilot ADaM data of the pharmaverseadam package.  It is copied into
# the studies folder a user sets up -- never into a developer's test
# studies -- and made there: its programs, its study ARD, its reports.

.sample_id <- "SAMPLE-01"

#' Add the sample study
#'
#' Copies tflplanner's sample study, `SAMPLE-01`, into a studies folder and
#' registers it: the CDISC pilot ADaM data of the 'pharmaverseadam' package
#' (ADSL, ADAE, ADVS), one study ARD and four tables, a listing and a
#' figure made from it.  With `run = TRUE` it then makes them -- the ARD
#' programs and the report programs, as an official run with its batch
#' folder (see [run_batch()]) -- so the study opens with its ARD and RTFs
#' in place.  [setup_tflplanner()]`(sample = TRUE)` does this at setup.
#'
#' @param root The studies folder (default: where new studies go, see
#'   [setup_tflplanner()]).
#' @param run Make the ARD and the reports (needs 'cards' and 'cardx').
#' @param home tflplanner's home.
#' @return The study, invisibly.
#' @examples
#' \dontrun{
#' create_sample_study()
#' }
#' @export
create_sample_study <- function(root = studies_root(home), run = TRUE,
                                home = tflplanner_home()) {
  src <- system.file("sample", .sample_id, package = "tflplanner")
  if (!nzchar(src)) stop("The sample study is not installed.", call. = FALSE)
  if (!is.null(.read_state(.sample_id, home))) {
    stop("The sample study ", .sample_id, " is already registered.",
         call. = FALSE)
  }
  path <- file.path(root, .sample_id)
  if (file.exists(path)) {
    stop("A folder '", .sample_id, "' is already there: ", path,
         call. = FALSE)
  }
  dir.create(path, recursive = TRUE)
  for (f in list.files(src, recursive = TRUE, all.files = TRUE)) {
    dir.create(file.path(path, dirname(f)), recursive = TRUE,
               showWarnings = FALSE)
    file.copy(file.path(src, f), file.path(path, f))
  }
  for (d in study_layout()) {
    dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
  }
  s <- register_study(path, home)
  s <- save_study(s, home = home)
  if (run) {
    miss <- c("cards", "cardx")[!vapply(c("cards", "cardx"),
                                        requireNamespace, NA,
                                        quietly = TRUE)]
    if (length(miss)) {
      message("The sample study is in place; to make its ARD and reports, ",
              "install ", paste(miss, collapse = " and "),
              " and run it from the app (Results: official run).")
    } else {
      message("Making the sample study's ARD and reports (about a minute) ...")
      b <- run_batch(s, c("ard", "tfl"))
      message(if (b$ok) "Done: " else "Done, with errors: ",
              sum(b$result$status == "OK"), " of ", nrow(b$result),
              " programs ran without error (", b$batch, ").")
    }
  }
  message("The sample study: ", path)
  invisible(s)
}
