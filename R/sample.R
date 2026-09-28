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
#' Copies tflplanner's sample study into a studies folder and registers it:
#' the CDISC pilot ADaM data of the 'pharmaverseadam' package (ADSL, ADAE,
#' ADVS, and ADTTE derived from them), the ARD definition, five tables made
#' from one study ARD, a listing and two figures -- every step, from the ARD
#' to the reports, defined in the app and written as Excel (`spec/`: the
#' table and report definitions, the listing / figure definition and
#' `ard_spec.xlsx`, an export of the ARD definition to read).  With
#' `run = TRUE` it then makes them -- the ARD programs and the report
#' programs, as an official run with its batch folder (see [run_batch()])
#' -- so the study opens with its ARD and RTFs in place.
#' [setup_tflplanner()]`(sample = TRUE)` does this at setup, and the app's
#' *New study* dialog under a study ID of your own.
#'
#' @param root The studies folder (default: where new studies go, see
#'   [setup_tflplanner()]).
#' @param run Make the ARD and the reports (needs 'cards' and 'cardx').
#' @param study_id The new study's ID (default `SAMPLE-01`).
#' @param title,compound,phase,description The study's fields; `NULL`
#'   keeps the sample's.
#' @param home tflplanner's home.
#' @return The study, invisibly.
#' @examples
#' \dontrun{
#' create_sample_study()
#' }
#' @export
create_sample_study <- function(root = studies_root(home), run = TRUE,
                                study_id = .sample_id, title = NULL,
                                compound = NULL, phase = NULL,
                                description = NULL,
                                home = tflplanner_home()) {
  src <- system.file("sample", .sample_id, package = "tflplanner")
  if (!nzchar(src)) stop("The sample study is not installed.", call. = FALSE)
  study_id <- .check_study_id(study_id)
  if (!is.null(.read_state(study_id, home))) {
    stop("Study '", study_id, "' is already registered.", call. = FALSE)
  }
  path <- file.path(root, study_id)
  if (file.exists(path)) {
    stop("A folder '", study_id, "' is already there: ", path,
         call. = FALSE)
  }
  dir.create(path, recursive = TRUE)
  for (f in list.files(src, recursive = TRUE, all.files = TRUE)) {
    to <- if (identical(f, paste0(.sample_id, ".Rproj"))) {
      paste0(study_id, ".Rproj")
    } else f
    dir.create(file.path(path, dirname(to)), recursive = TRUE,
               showWarnings = FALSE)
    file.copy(file.path(src, f), file.path(path, to))
  }
  for (d in study_layout()) {
    dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
  }
  rproj <- file.path(path, paste0(study_id, ".Rproj"))
  if (!file.exists(rproj)) writeLines(.rproj, rproj)
  # the study's own ID and fields
  meta <- .read_meta(path)
  meta$study_id <- study_id
  given <- list(title = title, compound = compound, phase = phase,
                description = description)
  for (k in names(given)) if (!is.null(given[[k]])) meta[[k]] <- given[[k]]
  meta$created <- format(Sys.Date())
  .write_meta(meta, path)
  s <- register_study(path, home)
  s <- save_study(s, home = home)
  # every definition as Excel, to read: the ARD definition's export too
  .write_ard_spec(s$planner$ard,
                  file.path(path, study_layout()[["spec"]], .ard_file))
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
