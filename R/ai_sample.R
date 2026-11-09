# The sample for the AI tab: tflspec::tfl_ai_sample_study() writes its
# materials (a TOC, shells, ADaM, ADaM spec and SAP; no answers), and the
# New study dialog fills the AI tab's step 1 with them (R/srv_study.R).


# Can the New study dialog offer it: the AI tab is on and the installed
# tflspec writes it
.ai_sample_ok <- function() {
  .ai_drafts_on() && "tfl_ai_sample_study" %in% getNamespaceExports("tflspec")
}

# The materials written into `dir` (again, over what is there), and where
# each is: list(toc, shells, adam, adam_spec, docs) -- what step 1 takes
.ai_sample_kit <- function(dir) {
  if (!"tfl_ai_sample_study" %in% getNamespaceExports("tflspec")) {
    stop("The installed tflspec does not write the AI sample: it needs a newer tflspec ",
         "(tfl_ai_sample_study()).", call. = FALSE)
  }
  fn <- getExportedValue("tflspec", "tfl_ai_sample_study")
  fn(dir, overwrite = TRUE)
  # once written: a folder not there yet keeps its temp path unresolved
  # (Windows RUNNER~1, macOS /var), so a second call would name it differently
  dir <- normalizePath(dir, "/", mustWork = TRUE)
  inp <- function(f) file.path(dir, "inputs", f)
  list(toc = inp("toc.xlsx"), shells = inp("shells.xlsx"), adam = file.path(dir, "adam"),
       adam_spec = inp("adam_spec.xlsx"), docs = inp("sap.docx"))
}

# The AI sample as a study of its own (New study > the sample for the AI
# tab): root/<id> made at once with tfl_ai_study_init() over the sample's
# ADaM, copied into its data/adam; the TOC, the shells and the ADaM spec in
# its docs/, the SAP in ai/docs/; registered.  Gives list(study, kit), the
# kit's paths those in the study (the AI tab's step 1 filled with them).
# Nothing is left behind when it fails.
.ai_sample_study <- function(id, kit, root = studies_root(), home = tflplanner_home(), title = NULL) {
  dir <- .study_path(root, id)
  if (dir.exists(dir)) stop(sprintf("A folder %s is already there.", dir), call. = FALSE)
  dir.create(dir, recursive = TRUE)
  ok <- FALSE
  on.exit(if (!ok) unlink(dir, recursive = TRUE), add = TRUE)
  put <- function(f, sub) {
    to <- file.path(dir, sub)
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(file.copy(f, to, overwrite = TRUE))) {
      stop(sprintf("Could not copy %s to %s.", f, to), call. = FALSE)
    }
    normalizePath(file.path(to, basename(f)), "/")
  }
  adam <- study_layout()[["adam"]]
  vapply(list.files(kit$adam, full.names = TRUE), put, "", sub = adam)
  out <- list(toc = put(kit$toc, "docs"),
              shells = vapply(kit$shells, put, "", sub = "docs", USE.NAMES = FALSE),
              adam = normalizePath(file.path(dir, adam), "/", mustWork = FALSE),
              adam_spec = put(kit$adam_spec, "docs"),
              docs = vapply(kit$docs, put, "", sub = "ai/docs", USE.NAMES = FALSE))
  tflspec::tfl_ai_study_init(dir, adam = adam, study_id = id, title = title)
  s <- register_study(dir, home = home)
  ok <- TRUE
  list(study = s, kit = out)
}

#' Add the AI sample study
#'
#' Makes the sample for the AI tab as a study of its own, without the app:
#' the same as the app's *New study* dialog with *The sample for the AI tab*.
#' 'tflspec' writes the sample's materials (a TOC, shells, ADaM data, an ADaM
#' spec and a SAP; no answers) into `samples/ai-sample` under the home; the
#' study `root/<study_id>` is then made over the sample's ADaM
#' ([tflspec::tfl_ai_study_init()]) with the TOC, the shells and the ADaM
#' spec in its `docs/`, the ADaM in `data/adam/` and the SAP in `ai/docs/`,
#' and registered.  Everything in it is fictional.
#'
#' The returned `kit` holds the paths of the materials inside the study:
#' `toc`, `shells`, `adam`, `adam_spec` and `docs`.  These are what
#' [tflspec::tfl_ai_prepare()] takes (`toc`, `shells`, `adam`, `adam_spec`
#' and `docs`), the same fields the AI tab's step 1 is filled with.
#'
#' @param root The studies folder (default: where new studies go, see
#'   [setup_tflplanner()]).
#' @param study_id The new study's ID (default `DMY-001`).
#' @param title The study's title.
#' @param home tflplanner's home.
#' @return `list(study, kit)`, invisibly: the study, and the `kit` of paths
#'   described above.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (a few seconds: it writes the sample's materials and the study)
#' made <- create_ai_sample_study()
#' kit <- made$kit
#' # then, as the AI tab's step 1 does:
#' p <- tflspec::tfl_ai_prepare(made$study$path, toc = kit$toc, adam = kit$adam,
#'                              shells = kit$shells, adam_spec = kit$adam_spec,
#'                              docs = kit$docs, dry_run = TRUE)
#' }
#' options(old)
#' @export
create_ai_sample_study <- function(root = studies_root(home), study_id = "DMY-001",
                                   title = "Example Pharma DMY-001 (fictional)",
                                   home = tflplanner_home()) {
  kit <- .ai_sample_kit(file.path(home, "samples", "ai-sample"))
  made <- .ai_sample_study(study_id, kit, root = root, home = home, title = title)
  invisible(made)
}
