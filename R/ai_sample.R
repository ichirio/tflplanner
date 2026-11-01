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
              adam = normalizePath(file.path(dir, adam), "/"),
              adam_spec = put(kit$adam_spec, "docs"),
              docs = vapply(kit$docs, put, "", sub = "ai/docs", USE.NAMES = FALSE))
  tflspec::tfl_ai_study_init(dir, adam = adam, study_id = id, title = title)
  s <- register_study(dir, home = home)
  ok <- TRUE
  list(study = s, kit = out)
}
