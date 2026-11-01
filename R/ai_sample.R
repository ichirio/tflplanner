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
  dir <- normalizePath(dir, "/", mustWork = FALSE)
  fn(dir, overwrite = TRUE)
  inp <- function(f) file.path(dir, "inputs", f)
  list(toc = inp("toc.xlsx"), shells = inp("shells.xlsx"), adam = file.path(dir, "adam"),
       adam_spec = inp("adam_spec.xlsx"), docs = inp("sap.docx"))
}
