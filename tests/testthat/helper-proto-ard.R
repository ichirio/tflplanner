# PROTOTYPE (tflspec#1032): a report's ARD made by its ARD program
# (programs/ard/<id>.R) in its own R, then the working study ARD collected
# from the reports' files -- what update_study_ard() did, which the
# prototype has no more (a preview, run_study(), makes it with the report).
proto_make_ard <- function(study, id) {
  prog <- file.path("programs", "ard", .ard_prog_name(id))
  px <- processx::run(file.path(R.home("bin"), "Rscript"),
                      c("-e", sprintf("source(%s)", encodeString(prog, quote = "\"")),
                        "-e", "source('programs/onepass_helpers.R'); invisible(collect_ard())"),
                      wd = study$path, error_on_status = FALSE, stderr_to_stdout = TRUE)
  st <- ard_status(study)
  out <- strsplit(px$stdout, "\r?\n")[[1L]]
  list(ok = identical(px$status, 0L), rows = st$rows[match(id, st$output_id)],
       error = if (!identical(px$status, 0L)) paste(utils::tail(out, 15L), collapse = "\n"),
       output = out)
}

# the working ARD's status file, as collect_ard() writes it (a test's own
# rows)
proto_write_ard_status <- function(study, d) {
  f <- file.path(study$path, "output", "ard", "ard_status.csv")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(d, f, row.names = FALSE)
}

# a report's rows of the study ARD file (output/ard/ard.rds) as a whole-study
# run_ard() wrote it (what study_ard_rows() read before the prototype)
proto_study_rows <- function(study, id) {
  f <- file.path(study$path, "output", "ard", "ard.rds")
  if (!file.exists(f)) return(NULL)
  a <- readRDS(f)
  a[a$output_id %in% id, , drop = FALSE]
}
