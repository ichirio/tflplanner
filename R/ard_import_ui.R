# The ARD tab's "ARDs taken in": the record of ard_imports() with the
# reports that use each one, and the replacing of one ARD taken in by
# another (the back end's functions, put together; nothing is forgotten).

# The reports whose report row names an ARD taken in (`ard_source =
# import:<file>`)
.imports_used_by <- function(report, file) {
  if (is.null(report) || !nrow(report) || !"ard_source" %in% names(report)) {
    return(character())
  }
  k <- !is.na(report$output_id) & !is.na(report$ard_source) &
    report$ard_source == paste0("import:", file)
  unique(report$output_id[k])
}

# The record as the screen lists it: the newest first, with the reports
# that use each ARD
.imports_view <- function(imports, report) {
  if (!nrow(imports)) return(cbind(imports, used_by = character()))
  imports$used_by <- vapply(imports$file, function(f)
    paste(.imports_used_by(report, f), collapse = ", "), "")
  imports[rev(seq_len(nrow(imports))), , drop = FALSE]
}

# Replace an ARD taken in by another file: take the new one in, point the
# reports that used the old one at it, and mark the old one removed (its
# file and its row stay on the record).  Returns list(planner, row) -- the
# planner with the reports pointed at the new file, the new record row
# (with the check's problems as attribute `check`).
replace_imported_ard <- function(study, old_import_id, path, source = NA_character_,
                                 name = NULL, output_id = NULL) {
  log <- ard_imports(study)
  i <- match(old_import_id, log$import_id)
  if (is.na(i)) stop("No import ", old_import_id, call. = FALSE)
  old_file <- log$file[i]
  outs <- output_id %||% {
    o <- trimws(strsplit(log$outputs[i] %||% "", "|", fixed = TRUE)[[1L]])
    o[nzchar(o)]
  }
  row <- import_ard(study, path, output_id = if (length(outs)) outs,
                    source = source, name = name)
  x <- study$planner
  for (id in .imports_used_by(x$sheets$report, old_file)) {
    x <- use_imported_ard(x, id, row$file)
  }
  remove_imported_ard(study, old_import_id)
  list(planner = x, row = row)
}
