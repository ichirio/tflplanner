# The study's datasets as delivered (#515): the data's file type is never
# assumed.  A dataset whose file is not there, while the same dataset is in
# its folder in another form, is pointed at that file -- .sas7bdat first,
# then .xpt, .rds, .parquet.

.data_ext_order <- c("sas7bdat", "xpt", "rds", "parquet")

# A dataset path as delivered: `path` itself when its file is there, else the
# same name with the first extension of .data_ext_order whose file is there,
# else `path` (relative paths are the study folder's)
.dataset_delivered <- function(study_path, path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) return(path)
  at <- function(p) if (grepl("^(/|[A-Za-z]:)", p)) p else file.path(study_path, p)
  if (file.exists(at(path))) return(path)
  base <- tools::file_path_sans_ext(path)
  for (e in .data_ext_order) {
    cand <- paste0(base, ".", e)
    if (file.exists(at(cand))) return(cand)
  }
  path
}

# The planner with its datasets' paths as delivered, and what changed:
# list(planner, changed = data.frame(dataset, from, to))
.datasets_as_delivered <- function(planner, study_path) {
  d <- planner$ard$datasets
  none <- data.frame(dataset = character(), from = character(), to = character(),
                     stringsAsFactors = FALSE)
  if (is.null(d) || !nrow(d) || !"path" %in% names(d) || is.null(study_path)) {
    return(list(planner = planner, changed = none))
  }
  to <- vapply(as.character(d$path), function(p) .dataset_delivered(study_path, p), "",
               USE.NAMES = FALSE)
  ch <- !is.na(to) & !is.na(d$path) & to != d$path
  if (!any(ch)) return(list(planner = planner, changed = none))
  changed <- data.frame(dataset = as.character(d$dataset[ch]), from = as.character(d$path[ch]),
                        to = to[ch], stringsAsFactors = FALSE)
  planner$ard$datasets$path[ch] <- to[ch]
  list(planner = planner, changed = changed)
}

# Does a report make an ARD -- a data step of its own, or analyses / ARD
# code (as fetch_ard() asks)?  A figure or a listing that reads its data
# itself does not: its preview makes none, and says no error
.report_uses_ard <- function(p, output_id) {
  tryCatch(.is_code_step(p, output_id, "data") || !is.na(.ard_code_of(p, output_id)),
           error = function(e) TRUE)
}
