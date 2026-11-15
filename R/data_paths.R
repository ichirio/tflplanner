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
# itself does not: its preview makes none, and says no error.  A template
# figure with analyses of its own (forest_hr) does.
.report_uses_ard <- function(p, output_id) {
  tryCatch(.is_code_step(p, output_id, "data") || !is.na(.ard_code_of(p, output_id)) ||
             (!is.null(.tpl_of(p, output_id)) && identical(.fig_ard_source(p, output_id)$kind, "own")),
           error = function(e) TRUE)
}

# ---- data.yml against the files (#515) ---------------------------------------

# A path in the way the study writes data.yml: relative to the study when it
# is inside it, else as it is (the same rule as the ADaM folder of step 1)
.data_path_in_study <- function(path, study_path) {
  if (is.na(path) || !nzchar(path)) return(path)
  .ai_adam_in_study(normalizePath(path, "/", mustWork = FALSE), study_path)
}

# The rows of tflspec's `data_paths` (tfl_ai_prepare()) that need a look:
# the dataset's file is not there, or the file given is another file or type.
# A data frame: dataset, in_data_yml, given, problem (the person's words);
# no rows when all is well or `dp` is NULL
.data_paths_problems <- function(dp, t = identity) {
  none <- data.frame(dataset = character(), in_data_yml = character(), given = character(),
                     problem = character(), stringsAsFactors = FALSE)
  if (!is.data.frame(dp) || !nrow(dp)) return(none)
  miss <- !(dp$exists %in% TRUE)
  diff <- dp$differs %in% TRUE
  bad <- miss | diff
  if (!any(bad)) return(none)
  why <- ifelse(miss & diff, t("no file at that path; the file given is another"),
                ifelse(miss, t("no file at that path"), t("the file given is another file or type")))
  data.frame(dataset = as.character(dp$dataset[bad]), in_data_yml = as.character(dp$in_data_yml[bad]),
             given = as.character(dp$given[bad]), problem = why[bad], stringsAsFactors = FALSE)
}

# The planner with the datasets of `dp`'s problem rows pointed at the files
# given (paths relative to the study when inside it); list(planner,
# changed = data.frame(dataset, from, to)).  A dataset with no file given is
# left as it is.
.data_paths_apply <- function(planner, dp, study_path) {
  pr <- .data_paths_problems(dp)
  pr <- pr[!is.na(pr$given) & nzchar(pr$given), , drop = FALSE]
  d <- planner$ard$datasets
  none <- data.frame(dataset = character(), from = character(), to = character(),
                     stringsAsFactors = FALSE)
  if (!nrow(pr) || is.null(d) || !nrow(d) || !"path" %in% names(d)) {
    return(list(planner = planner, changed = none))
  }
  i <- match(toupper(pr$dataset), toupper(as.character(d$dataset)))
  keep <- !is.na(i)
  pr <- pr[keep, , drop = FALSE]
  i <- i[keep]
  to <- vapply(pr$given, .data_path_in_study, "", study_path = study_path, USE.NAMES = FALSE)
  from <- as.character(d$path[i])
  ch <- is.na(from) | to != from
  if (!any(ch)) return(list(planner = planner, changed = none))
  planner$ard$datasets$path[i[ch]] <- to[ch]
  list(planner = planner,
       changed = data.frame(dataset = pr$dataset[ch], from = from[ch], to = to[ch],
                            stringsAsFactors = FALSE))
}

# An ARD preview's error from the child R: when it is a file that could not
# be opened, the datasets of spec/data.yml whose file is not there are named
# ("ADSL: no file at data/adam/adsl.rds (spec/data.yml)."); all paths there:
# the message as it is.  `study`: the study (path, planner)
.data_files_note <- function(why, study) {
  if (is.na(why) || !grepl("cannot open the connection|cannot open file|No such file", why,
                           ignore.case = TRUE)) return(why)
  d <- study$planner$ard$datasets
  if (is.null(d) || !nrow(d) || !"path" %in% names(d) || is.null(study$path)) return(why)
  p <- as.character(d$path)
  full <- ifelse(grepl("^(/|[A-Za-z]:)", p), p, file.path(study$path, p))
  bad <- !is.na(p) & nzchar(p) & !file.exists(full)
  if (!any(bad)) return(why)
  paste0(sub("\\.+$", "", why), ". ",
         paste(sprintf("%s: no file at %s (spec/data.yml).", as.character(d$dataset[bad]), p[bad]),
               collapse = " "))
}
