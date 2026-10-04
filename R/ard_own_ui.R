# ============================================================================
#  ARD functions of one's own, for the app's "Own functions" tab
# ----------------------------------------------------------------------------
#  A study's own ard_*() functions are R files its ARD programs load (the
#  ARD definition's study key `source`; programs/ard/functions/ by
#  convention); the company keeps its own in the standards folder
#  (company_ard_functions()).  Here: both listed side by side (a study's
#  copy wins), where each is used, tried on the study's data in a separate
#  R process (the last try kept in programs/ard/functions/.checks.json),
#  and new ones started from tflspec's templates.  Files are read, never
#  run, to be listed (tflspec::tfl_ard_function_info()); they are edited
#  outside the app.
# ============================================================================

.fun_checks_file <- function(study) {
  file.path(study$path, .study_fun_dir, ".checks.json")
}

# the R files a study's ARD programs load (`source`, in that order), and
# those in programs/ard/functions/ not loaded (yet); test files left out
.study_fun_files <- function(study) {
  st <- study$planner$ard$study
  k <- match("source", st$key)
  src <- if (is.na(k)) character() else .split_bar(st$value[k])
  src <- src[grepl("[.][Rr]$", src)]
  dir <- file.path(study$path, .study_fun_dir)
  more <- list.files(dir, pattern = "[.][Rr]$")
  more <- file.path(.study_fun_dir, more[!grepl("^test-", more)])
  data.frame(file = c(src, setdiff(more, src)),
             loaded = c(rep(TRUE, length(src)), rep(FALSE, length(setdiff(more, src)))),
             stringsAsFactors = FALSE)
}

#' A study's own ARD functions
#'
#' The functions defined in the R files the study's ARD programs load (its
#' ARD definition's study key `source`) and in its `programs/ard/functions/`
#' (loaded or not), read -- not run -- with
#' [tflspec::tfl_ard_function_info()].
#'
#' @param study An `rtfstudy`.
#' @return A data frame, one row a function: `name`, `file` (relative to
#'   the study folder), `loaded`, `title`, `description`, `stat_names`.
#' @export
study_ard_functions <- function(study) {
  fs <- .study_fun_files(study)
  none <- data.frame(name = character(), file = character(), loaded = logical(),
                     title = character(), description = character(),
                     stat_names = character(), stringsAsFactors = FALSE)
  have <- file.exists(file.path(study$path, fs$file))
  fs <- fs[have, , drop = FALSE]
  if (!nrow(fs)) return(none)
  info <- tflspec::tfl_ard_function_info(file.path(study$path, fs$file))
  info <- info[!is.na(info$name), , drop = FALSE]
  if (!nrow(info)) return(none)
  rel <- fs$file[match(normalizePath(info$file, "/", FALSE),
                       normalizePath(file.path(study$path, fs$file), "/", FALSE))]
  out <- data.frame(name = info$name, file = rel,
                    loaded = fs$loaded[match(rel, fs$file)],
                    title = info$title, description = info$description,
                    stat_names = info$stat_names, stringsAsFactors = FALSE)
  out <- out[!duplicated(out$name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The ARD functions of one's own a study can use
#'
#' The study's ([study_ard_functions()]) and the company's
#' ([company_ard_functions()]) side by side: one row a function name.  A
#' study's copy wins; when both have one, whether their files differ and
#' which is newer.  Where the study's analyses use it, and the last time it
#' was tried ([try_ard_function()]).
#'
#' @param study An `rtfstudy`.
#' @param home The tflplanner home.
#' @return A data frame: `name`, `where` (`"study"`, `"company"`,
#'   `"both"`), `study_file`, `company_file`, `loaded`, `differs`, `newer`
#'   (`"study"`, `"company"` or `NA`), `used_by` (`"T1 / RD | ..."`),
#'   `title`, `description`, `stat_names`, `tried` (when), `problems`
#'   (errors and warnings found), `stale` (the file changed since).
#' @export
own_ard_functions <- function(study, home = tflplanner_home()) {
  st <- study_ard_functions(study)
  co <- tryCatch(company_ard_functions(home), error = function(e) NULL)
  if (is.null(co)) co <- data.frame(name = character(), file = character(),
                                    title = character(), description = character(),
                                    stat_names = character())
  names_ <- unique(c(st$name, co$name))
  a <- study$planner$ard$analyses
  checks <- own_function_checks(study)
  rows <- lapply(names_, function(n) {
    i <- match(n, st$name)
    j <- match(n, co$name)
    sf <- if (!is.na(i)) st$file[i] else NA_character_
    cf <- if (!is.na(j)) file.path(.own_fun_dir(home), co$file[j]) else NA_character_
    spath <- if (!is.na(sf)) file.path(study$path, sf)
    differs <- NA
    newer <- NA_character_
    if (!is.na(sf) && !is.na(cf) && file.exists(cf)) {
      differs <- !identical(unname(tools::md5sum(spath)), unname(tools::md5sum(cf)))
      if (isTRUE(differs)) {
        newer <- if (file.mtime(cf) > file.mtime(spath)) "company" else "study"
      }
    }
    used <- a[!is.na(a$method) & a$method == n, , drop = FALSE]
    src <- if (!is.na(i)) st else co
    k <- if (!is.na(i)) i else j
    ck <- checks[[n]]
    file_now <- if (!is.null(spath)) spath else cf
    stale <- !is.null(ck) && !is.null(ck$md5) && !is.na(file_now) && file.exists(file_now) &&
      !identical(ck$md5, unname(tools::md5sum(file_now)))
    data.frame(
      name = n,
      where = if (!is.na(i) && !is.na(j)) "both" else if (!is.na(i)) "study" else "company",
      study_file = sf, company_file = cf,
      loaded = !is.na(i) && isTRUE(st$loaded[i]),
      differs = differs, newer = newer,
      used_by = if (nrow(used)) paste(paste(used$output_id, used$analysis_id, sep = " / "),
                                      collapse = " | ") else NA_character_,
      title = src$title[k], description = src$description[k],
      stat_names = src$stat_names[k],
      tried = if (!is.null(ck)) ck$when %||% NA_character_ else NA_character_,
      problems = if (!is.null(ck)) as.integer(ck$problems %||% NA) else NA_integer_,
      stale = stale, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    name = character(), where = character(), study_file = character(),
    company_file = character(), loaded = logical(), differs = logical(),
    newer = character(), used_by = character(), title = character(),
    description = character(), stat_names = character(), tried = character(),
    problems = integer(), stale = logical(), stringsAsFactors = FALSE)), rows))
  rownames(out) <- NULL
  out
}

#' The last try of each of a study's own ARD functions
#'
#' @param study An `rtfstudy`.
#' @return A list by function name: `when`, `md5` (of the file tried),
#'   `data`, `args`, `problems` (errors and warnings found), `rows` (of the
#'   ARD it gave), `versions`.
#' @export
own_function_checks <- function(study) {
  f <- .fun_checks_file(study)
  if (!file.exists(f)) return(list())
  tryCatch(jsonlite::read_json(f, simplifyVector = TRUE), error = function(e) list())
}

.record_function_check <- function(study, name, rec) {
  f <- .fun_checks_file(study)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  all <- own_function_checks(study)
  all[[name]] <- rec
  jsonlite::write_json(all, f, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(rec)
}

# how a program reads a data file, by its extension
.read_code <- function(path) {
  q <- encodeString(path, quote = "\"")
  switch(tolower(tools::file_ext(path)),
         rds = sprintf("readRDS(%s)", q),
         xpt = sprintf("haven::read_xpt(%s)", q),
         sas7bdat = sprintf("haven::read_sas(%s)", q),
         csv = sprintf("utils::read.csv(%s, stringsAsFactors = FALSE)", q),
         parquet = sprintf("as.data.frame(arrow::read_parquet(%s))", q),
         sprintf("readRDS(%s)", q))
}

#' Try one of a study's (or the company's) ARD functions
#'
#' In a separate R process, so a function that hangs or fails does not stop
#' the app: loads the R files the study's ARD programs load (`source`, in
#' their order; the company's file too when the study does not have the
#' function), reads the data (a dataset of the ARD definition, of an
#' analysis set; `cards::ADSL` when none is given), and runs
#' [tflspec::tfl_check_ard_function()] with `args` as an analysis row would
#' give them.  The result is kept (`programs/ard/functions/.checks.json`).
#'
#' @param study An `rtfstudy`.
#' @param name The function.
#' @param dataset,population_id The data: a dataset of the ARD definition
#'   and an analysis set (either may be `NA`); both `NA`: `cards::ADSL`.
#' @param args Its arguments as R (`"by = TRT01A, variables = AGE"`).
#' @param home The tflplanner home.
#' @param timeout Seconds to allow.
#' @return A list: `problems` (data frame: `level`, `check`, `message`),
#'   `ard` (the first rows of the ARD it gave, or `NULL`), `error` (when it
#'   could not be tried), `seconds`.
#' @export
try_ard_function <- function(study, name, dataset = NA_character_,
                             population_id = NA_character_, args = "",
                             home = tflplanner_home(), timeout = 60) {
  x <- study$planner
  q <- function(p) encodeString(normalizePath(p, "/", FALSE), quote = "\"")
  files <- .study_fun_files(study)
  files <- files$file[files$loaded]
  own <- own_ard_functions(study, home)
  k <- match(name, own$name)
  if (is.na(k)) stop("No ARD function ", name, ".", call. = FALSE)
  extra <- if (is.na(own$study_file[k]) || !own$loaded[k]) {
    if (!is.na(own$study_file[k])) file.path(study$path, own$study_file[k]) else own$company_file[k]
  }
  # the data: the dataset (else the analysis set's), filtered to the set
  po <- x$ard$populations
  ds <- dataset
  pw <- NA_character_
  if (!is.na(population_id)) {
    i <- match(population_id, po$population_id)
    if (is.na(ds) && !is.na(i)) ds <- po$dataset[i]
    if (!is.na(i)) pw <- po$where[i]
  }
  d <- x$ard$datasets
  data_code <- if (!is.na(ds)) {
    pth <- d$path[match(ds, d$dataset)]
    if (is.na(pth)) stop("No dataset ", ds, " in the ARD definition.", call. = FALSE)
    c(paste(".data <-", .read_code(file.path(study$path, pth))),
      if (!is.na(pw)) sprintf(".data <- subset(.data, %s)", pw))
  } else ".data <- cards::ADSL"
  tmp <- tempfile("try")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  f_out <- file.path(tmp, "out.rds")
  call <- sprintf("tflspec::tfl_check_ard_function(%s, .data%s)", name,
                  if (nzchar(trimws(args))) paste0(", ", args) else "")
  script <- c(
    "suppressPackageStartupMessages(library(cards))",
    vapply(file.path(study$path, files), function(f) sprintf("source(%s)", q(f)), ""),
    if (!is.null(extra)) sprintf("source(%s)", q(extra)),
    data_code,
    ".res <- list(problems = NULL, ard = NULL, error = NULL)",
    ".res$problems <- tryCatch({",
    paste0("  .p <- ", call),
    "  .a <- attr(.p, \"ard\")",
    "  if (!is.null(.a)) .res$ard <- utils::head(.a, 20)",
    "  attr(.p, \"ard\") <- NULL",
    "  .p",
    "}, error = function(e) { .res$error <<- conditionMessage(e); NULL })",
    sprintf("saveRDS(.res, %s)", q(f_out)))
  f_script <- file.path(tmp, "try.R")
  writeLines(enc2utf8(script), f_script, useBytes = TRUE)
  t0 <- Sys.time()
  px <- processx::run(file.path(R.home("bin"), "Rscript"), f_script,
                      wd = study$path, error_on_status = FALSE,
                      timeout = timeout, stderr_to_stdout = TRUE)
  res <- if (file.exists(f_out)) readRDS(f_out) else
    list(problems = NULL, ard = NULL,
         error = if (isTRUE(px$timeout)) sprintf("It did not finish in %d seconds.", timeout)
                 else .first_error(px$stdout) %||% "It could not be tried.")
  res$seconds <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  file_now <- if (!is.na(own$study_file[k])) file.path(study$path, own$study_file[k]) else own$company_file[k]
  p <- res$problems
  .record_function_check(study, name, list(
    when = format(Sys.time(), "%Y-%m-%d %H:%M"),
    md5 = unname(tools::md5sum(file_now)),
    data = if (!is.na(ds)) paste(c(ds, if (!is.na(population_id)) population_id), collapse = " x ") else "cards::ADSL",
    args = args,
    problems = if (!is.null(res$error)) NA_integer_ else
      sum(p$level %in% c("error", "warning")),
    rows = if (!is.null(res$ard)) nrow(res$ard) else 0L,
    error = res$error,
    versions = c(tflspec = as.character(utils::packageVersion("tflspec")),
                 cards = as.character(utils::packageVersion("cards")))))
  res
}

#' Start an ARD function of one's own from a template
#'
#' Writes [tflspec::tfl_ard_function_template()]'s skeleton (and its test,
#' with `test = TRUE`) into the study's `programs/ard/functions/` -- and
#' adds it to the study key `source`, so the ARD programs load it -- or
#' into the company standards' `ard_functions/`.  Edit it afterwards (in
#' RStudio ...), then try it.
#'
#' @param study An `rtfstudy`.
#' @param name The function's name (`ard_...`).
#' @param type `"summary"`, `"test"` or `"free"`.
#' @param where `"study"` or `"company"`.
#' @param test Write a testthat file next to it.
#' @param home The tflplanner home.
#' @return The study (its `source` updated when written for the study).
#' @export
new_ard_function <- function(study, name, type = c("summary", "test", "free"),
                             where = c("study", "company"), test = TRUE,
                             home = tflplanner_home()) {
  type <- match.arg(type)
  where <- match.arg(where)
  if (!grepl("^ard_[A-Za-z0-9_.]+$", name)) {
    stop("An ARD function's name starts with ard_ (ard_riskdiff).", call. = FALSE)
  }
  if (name %in% own_ard_functions(study, home)$name) {
    stop("There is an ARD function ", name, " already.", call. = FALSE)
  }
  dir <- if (where == "study") file.path(study$path, .study_fun_dir) else .own_fun_dir(home)
  if (where == "company" && !.company_writable(home)) {
    stop("The company standards' folder cannot be written: ", dir, call. = FALSE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, paste0(name, ".R"))
  tflspec::tfl_ard_function_template(name, type, file = f, test = test)
  if (where == "study") study <- .add_source(study, file.path(.study_fun_dir, basename(f)))
  study
}

# the study key `source` with one more file
.add_source <- function(study, rel) {
  st <- study$planner$ard$study
  k <- match("source", st$key)
  have <- if (is.na(k)) character() else .split_bar(st$value[k])
  if (rel %in% have) return(study)
  if (is.na(k)) {
    st[nrow(st) + 1L, ] <- NA
    k <- nrow(st)
    st$key[k] <- "source"
  }
  st$value[k] <- paste(c(have, rel), collapse = " | ")
  study$planner$ard$study <- st
  study
}

# can tflplanner write to the company standards' ARD functions?
.company_writable <- function(home = tflplanner_home()) {
  d <- .own_fun_dir(home)
  probe <- if (dir.exists(d)) d else dirname(d)
  while (!dir.exists(probe) && probe != dirname(probe)) probe <- dirname(probe)
  file.access(probe, 2L) == 0L
}

#' Replace a study's copy of an ARD function with the company's
#'
#' The study's file of that function is overwritten with the company's
#' (when the company's is newer, say).  The study's results change only
#' when its ARD is made again.
#'
#' @param study An `rtfstudy`.
#' @param name The function.
#' @param home The tflplanner home.
#' @return The study file's path, invisibly.
#' @export
replace_with_company_ard_function <- function(study, name, home = tflplanner_home()) {
  own <- own_ard_functions(study, home)
  k <- match(name, own$name)
  if (is.na(k) || own$where[k] != "both") {
    stop(name, " is not both the study's and the company's.", call. = FALSE)
  }
  dest <- file.path(study$path, own$study_file[k])
  file.copy(own$company_file[k], dest, overwrite = TRUE)
  invisible(dest)
}

# a column list as a call writes it: AGE, or c(AGE, BMIBL)
.vars <- function(x) {
  v <- .split_bar(x)
  if (length(v) == 1L) v else paste0("c(", paste(v, collapse = ", "), ")")
}
