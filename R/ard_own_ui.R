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
#'   the study folder), `loaded`, `title`, `description`, `stat_names`,
#'   `keywords` (as [company_ard_functions()]).
#' @export
study_ard_functions <- function(study) {
  fs <- .study_fun_files(study)
  none <- data.frame(name = character(), file = character(), loaded = logical(),
                     title = character(), description = character(),
                     stat_names = character(), keywords = character(),
                     stringsAsFactors = FALSE)
  have <- file.exists(file.path(study$path, fs$file))
  fs <- fs[have, , drop = FALSE]
  if (!nrow(fs)) return(none)
  info <- tflspec::tfl_ard_function_info(file.path(study$path, fs$file))
  info <- info[!is.na(info$name), , drop = FALSE]
  if (!nrow(info)) return(none)
  kw <- .fn_own_keywords(file.path(study$path, fs$file))
  rel <- fs$file[match(normalizePath(info$file, "/", FALSE),
                       normalizePath(file.path(study$path, fs$file), "/", FALSE))]
  out <- data.frame(name = info$name, file = rel,
                    loaded = fs$loaded[match(rel, fs$file)],
                    title = info$title, description = info$description,
                    stat_names = info$stat_names,
                    keywords = kw$keywords[match(info$name, kw$name)],
                    stringsAsFactors = FALSE)
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
#'   `title`, `description`, `stat_names`, `keywords`, `tried` (when), `problems`
#'   (errors and warnings found), `stale` (the file changed since).
#' @export
own_ard_functions <- function(study, home = tflplanner_home()) {
  st <- study_ard_functions(study)
  co <- tryCatch(company_ard_functions(home), error = function(e) NULL)
  if (is.null(co)) co <- data.frame(name = character(), file = character(),
                                    title = character(), description = character(),
                                    stat_names = character(), keywords = character())
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
      ch <- .copy_change(study, n, spath, cf)
      differs <- !identical(ch, "same")
      if (isTRUE(differs)) newer <- ch
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
      stat_names = src$stat_names[k], keywords = src$keywords[k],
      tried = if (!is.null(ck)) ck$when %||% NA_character_ else NA_character_,
      problems = if (!is.null(ck)) as.integer(ck$problems %||% NA) else NA_integer_,
      stale = stale, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    name = character(), where = character(), study_file = character(),
    company_file = character(), loaded = logical(), differs = logical(),
    newer = character(), used_by = character(), title = character(),
    description = character(), stat_names = character(), keywords = character(),
    tried = character(),
    problems = integer(), stale = logical(), stringsAsFactors = FALSE)), rows))
  rownames(out) <- NULL
  out
}

#' The last try of each of a study's own ARD functions
#'
#' @param study An `rtfstudy`.
#' @return A list by function name: `when`, `user`, `md5` (of the file
#'   tried -- not of other files it may use: a change there is not seen),
#'   `data`, `args`, `problems` (errors and warnings found), `errors`,
#'   `warnings`, `notes`, `rows` (of the whole ARD it gave), `versions`
#'   (tflspec, cards, cardx).  A company's function tried before the study
#'   uses it is kept here too: once it is copied, the record holds (the
#'   files are the same).
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

# The code that makes a dataset x analysis set's data as the ARD programs
# make it (tflspec writes it for one analysis on it), ending with `.data`
.try_data_code <- function(x, dataset, population_id) {
  a <- x$ard
  a$analyses <- .normalize_ard_sheet(data.frame(
    output_id = ".try", analysis_id = "TRY", method = "cards::ard_summary",
    dataset = dataset, population_id = population_id, variables = "TRY_",
    stringsAsFactors = FALSE), "analyses")
  code <- tflspec::tfl_ard_code(structure(a, class = "tfl_ard_spec"), part = "body")
  stop_at <- match("# ---- analyses ----", code)
  # the analysis pipes its data into the call: `ard_try <- adsl_saf |>`
  call <- code[grep("^ard_try <- ", code)[1L]]
  obj <- sub("^ard_try <- ([A-Za-z0-9_.]+).*$", "\\1", call)
  c(code[seq_len(stop_at - 1L)], paste(".data <-", obj))
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
  # the data, made by the code the ARD programs make it with (the analysis
  # set from its dataset, the other datasets cut to its subjects)
  data_code <- if (!is.na(dataset) || !is.na(population_id)) {
    .try_data_code(x, dataset, population_id)
  } else c(".data <- cards::ADSL")
  ds <- dataset
  tmp <- tempfile("try")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  f_out <- file.path(tmp, "out.rds")
  call <- sprintf("tflspec::tfl_check_ard_function(%s, .data%s)", name,
                  if (nzchar(trimws(args))) paste0(", ", args) else "")
  script <- c(
    # as the ARD programs' setup: cards, dplyr, and the functions the
    # programs call (programs/study_helpers.R's)
    "suppressPackageStartupMessages({library(cards); library(dplyr)})",
    tflspec::tfl_helpers_code(),
    "tryCatch({",
    vapply(file.path(study$path, files), function(f) sprintf("  source(%s)", q(f)), ""),
    if (!is.null(extra)) sprintf("  source(%s)", q(extra)),
    "}, error = function(e) {",
    sprintf("  saveRDS(list(error = paste(\"[files]\", conditionMessage(e))), %s); quit(save = \"no\")", q(f_out)),
    "})",
    ".res <- list(problems = NULL, ard = NULL, error = NULL)",
    "tryCatch({",
    paste0("  ", data_code),
    "}, error = function(e) {",
    "  .res$error <<- paste(\"[data]\", conditionMessage(e))",
    sprintf("  saveRDS(.res, %s); quit(save = \"no\")", q(f_out)),
    "})",
    ".res$problems <- tryCatch({",
    paste0("  .p <- ", call),
    "  .a <- attr(.p, \"ard\")",
    "  if (!is.null(.a)) { .res$ard <- utils::head(.a, 20); .res$rows <- nrow(.a) }",
    "  attr(.p, \"ard\") <- NULL",
    "  .p",
    "}, error = function(e) { .res$error <<- paste(\"[function]\", conditionMessage(e)); NULL })",
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
    data = if (!is.na(ds) || !is.na(population_id))
      paste(stats::na.omit(c(ds, population_id)), collapse = " x ") else "cards::ADSL",
    args = args,
    problems = if (!is.null(res$error)) NA_integer_ else
      sum(p$level %in% c("error", "warning")),
    errors = sum(p$level %in% "error"), warnings = sum(p$level %in% "warning"),
    notes = sum(p$level %in% "note"),
    rows = res$rows %||% 0L,
    error = res$error,
    user = Sys.info()[["user"]],
    versions = as.list(vapply(c("tflspec", "cards", "cardx"), function(pk)
      if (requireNamespace(pk, quietly = TRUE)) as.character(utils::packageVersion(pk))
      else NA_character_, ""))))
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
  taken <- unlist(lapply(c("cards", "cardx"), function(pk)
    if (requireNamespace(pk, quietly = TRUE)) getNamespaceExports(pk)))
  if (name %in% taken) {
    stop(name, " is a cards / cardx function: a function of one's own would hide it.",
         call. = FALSE)
  }
  dir <- if (where == "study") file.path(study$path, .study_fun_dir) else .own_fun_dir(home)
  if (where == "company" && !.company_writable(home)) {
    stop("The company standards' folder cannot be written: ", dir, call. = FALSE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, paste0(name, ".R"))
  tflspec::tfl_ard_function_template(name, type, file = f, test = test)
  .add_keywords_line(f, name)
  if (where == "study") study <- .add_source(study, file.path(.study_fun_dir, basename(f)))
  study
}

# a new function's file: an empty `# tflplanner-keywords:` line at the top
# of the comments above it, to fill in (the words the ARD form's search
# finds it by; a plain comment, which roxygen does not read)
.add_keywords_line <- function(file, name) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  d <- grep(paste0("^", gsub(".", "[.]", name, fixed = TRUE), "[[:space:]]*(<-|=)"), x)[1L]
  if (is.na(d)) return(invisible(FALSE))
  k <- d - 1L
  while (k >= 1L && grepl("^[[:space:]]*#", x[k])) k <- k - 1L
  x <- append(x, "# tflplanner-keywords: ", after = k)
  writeLines(enc2utf8(x), file, useBytes = TRUE)
  invisible(TRUE)
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
take_company_ard_function <- function(study, name, home = tflplanner_home()) {
  own <- own_ard_functions(study, home)
  k <- match(name, own$name)
  if (is.na(k) || own$where[k] != "both") {
    stop(name, " is not both the study's and the company's.", call. = FALSE)
  }
  dest <- file.path(study$path, own$study_file[k])
  # the study's file may hold other functions: not lost
  mine <- tflspec::tfl_ard_function_info(dest)$name
  theirs <- tflspec::tfl_ard_function_info(own$company_file[k])$name
  lost <- setdiff(stats::na.omit(mine), theirs)
  if (length(lost)) {
    stop("The study's ", own$study_file[k], " holds ", paste(lost, collapse = ", "),
         " as well, which the company's file has not: not replaced.", call. = FALSE)
  }
  file.copy(own$company_file[k], dest, overwrite = TRUE)
  .record_copied(study, name, own$company_file[k])
  invisible(dest)
}

# a column list as a call writes it: AGE, or c(AGE, BMIBL)
.vars <- function(x) {
  v <- .split_bar(x)
  if (length(v) == 1L) v else paste0("c(", paste(v, collapse = ", "), ")")
}

# What a study took from the company: the company file's md5 when it was
# copied (programs/ard/functions/.copied.json), so a later difference is
# told apart without file times: the company's changed, the study's, both
.copied_file <- function(study) file.path(study$path, .study_fun_dir, ".copied.json")

.copied <- function(study) {
  f <- .copied_file(study)
  if (!file.exists(f)) return(list())
  tryCatch(jsonlite::read_json(f, simplifyVector = TRUE), error = function(e) list())
}

.record_copied <- function(study, name, company_file) {
  all <- .copied(study)
  all[[name]] <- list(md5 = unname(tools::md5sum(company_file)),
                      when = format(Sys.time(), "%Y-%m-%d %H:%M"))
  dir.create(dirname(.copied_file(study)), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(all, .copied_file(study), auto_unbox = TRUE, pretty = TRUE)
}

# How a study's copy and the company's file differ: "same", "company" (the
# company's changed since it was copied), "study" (changed in the study),
# "both", or "unknown" (no record of the copy)
.copy_change <- function(study, name, study_file, company_file) {
  ms <- unname(tools::md5sum(study_file))
  mc <- unname(tools::md5sum(company_file))
  if (identical(ms, mc)) return("same")
  was <- .copied(study)[[name]]$md5
  if (is.null(was)) return("unknown")
  if (identical(ms, was)) return("company")
  if (identical(mc, was)) return("study")
  "both"
}
