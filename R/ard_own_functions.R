# ============================================================================
#  ARD functions of the company's own
# ----------------------------------------------------------------------------
#  A company keeps its own ard_*() functions as R files in the standards
#  folder, standards/ard_functions/.  A study that uses one gets a copy in
#  its own programs/ard/functions/, and the ARD definition's study key
#  `source` names it -- so the study's programs run from the study folder
#  alone, and a later change to the company's file does not change a study
#  already made.  A study file of the same name wins: it is not replaced.
#  tflspec::tfl_check_ard_function() tries one before it is used.
# ============================================================================

.own_fun_dir <- function(home = tflplanner_home()) {
  file.path(home, "standards", "ard_functions")
}

.study_fun_dir <- file.path("programs", "ard", "functions")

# the functions an R file defines at its top level (`name <- function`)
.defined_functions <- function(file) {
  ex <- tryCatch(parse(file, keep.source = FALSE), error = function(e) NULL)
  out <- character()
  for (e in ex) {
    if (is.call(e) && as.character(e[[1L]]) %in% c("<-", "=") &&
        is.name(e[[2L]]) && is.call(e[[3L]]) &&
        identical(e[[3L]][[1L]], as.name("function"))) {
      out <- c(out, as.character(e[[2L]]))
    }
  }
  out
}

#' The company's own ARD functions
#'
#' The functions defined in the R files of the standards folder's
#' `ard_functions/` (`<home>/standards/ard_functions/*.R`): one row each,
#' with its `file`.  An analysis names one as its `method` once the study
#' uses it ([use_company_ard_function()]).
#'
#' @param home The tflplanner home.
#' @return A data frame: `name`, `file`.
#' @export
company_ard_functions <- function(home = tflplanner_home()) {
  fs <- list.files(.own_fun_dir(home), pattern = "[.][Rr]$", full.names = TRUE)
  rows <- lapply(fs, function(f) {
    n <- .defined_functions(f)
    if (length(n)) data.frame(name = n, file = basename(f),
                              stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(name = character(),
                                          file = character(),
                                          stringsAsFactors = FALSE)), rows))
  rownames(out) <- NULL
  out
}

#' Use one of the company's ARD functions in a study
#'
#' Copies the file that defines it into the study's
#' `programs/ard/functions/` -- unless the study has a file of that name,
#' which wins -- and adds it to the ARD definition's study key `source`, so
#' the study's ARD programs load it.  Save the study afterwards.
#'
#' @param study An `rtfstudy`.
#' @param name The function.
#' @param home The tflplanner home.
#' @return The study, its ARD definition's `source` updated.
#' @export
use_company_ard_function <- function(study, name, home = tflplanner_home()) {
  own <- company_ard_functions(home)
  i <- match(name, own$name)
  if (is.na(i)) {
    stop("The company has no ARD function ", name, " (",
         .own_fun_dir(home), ").", call. = FALSE)
  }
  rel <- file.path(.study_fun_dir, own$file[i])
  dest <- file.path(study$path, rel)
  if (file.exists(dest)) {
    message("The study has its own ", rel, ": kept, not replaced.")
  } else {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(.own_fun_dir(home), own$file[i]), dest)
  }
  st <- study$planner$ard$study
  k <- match("source", st$key)
  have <- if (is.na(k)) character() else .split_bar(st$value[k])
  if (!rel %in% have) {
    v <- paste(c(have, rel), collapse = " | ")
    if (is.na(k)) {
      st[nrow(st) + 1L, ] <- NA
      k <- nrow(st)
      st$key[k] <- "source"
    }
    st$value[k] <- v
    study$planner$ard$study <- st
  }
  study
}

#' Try one of the company's ARD functions
#'
#' Loads its file and runs [tflspec::tfl_check_ard_function()] on `data`,
#' with the arguments an analysis row would give.
#'
#' @param name The function.
#' @param data The data to try it on.
#' @param ... Its other arguments (`by = TRT01A, variables = AGE`).
#' @param stat_names The statistics it should give.
#' @param home The tflplanner home.
#' @return The problems found ([tflspec::tfl_check_ard_function()]).
#' @export
check_company_ard_function <- function(name, data, ..., stat_names = NULL,
                                       home = tflplanner_home()) {
  own <- company_ard_functions(home)
  i <- match(name, own$name)
  if (is.na(i)) stop("The company has no ARD function ", name, ".", call. = FALSE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(.own_fun_dir(home), own$file[i]), envir = env)
  f <- get(name, envir = env)
  eval(substitute(tflspec::tfl_check_ard_function(f, data, ..., stat_names = stat_names)))
}
