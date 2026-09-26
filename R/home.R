# The app's own home: where rtfplanner keeps what it knows about each study,
# apart from the study folders themselves.
#
#   <home>/
#     config.yml                 studies_root (where new study folders go),
#                                last_study
#     studies/<STUDY_ID>/
#       state.json               the study as last saved: study.yml fields,
#                                its folder, every sheet, the report list,
#                                the data code -- the master copy
#       history/<time>.json      each earlier saved state
#
# The definition workbooks in a study folder's spec/ are written from this
# state on every save (the programs read them), and can be exported or
# imported on request; the app itself opens a study from its state.
#
# Where the home is: option(rtfplanner.home), else the environment variable
# RTFPLANNER_HOME, else the folder setup_rtfplanner() was last given (kept
# in a one-line file in tools::R_user_dir("rtfplanner", "config")), else
# tools::R_user_dir("rtfplanner", "data").

.pointer_file <- function() {
  file.path(tools::R_user_dir("rtfplanner", "config"), "home")
}

#' Where rtfplanner keeps its settings and the studies' saved state
#'
#' @return The home folder (it may not exist yet: see
#'   [setup_rtfplanner()]).
#' @export
rtfplanner_home <- function() {
  h <- getOption("rtfplanner.home")
  if (is.null(h) || !nzchar(h)) h <- Sys.getenv("RTFPLANNER_HOME")
  if (!nzchar(h) && file.exists(.pointer_file())) {
    h <- trimws(readLines(.pointer_file(), n = 1L, warn = FALSE))
  }
  if (!length(h) || !nzchar(h)) h <- tools::R_user_dir("rtfplanner", "data")
  normalizePath(h, "/", mustWork = FALSE)
}

.config_file <- function(home = rtfplanner_home()) file.path(home, "config.yml")

#' Set up rtfplanner's home
#'
#' Creates the folder where rtfplanner keeps its settings and the saved
#' state of every study, and says where new study folders go.  Run it once;
#' [run_app()] runs it with the defaults when it finds no home.  Running it
#' again changes the settings given and keeps the rest.
#'
#' @param home The home folder.  `NULL` keeps the current one (see
#'   [rtfplanner_home()]); a folder given here is remembered for later
#'   sessions.
#' @param studies_root Where new study folders are created.  `NULL` keeps
#'   the current setting, or on a first setup uses `<home>/workspace`.
#' @param language The app's language, `"en"` (the default) or `"ja"`;
#'   `NULL` keeps the current setting.
#' @return The settings, invisibly.
#' @examples
#' \dontrun{
#' setup_rtfplanner(studies_root = "C:/studies")
#' }
#' @export
setup_rtfplanner <- function(home = NULL, studies_root = NULL,
                             language = NULL) {
  if (!is.null(home)) {
    home <- normalizePath(home, "/", mustWork = FALSE)
    dir.create(dirname(.pointer_file()), recursive = TRUE,
               showWarnings = FALSE)
    writeLines(home, .pointer_file())
  } else {
    home <- rtfplanner_home()
  }
  dir.create(file.path(home, "studies"), recursive = TRUE,
             showWarnings = FALSE)
  cfg <- rtfplanner_config(home)
  if (!is.null(studies_root)) cfg$studies_root <- studies_root
  if (is.null(cfg$studies_root) || !nzchar(cfg$studies_root)) {
    cfg$studies_root <- file.path(home, "workspace")
  }
  cfg$studies_root <- normalizePath(cfg$studies_root, "/", mustWork = FALSE)
  if (!is.null(language)) cfg$language <- match.arg(language, app_languages())
  dir.create(cfg$studies_root, recursive = TRUE, showWarnings = FALSE)
  .write_config(cfg, home)
  message("rtfplanner home: ", home, "\nnew studies go to: ",
          cfg$studies_root)
  invisible(c(list(home = home), cfg))
}

.is_set_up <- function(home = rtfplanner_home()) file.exists(.config_file(home))

#' rtfplanner's settings
#'
#' @param home The home folder.
#' @return A list: `studies_root`, `last_study`, `language`.
#' @export
rtfplanner_config <- function(home = rtfplanner_home()) {
  f <- .config_file(home)
  cfg <- if (file.exists(f)) yaml::read_yaml(f) else list()
  keys <- c("studies_root", "last_study", "language")
  cfg[keys] <- lapply(cfg[keys], function(v)
      if (is.null(v) || !nzchar(v)) NULL else as.character(v))
  cfg
}

.write_config <- function(cfg, home = rtfplanner_home()) {
  cfg <- cfg[!vapply(cfg, is.null, NA)]
  yaml::write_yaml(cfg, .config_file(home))
}

.set_config <- function(key, value, home = rtfplanner_home()) {
  cfg <- rtfplanner_config(home)
  cfg[[key]] <- value
  .write_config(cfg, home)
}

#' @rdname rtfplanner_config
#' @export
studies_root <- function(home = rtfplanner_home()) {
  r <- rtfplanner_config(home)$studies_root
  if (is.null(r)) file.path(home, "workspace") else r
}

# ------------------------------------------------------------- the store

.store_dir <- function(id, home = rtfplanner_home()) {
  file.path(home, "studies", id)
}

.state_file <- function(id, home = rtfplanner_home()) {
  file.path(.store_dir(id, home), "state.json")
}

.cols_df <- function(x) {
  # a data frame as named columns; jsonlite keeps a zero-row frame's
  # columns this way, which rows would lose
  lapply(as.list(x), as.character)
}

.state_of <- function(study) {
  p <- study$planner
  list(format = 1L,
       rtfplanner = as.character(utils::packageVersion("rtfplanner")),
       saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
       path = study$path,
       meta = lapply(study$meta[.study_fields], function(v)
         if (is.na(v)) NULL else v),
       planner = list(
         study = as.list(p$study[!is.na(p$study)]),
         setup = if (is.na(p$setup)) NULL else p$setup,
         outputs = .cols_df(p$outputs),
         sheets = lapply(p$sheets, .cols_df)))
}

.json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, na = "null",
                   null = "null")
}

.df_from <- function(cols, template) {
  n <- if (length(cols)) max(0L, lengths(cols)) else 0L
  out <- lapply(names(template), function(cn) {
    v <- cols[[cn]]
    if (is.null(v)) rep(NA_character_, n) else {
      v <- as.character(unlist(lapply(v, function(e)
        if (is.null(e)) NA_character_ else e)))
      v
    }
  })
  as.data.frame(stats::setNames(out, names(template)),
                stringsAsFactors = FALSE)
}

.planner_from_state <- function(st) {
  p <- new_planner()
  ps <- st$planner
  for (k in names(ps$study)) {
    if (k %in% names(p$study)) p$study[[k]] <- as.character(ps$study[[k]])
  }
  if (!is.null(ps$setup)) p$setup <- as.character(ps$setup)
  p$outputs <- .df_from(ps$outputs, .empty_outputs())
  for (s in names(p$sheets)) {
    p$sheets[[s]] <- .normalize_sheet(.df_from(ps$sheets[[s]],
                                               p$sheets[[s]]), s)
  }
  p
}

.read_state <- function(id, home = rtfplanner_home()) {
  f <- .state_file(id, home)
  if (!file.exists(f)) return(NULL)
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}

.write_state <- function(study, home = rtfplanner_home()) {
  id <- study$meta$study_id
  dir <- .store_dir(id, home)
  dir.create(file.path(dir, "history"), recursive = TRUE,
             showWarnings = FALSE)
  f <- .state_file(id, home)
  new <- .state_of(study)
  body <- function(x) {
    x$saved <- NULL
    x$rtfplanner <- NULL
    x
  }
  if (file.exists(f)) {
    old <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    if (identical(.json(body(old)), .json(body(.from_json(.json(new)))))) {
      return(invisible(FALSE))
    }
    stamp <- gsub("[^0-9]", "", old$saved %||% format(file.mtime(f)))
    file.copy(f, file.path(dir, "history", paste0(stamp, ".json")))
  }
  writeLines(enc2utf8(.json(new)), f, useBytes = TRUE)
  invisible(TRUE)
}

.from_json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)
