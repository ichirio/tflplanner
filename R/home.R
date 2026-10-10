# The app's own home: where tflplanner keeps what it knows about each study,
# apart from the study folders themselves.
#
#   <home>/
#     config.yml                 studies_root (where new study folders go),
#                                last_study
#     studies/<STUDY_ID>/
#       state.json               the study as last saved: study.yml fields,
#                                its folder, every sheet, the report list,
#                                the data code -- a copy of the study
#                                folder's definition files, with each file's
#                                fingerprint (#274)
#       history/<time>.json      each earlier saved state
#
# The definition files in a study folder (spec/, study.yml) are the study's
# source: written on every save, and read back on open when they changed
# outside tflplanner (R/spec_source.R); unchanged, the study opens from its
# state.
#
# Where the home is: option(tflplanner.home), else the environment variable
# TFLPLANNER_HOME, else the folder setup_tflplanner() was last given (kept
# in a one-line file in tools::R_user_dir("tflplanner", "config")), else
# tools::R_user_dir("tflplanner", "data").

.pointer_file <- function() {
  file.path(tools::R_user_dir("tflplanner", "config"), "home")
}

# a folder in the temporary folder (where tempdir() is made)
.in_temp <- function(path) {
  p <- .real_path(path)
  tmp <- .real_path(dirname(tempdir()))
  startsWith(p, paste0(sub("/$", "", tmp), "/"))
}

# A path as the file system has it, so two ways of naming a folder compare
# equal: its nearest existing folder resolved (normalizePath() resolves
# only what exists: macOS's /var is /private/var), the rest kept; macOS's
# /private dropped; in lower case on Windows
.real_path <- function(path) {
  p <- normalizePath(path, "/", mustWork = FALSE)
  rest <- character()
  while (!file.exists(p) && !identical(dirname(p), p)) {
    rest <- c(basename(p), rest)
    p <- dirname(p)
  }
  p <- normalizePath(p, "/", mustWork = FALSE)
  if (length(rest)) p <- paste(c(sub("/$", "", p), rest), collapse = "/")
  p <- sub("^/private(/(var|tmp|etc)(/|$))", "\\1", p)
  if (.Platform$OS.type == "windows") tolower(p) else p
}

#' Where tflplanner keeps its settings and the studies' saved state
#'
#' @return The home folder (it may not exist yet: see
#'   [setup_tflplanner()]).
#' @export
tflplanner_home <- function() {
  h <- getOption("tflplanner.home")
  if (is.null(h) || !nzchar(h)) h <- Sys.getenv("TFLPLANNER_HOME")
  if (!nzchar(h) && file.exists(.pointer_file())) {
    h <- trimws(readLines(.pointer_file(), n = 1L, warn = FALSE))
  }
  if (!length(h) || !nzchar(h)) h <- tools::R_user_dir("tflplanner", "data")
  normalizePath(h, "/", mustWork = FALSE)
}

.config_file <- function(home = tflplanner_home()) file.path(home, "config.yml")

#' Set up tflplanner's home
#'
#' Creates the folder where tflplanner keeps its settings and the saved
#' state of every study, and says where new study folders go.  Run it once;
#' [run_app()] runs it with the defaults when it finds no home.  Running it
#' again changes the settings given and keeps the rest.
#'
#' **Called with no arguments in an interactive session**, it walks you
#' through the setup step by step, asking before it changes anything:
#'
#' 1. the home folder and where new study folders go;
#' 2. the packages tflplanner's programs use (cards, cardx, dplyr, ...)
#'    that are not installed yet ([tflplanner_packages()]);
#' 3. a shortcut that starts tflplanner with a double click
#'    ([add_shortcut()]).
#'
#' Everything else -- the study folders, the language, the company
#' standards, the sample study -- can be changed in the app's settings.
#' To update tflplanner later, use [update_tflplanner()] or the "update and
#' launch" shortcut.
#'
#' @param home The home folder.  `NULL` keeps the current one (see
#'   [tflplanner_home()]); a folder given here is remembered for later
#'   sessions -- one in the temporary folder only for this session.
#' @param studies_root Where new study folders are created.  `NULL` keeps
#'   the current setting, or on a first setup uses `<home>/workspace`.
#' @param language The app's language, `"en"` (the default) or `"ja"`;
#'   `NULL` keeps the current setting.
#' @param standards A company standards workbook ([standards_template()])
#'   to install, `"builtin"` to go back to the built-in draft, or `NULL` to
#'   keep what is installed.
#' @param sample `TRUE` adds the sample study (SAMPLE-01) to the studies
#'   folder and makes its ARD and reports ([create_sample_study()]); a
#'   sample already there is left as it is.
#' @param port The port the app runs on when started from its shortcut or
#'   [launch_app()] (default 7470); `NULL` keeps the setting.
#' @param check_updates Whether the app looks for a newer version when it
#'   starts (default `FALSE`; it only says so, it installs nothing); `NULL`
#'   keeps the setting.  tflplanner is updated only when you ask:
#'   [update_tflplanner()], or the "update and launch" shortcut
#'   (`add_shortcut(update = TRUE)`).
#' @return The settings, invisibly.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' tflplanner_config()
#' options(old)
#'
#' \dontrun{
#' # not run: these remember the home and the study folder for every later
#' # session (and the first one asks, step by step)
#' setup_tflplanner()
#' setup_tflplanner(studies_root = "C:/studies", sample = TRUE)
#' }
#' @export
setup_tflplanner <- function(home = NULL, studies_root = NULL,
                             language = NULL, standards = NULL,
                             sample = FALSE, port = NULL,
                             check_updates = NULL) {
  if (nargs() == 0L && interactive()) return(.setup_wizard())
  if (!is.null(port)) port <- .check_port(port)
  if (!is.null(home)) {
    home <- normalizePath(home, "/", mustWork = FALSE)
    if (.in_temp(home)) {
      # a home in the temporary folder (a script's, a test's) is this
      # session's only: remembered, every later session would open a folder
      # that is deleted when this one ends
      options(tflplanner.home = home)
      message("The home ", home, " is in the temporary folder: it is used in ",
              "this R session only, not remembered for later ones.")
    } else {
      dir.create(dirname(.pointer_file()), recursive = TRUE,
                 showWarnings = FALSE)
      writeLines(home, .pointer_file())
    }
  } else {
    home <- tflplanner_home()
  }
  dir.create(file.path(home, "studies"), recursive = TRUE,
             showWarnings = FALSE)
  cfg <- tflplanner_config(home)
  if (!is.null(studies_root)) cfg$studies_root <- studies_root
  if (is.null(cfg$studies_root) || !nzchar(cfg$studies_root)) {
    cfg$studies_root <- file.path(home, "workspace")
  }
  # made before it is normalized, so the path is the resolved one
  # (symbolic links, short names)
  dir.create(cfg$studies_root, recursive = TRUE, showWarnings = FALSE)
  cfg$studies_root <- normalizePath(cfg$studies_root, "/", mustWork = FALSE)
  if (!is.null(language)) cfg$language <- match.arg(language, app_languages())
  if (!is.null(port)) cfg$port <- port
  if (!is.null(check_updates)) cfg$check_updates <- isTRUE(check_updates)
  if (!is.null(standards)) {
    f <- .standards_file(home)
    if (identical(standards, "builtin")) {
      unlink(f)
    } else {
      read_standards(standards)          # refuse a workbook that does not read
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      file.copy(standards, f, overwrite = TRUE)
    }
  }
  .write_config(cfg, home)
  message("tflplanner home: ", home, "\nnew studies go to: ",
          cfg$studies_root)
  if (isTRUE(sample)) {
    if (!is.null(.read_state(.sample_id, home)) ||
        file.exists(file.path(cfg$studies_root, .sample_id))) {
      message("The sample study ", .sample_id, " is already there.")
    } else {
      create_sample_study(cfg$studies_root, home = home)
    }
  }
  invisible(c(list(home = home), cfg))
}

.is_set_up <- function(home = tflplanner_home()) file.exists(.config_file(home))

#' tflplanner's settings
#'
#' @param home The home folder.
#' @return A list: `studies_root`, `last_study`, `language`, and when set
#'   `port`, `update_channel`, `check_updates`.
#' @export
tflplanner_config <- function(home = tflplanner_home()) {
  f <- .config_file(home)
  cfg <- if (file.exists(f)) yaml::read_yaml(f) else list()
  keys <- c("studies_root", "last_study", "language", "update_channel")
  cfg[keys] <- lapply(cfg[keys], function(v)
      if (is.null(v) || !nzchar(v)) NULL else as.character(v))
  cfg
}

.write_config <- function(cfg, home = tflplanner_home()) {
  cfg <- cfg[!vapply(cfg, is.null, NA)]
  yaml::write_yaml(cfg, .config_file(home))
}

.set_config <- function(key, value, home = tflplanner_home()) {
  cfg <- tflplanner_config(home)
  cfg[[key]] <- value
  .write_config(cfg, home)
}

#' @rdname tflplanner_config
#' @export
studies_root <- function(home = tflplanner_home()) {
  r <- tflplanner_config(home)$studies_root
  if (is.null(r)) file.path(home, "workspace") else r
}

# ------------------------------------------------------------- the store

.store_dir <- function(id, home = tflplanner_home()) {
  file.path(home, "studies", id)
}

# The draft: what a session changed and has not saved yet, kept beside the
# study's state so that a reload, a closed tab or a lost connection does not
# lose it.  Saving removes it; opening the study offers it back.
.draft_file <- function(id, home = tflplanner_home()) {
  file.path(.store_dir(id, home), "draft.json")
}

.write_draft <- function(study, home = tflplanner_home()) {
  f <- .draft_file(study$meta$study_id, home)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(.json(.state_of(study))), f, useBytes = TRUE)
  invisible(f)
}

.read_draft <- function(id, home = tflplanner_home()) {
  f <- .draft_file(id, home)
  if (!file.exists(f)) return(NULL)
  st <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE),
                 error = function(e) NULL)
  if (is.null(st)) return(NULL)
  .study_from_state(st)
}

.drop_draft <- function(id, home = tflplanner_home()) {
  f <- .draft_file(id, home)
  if (file.exists(f)) unlink(f)
  invisible(NULL)
}

.state_file <- function(id, home = tflplanner_home()) {
  file.path(.store_dir(id, home), "state.json")
}

.cols_df <- function(x) {
  # a data frame as named columns; jsonlite keeps a zero-row frame's
  # columns this way, which rows would lose
  lapply(as.list(x), as.character)
}

.state_of <- function(study) {
  p <- study$planner
  list(format = 2L,
       tflplanner = as.character(utils::packageVersion("tflplanner")),
       saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
       path = study$path,
       meta = lapply(study$meta[.study_fields], function(v)
         if (is.na(v)) NULL else v),
       planner = list(
         study = as.list(p$study[!is.na(p$study)]),
         setup = if (is.na(p$setup)) NULL else p$setup,
         outputs = .cols_df(p$outputs),
         sheets = lapply(p$sheets, .cols_df),
         ard = lapply(p$ard %||% .empty_ard_spec(), .cols_df),
         lf = lapply(p$lf %||% .empty_lf(), .cols_df),
         fig_designs = if (length(p$fig_designs)) p$fig_designs))
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
  if (!is.null(ps$lf)) {
    for (s in names(p$lf)) {
      p$lf[[s]] <- .normalize_lf_sheet(.df_from(ps$lf[[s]], p$lf[[s]]), s)
    }
  }
  p <- .old_program_default(p)
  p$fig_designs <- .fig_designs_from_state(ps$fig_designs)
  if (!is.null(ps$ard)) {
    for (s in names(p$ard)) {
      p$ard[[s]] <- .normalize_ard_sheet(.df_from(ps$ard[[s]], p$ard[[s]]), s)
    }
  }
  .renamed_outputs(p)
}

.read_state <- function(id, home = tflplanner_home()) {
  f <- .state_file(id, home)
  if (!file.exists(f)) return(NULL)
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}

# The state with the fingerprint of each definition file as it is now
# (#274): written after the files, so what is recorded is what is on disk.
# A change of the fingerprints only (a workbook saved again unchanged) is
# written without a history entry.
.write_state <- function(study, home = tflplanner_home()) {
  id <- study$meta$study_id
  dir <- .store_dir(id, home)
  dir.create(file.path(dir, "history"), recursive = TRUE,
             showWarnings = FALSE)
  f <- .state_file(id, home)
  new <- .state_of(study)
  new$files <- .spec_fingerprints(study$path)
  body <- function(x) {
    x$saved <- NULL
    x$tflplanner <- NULL
    x$files <- NULL
    x$format <- NULL
    x
  }
  if (file.exists(f)) {
    old <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    if (identical(.json(body(old)), .json(body(.from_json(.json(new)))))) {
      if (identical(.json(old$files), .json(.from_json(.json(new))$files)) &&
          identical(old$format, new$format)) {
        return(invisible(FALSE))
      }
      new$saved <- old$saved %||% new$saved
      writeLines(enc2utf8(.json(new)), f, useBytes = TRUE)
      return(invisible(FALSE))
    }
    stamp <- gsub("[^0-9]", "", old$saved %||% format(file.mtime(f)))
    file.copy(f, file.path(dir, "history", paste0(stamp, ".json")))
  }
  writeLines(enc2utf8(.json(new)), f, useBytes = TRUE)
  invisible(TRUE)
}

.from_json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)
