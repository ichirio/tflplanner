# The app's own home: where tflplanner keeps what it knows about each study,
# apart from the study folders themselves.
#
#   <home>/
#     config.yml                 studies_root (where new study folders go),
#                                last_study
#     state/<STUDY_ID>/
#       state.json               the study as last saved: study.yml fields,
#                                its folder, every sheet, the report list,
#                                the data code -- a copy of the study
#                                folder's definition files, with each file's
#                                fingerprint (#274)
#       history/<time>.json      each earlier saved state
#       draft.json, ard/, facts/ unsaved changes; what input assistance and
#                                the review read
#     studies/                   a new home's study folders (studies_root)
#
# The home and the study folders: the home holds them (<home>/studies, a
# new home's) or sits beside them (C:/tflplanner_home and C:/studies), never
# inside them -- tidying the studies would delete it (#359).  Before
# 0.0.2.9200 the state was at <home>/studies/<STUDY_ID>: moved on first use.
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
#'   the current setting, or on a first setup uses `<home>/studies`.  The
#'   home holds the study folders or sits beside them, never inside them
#'   (nor inside a study's folder): such a setting is refused.
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
#' # the whole layout in one call: C:/tflplanner_home, its study folders in
#' # C:/tflplanner_home/studies, the sample study among them
#' setup_tflplanner(home = "C:/tflplanner_home", sample = TRUE)
#' # or the study folders beside the home
#' setup_tflplanner(home = "C:/tflplanner_home", studies_root = "C:/studies")
#' }
#' @export
setup_tflplanner <- function(home = NULL, studies_root = NULL,
                             language = NULL, standards = NULL,
                             sample = FALSE, port = NULL,
                             check_updates = NULL) {
  if (nargs() == 0L && interactive()) return(.setup_wizard())
  if (!is.null(port)) port <- .check_port(port)
  # the home and the study folders: never the home inside the studies
  # (checked when either is set, or on a first setup)
  h <- if (is.null(home)) tflplanner_home() else normalizePath(home, "/", mustWork = FALSE)
  if (!is.null(home) || !is.null(studies_root) || !.is_set_up(h)) {
    root <- studies_root %||% tflplanner_config(h)$studies_root %||% file.path(h, "studies")
    problem <- .home_problem(h, root)
    if (!is.null(problem)) stop(problem, " ", .home_layouts, call. = FALSE)
  }
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
  dir.create(file.path(home, "state"), recursive = TRUE,
             showWarnings = FALSE)
  cfg <- tflplanner_config(home)
  if (!is.null(studies_root)) cfg$studies_root <- studies_root
  if (is.null(cfg$studies_root) || !nzchar(cfg$studies_root)) {
    cfg$studies_root <- file.path(home, "studies")
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
  if (is.null(r)) file.path(home, "studies") else r
}

# ------------------------------------------------- the home and the studies

# Is `path` the folder `root`, or inside it?
.under <- function(path, root) {
  p <- .real_path(path)
  r <- sub("/$", "", .real_path(root))
  identical(p, r) || startsWith(p, paste0(r, "/"))
}

# The study folder (one with a study.yml) a path is in, or NA
.study_folder_above <- function(path) {
  p <- normalizePath(path, "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(p, "study.yml"))) return(p)
    up <- dirname(p)
    if (identical(up, p)) return(NA_character_)
    p <- up
  }
}

# What is wrong with a home and the study folders, or NULL: the home is
# never inside the studies (tidying them would delete it), nor inside a
# study's folder (`t`: the app's words)
.home_problem <- function(home, root, t = identity) {
  if (!is.null(root) && nzchar(root) && .under(home, root)) {
    return(sprintf(t("The home %s is inside %s, where new study folders go: tidying the studies would delete it."),
                   home, normalizePath(root, "/", mustWork = FALSE)))
  }
  s <- .study_folder_above(home)
  if (!is.na(s)) return(sprintf(t("The home %s is inside the study folder %s."), home, s))
  NULL
}
.home_layouts <- paste("Keep the study folders inside the home (<home>/studies)",
                       "or beside it (C:/tflplanner_home and C:/studies).")

# A new study's folder: <root>/<id>, the root checked -- never one the home
# is in, nor one inside a study's folder -- and joined so that a drive's
# root gives C:/ABC-101 (file.path("C:/", id) is "C://ABC-101").  A blank
# root is the one set up.
.study_path <- function(root, id, home = tflplanner_home()) {
  root <- trimws(root %||% "")
  if (!nzchar(root)) root <- studies_root(home)
  root <- sub("[/\\\\]+$", "", root)
  # (a drive alone, "C:", is its root, not the drive's current folder)
  check <- if (!nzchar(root) || grepl("^[A-Za-z]:$", root)) paste0(root, "/") else root
  if (.under(home, check)) {
    stop(sprintf("A study folder cannot go in %s: the home %s is inside it.", check, home),
         call. = FALSE)
  }
  s <- .study_folder_above(check)
  if (!is.na(s)) {
    stop(sprintf("A study folder cannot go in %s: it is inside the study folder %s.", check, s),
         call. = FALSE)
  }
  file.path(if (nzchar(root)) root else "", id)
}

# A folder made (with the folders above it), or an error that says which
# and why -- not a later "cannot open the connection"
.make_dir <- function(path) {
  why <- NULL
  withCallingHandlers(
    dir.create(path, recursive = TRUE, showWarnings = TRUE),
    warning = function(w) {
      why <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    })
  if (!dir.exists(path)) {
    stop(sprintf("Could not make the folder %s (%s).", path,
                 sub("^cannot create dir '[^']*', reason '(.*)'$", "\\1", why %||% "no reason given")),
         call. = FALSE)
  }
  invisible(path)
}

# ------------------------------------------------------------- the store

.store_dir <- function(id, home = tflplanner_home()) {
  .move_old_stores(home)
  file.path(home, "state", id)
}

# Before 0.0.2.9200 a study's state was at <home>/studies/<ID>, the name a
# new home's study folders have now: moved to <home>/state/<ID>, once a
# session -- moved, never deleted; an item already at the new place stays
# where it was (said); a folder left empty goes.  A study folder that was
# its own state's folder (studies_root at <home>/studies) keeps its files.
.moved_homes <- new.env()
.move_old_stores <- function(home) {
  key <- .real_path(home)
  if (isTRUE(.moved_homes[[key]])) return(invisible())
  .moved_homes[[key]] <- TRUE
  old <- file.path(home, "studies")
  if (!dir.exists(old)) return(invisible())
  items <- c("state.json", "draft.json", "history", "ard", "facts")
  for (d in list.dirs(old, recursive = FALSE)) {
    if (!file.exists(file.path(d, "state.json"))) next
    to <- file.path(home, "state", basename(d))
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    for (f in intersect(items, list.files(d))) {
      if (file.exists(file.path(to, f))) {
        message("tflplanner: ", file.path(d, f), " is not moved: ",
                file.path(to, f), " is already there.")
        next
      }
      file.rename(file.path(d, f), file.path(to, f))
    }
    if (!length(list.files(d, all.files = TRUE, no.. = TRUE))) unlink(d, recursive = TRUE)
  }
  if (!length(list.files(old, all.files = TRUE, no.. = TRUE)) &&
      !identical(.real_path(old), .real_path(studies_root(home)))) {
    unlink(old, recursive = TRUE)
  }
  invisible()
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
  st <- .state_of(study)
  st$spec <- .draft_spec_of(study)
  writeLines(enc2utf8(.json(st)), f, useBytes = TRUE)
  invisible(f)
}

# A draft read back: the study with its sheets, and with its spec when
# the draft holds one (`planner$spec`; NULL: the study folder's)
.read_draft <- function(id, home = tflplanner_home()) {
  f <- .draft_file(id, home)
  if (!file.exists(f)) return(NULL)
  st <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE),
                 error = function(e) NULL)
  if (is.null(st)) return(NULL)
  d <- .study_from_state(st)
  if (!is.null(st$spec)) {
    d$planner$spec <- tryCatch(.draft_spec_back(st$spec, d$path), error = function(e) NULL)
  }
  d
}

# The spec's part of a draft (#415): the spec's files whose text differs
# from the study folder's (study.yml: but for the time it was updated),
# the folder's files the spec no longer has, and the view and the study's
# fields the spec was last in step with -- so that what only the spec
# holds comes back with the draft.  NULL: the spec is the folder's.
.draft_spec_of <- function(study) {
  sp <- study$planner$spec
  root <- study$path
  if (is.null(sp) || is.null(sp$base) || is.null(root) || !dir.exists(root)) return(NULL)
  drop <- function(r, x) if (identical(r, .study_file)) x[!startsWith(x, "updated:")] else x
  disk <- function(r) {
    f <- file.path(root, r)
    if (file.exists(f)) readLines(f, encoding = "UTF-8", warn = FALSE)
  }
  changed <- Filter(function(r) !identical(drop(r, disk(r)), drop(r, sp$text[[r]])),
                    names(sp$text))
  gone <- setdiff(.spec_files_yaml(root), names(sp$text))
  if (!length(changed) && !length(gone)) return(NULL)
  list(text = sp$text[changed], gone = gone, base = .planner_state(sp$base),
       meta = lapply(sp$meta, function(v) if (is.na(v)) NULL else v))
}

# The spec of a draft: the study folder's files with the draft's put in,
# in step with the view the draft says
.draft_spec_back <- function(x, root) {
  sp <- .spec_load(root)
  text <- if (is.null(sp)) list() else sp$text
  for (r in names(x$text)) text[[r]] <- as.character(unlist(x$text[[r]]))
  text[intersect(as.character(unlist(x$gone)), names(text))] <- NULL
  tmp <- tempfile("tflplanner-draft")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  for (r in names(text)) {
    dir.create(dirname(file.path(tmp, r)), recursive = TRUE, showWarnings = FALSE)
    writeLines(text[[r]], file.path(tmp, r), useBytes = TRUE)
  }
  new <- .spec_load(tmp)
  if (is.null(new)) return(NULL)
  new$study$dir <- if (is.null(sp)) root else sp$study$dir
  new$base <- .planner_body(.study_spec_keys(.planner_from_state(list(planner = x$base))))
  new$meta <- .spec_meta(x$meta)
  new
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
  list(format = 2L,
       tflplanner = as.character(utils::packageVersion("tflplanner")),
       saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
       path = study$path,
       meta = lapply(study$meta[.study_fields], function(v)
         if (is.na(v)) NULL else v),
       planner = .planner_state(study$planner))
}

# A planner's sheets as the state keeps them (.planner_from_state() reads
# them back)
.planner_state <- function(p) {
  list(study = as.list(p$study[!is.na(p$study)]),
       setup = if (is.na(p$setup)) NULL else p$setup,
       outputs = .cols_df(p$outputs),
       sheets = lapply(p$sheets, .cols_df),
       ard = lapply(p$ard %||% .empty_ard_spec(), .cols_df),
       lf = lapply(p$lf %||% .empty_lf(), .cols_df),
       fig_designs = if (length(p$fig_designs)) .fig_r_to_state(p$fig_designs))
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
  # (the report list's data code before 0.0.2.9206: a step's code now)
  old <- .old_data_code(.df_from(ps$outputs, data.frame(output_id = character(),
                                                        data_code = character())))
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
  p <- .migrate_step_code(p, old)
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
  on.exit(.state_remember(study, home), add = TRUE)
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
  # (the new state as JSON once, and as read back once)
  nj <- .json(new)
  if (file.exists(f)) {
    old <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    back <- .from_json(nj)
    # (both as read from JSON: compared as they are, not written again)
    if (identical(body(old), body(back))) {
      if (identical(old$files, back$files) &&
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
  writeLines(enc2utf8(nj), f, useBytes = TRUE)
  invisible(TRUE)
}

.from_json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)

# The state file as this process last wrote it, and the planner it wrote it
# from (.merge_saved(): a save from that planner with the file unchanged
# since has nothing to merge)
.state_seen <- new.env()
.state_remember <- function(study, home) {
  f <- .state_file(study$meta$study_id, home)
  if (file.exists(f)) {
    .state_seen[[f]] <- list(md5 = unname(tools::md5sum(f)), planner = study$planner)
  }
}
