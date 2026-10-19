# ============================================================================
#  AI drafts (#431): tflspec's AI table requests and their answers
# ----------------------------------------------------------------------------
#  tflspec writes a study's requests (ai/requests/<request>.md, each pasted
#  into a chat as one message) and reads the chat's answers back
#  (ai/answers/<request>.md -> tfl_ai_collect_answers() -> the report files
#  spec/outputs/<id>.yml).  These functions know no Shiny: the AI drafts
#  tab (R/ai_draft_ui.R) draws what they give.
#
#  Thin: tflspec's functions do the work (tfl_ai_requests(),
#  tfl_ai_save_answer(), tfl_ai_collect_answers(); tflspec 0.0.24.9175);
#  here are what the screen adds -- an answer's file and time, a request's
#  status, the counts, the study read again after a check.
# ============================================================================

# the folders, under the study's
.ai_requests_dir <- file.path("ai", "requests")
.ai_answers_dir <- file.path("ai", "answers")
.ai_repairs_dir <- file.path("ai", "repairs")
# a request's status, worst last (as tflspec's print ranks them)
.ai_status_rank <- c(ok = 1L, warning = 2L, `no answer` = 3L, error = 4L)

.ai_rel <- function(...) gsub("\\\\", "/", file.path(...))

# A text file as one string: UTF-8 (a byte-order mark dropped), "\n" line
# ends.  Text in another encoding (Shift-JIS / CP932, Notepad's "ANSI") is
# an error of class tflplanner_ai_encoding that says how to save it again.
.ai_read_text <- function(f) {
  n <- file.info(f)$size
  raw <- readBin(f, "raw", n = max(n, 0, na.rm = TRUE))
  if (length(raw) >= 3L && identical(raw[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    raw <- raw[-(1:3)]
  }
  bad <- function() {
    structure(class = c("tflplanner_ai_encoding", "error", "condition"),
              list(message = sprintf(paste0(
                "%s is not UTF-8 (it looks like Shift-JIS / CP932, Notepad's \"ANSI\"): ",
                "paste the answer into the box and save it, or save the file again as UTF-8."),
                basename(f)), call = NULL, file = f))
  }
  if (any(raw == as.raw(0L))) stop(bad())
  x <- rawToChar(raw)
  Encoding(x) <- "UTF-8"
  if (!validUTF8(x)) stop(bad())
  gsub("\r\n?", "\n", x)
}

# a request's family from its name (ae-1, ae-1a -> ae)
.ai_family_of <- function(request) sub("-[0-9]+[ab]*$", "", request)

# A study's requests, one row a request: request, family, outputs (the
# report ids, comma-separated), n, chars, file (from the study's folder),
# answer (the answer file, from the study's folder, or NA), answer_time.
# attr "instructions": the instructions file (once) or NA.  No requests: a
# data frame of no rows.
.ai_requests <- function(path) {
  d <- as.data.frame(tflspec::tfl_ai_requests(path), stringsAsFactors = FALSE)
  if (is.null(d$family)) d$family <- rep(NA_character_, nrow(d))
  d$family <- ifelse(is.na(d$family), .ai_family_of(d$request), d$family)
  ans <- vapply(d$request, function(r) .ai_answer_file(path, r), "", USE.NAMES = FALSE)
  d$answer <- ans
  d$answer_time <- file.mtime(as.character(ifelse(is.na(ans), "", file.path(path, ans))))
  # (the workbook a request's reports come from, tflspec >= 0.0.24.9178)
  if (is.null(d$book)) d$book <- rep(NA_character_, nrow(d))
  d <- d[c("request", "family", "outputs", "n", "chars", "file", "answer", "answer_time",
           "book")]
  rownames(d) <- NULL
  ins <- file.path(path, .ai_requests_dir, "00-instructions.md")
  attr(d, "instructions") <- if (file.exists(ins))
    .ai_rel(.ai_requests_dir, "00-instructions.md") else NA_character_
  d
}

# The answer file of a request, from the study's folder: <request>.md, else
# the names Notepad saves it as (<request>.md.txt, <request>.txt); NA when
# there is none.  (tflspec reads them in this order.)
.ai_answer_file <- function(path, request) {
  f <- .ai_rel(.ai_answers_dir, paste0(request, c(".md", ".md.txt", ".txt")))
  f <- f[file.exists(file.path(path, f))]
  if (length(f)) f[1L] else NA_character_
}

# The text of a request's file (or of 00-instructions.md, or of its fix
# request): one string, UTF-8
.ai_request_text <- function(path, request, what = c("request", "repair")) {
  what <- match.arg(what)
  .ai_check_request_name(request)
  dir <- if (identical(what, "repair")) .ai_repairs_dir else .ai_requests_dir
  f <- file.path(path, dir, paste0(request, ".md"))
  if (!file.exists(f)) stop("No file ", .ai_rel(dir, paste0(request, ".md")), call. = FALSE)
  .ai_read_text(f)
}

# The saved answer of a request, for the box it is pasted in: its text,
# "" when there is none.  An answer not in UTF-8: "" with attr "problem".
.ai_answer_text <- function(path, request) {
  f <- .ai_answer_file(path, request)
  if (is.na(f)) return("")
  tryCatch(.ai_read_text(file.path(path, f)),
           tflplanner_ai_encoding = function(e) structure("", problem = conditionMessage(e)))
}

.ai_check_request_name <- function(request) {
  if (!is.character(request) || length(request) != 1L || is.na(request) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", request)) {
    stop("A request's name is letters, digits, '.', '_' and '-' (ae-1).", call. = FALSE)
  }
  invisible(request)
}

# A request's answer saved as ai/answers/<request>.md (UTF-8, LF line
# ends; tfl_ai_save_answer() checks the request is one of the study's).
# Gives the file's path from the study's folder.
.ai_save_answer <- function(path, request, text, overwrite = TRUE) {
  .ai_check_request_name(request)
  # (one line end at the end: the file's own)
  text <- sub("\n+$", "", gsub("\r\n?", "\n", paste(as.character(text), collapse = "\n")))
  f <- tflspec::tfl_ai_save_answer(path, request, text, overwrite = overwrite)
  .ai_rel(.ai_answers_dir, basename(f))
}

# The answers checked: tfl_ai_collect_answers()'s list (reports,
# next_steps, assumptions, stray, requests), with the time of the check
.ai_collect <- function(path) {
  x <- tflspec::tfl_ai_collect_answers(path)
  x$time <- Sys.time()
  x
}

# a request's status: its worst report's (ok < warning < no answer < error);
# NA for a request the check has not seen
.ai_request_status <- function(requests, reports) {
  if (is.null(reports) || !nrow(reports)) return(rep(NA_character_, length(requests)))
  vapply(requests, function(q) {
    s <- reports$status[reports$request == q]
    if (!length(s)) NA_character_ else s[which.max(.ai_status_rank[s])]
  }, "", USE.NAMES = FALSE)
}

# a request's status as the list says it: "error 1", "ok 3", "no answer"
# (`t` the app's words)
.ai_status_label <- function(requests, reports, t = identity) {
  st <- .ai_request_status(requests, reports)
  vapply(seq_along(requests), function(i) {
    if (is.na(st[i])) return(NA_character_)
    if (identical(st[i], "no answer")) return(t(st[i]))
    paste(t(st[i]), sum(reports$request == requests[i] & reports$status == st[i]))
  }, "")
}

# The status line's counts: requests, answers, reports by status
.ai_counts <- function(requests, collected = NULL) {
  r <- collected$reports
  st <- if (is.null(r)) character() else r$status
  list(requests = nrow(requests), answers = sum(!is.na(requests$answer)),
       reports = sum(requests$n),
       status = vapply(names(.ai_status_rank), function(s) sum(st == s), 0L))
}

# The study read again from its definition files after a check wrote its
# reports (spec/outputs/<id>.yml): reload_from_spec() takes them in and
# records their fingerprints, so that no save sees them as made outside the
# app.  Never over unsaved changes: they would have to be merged (the
# Reload dialog does that), an error of class tflplanner_ai_unsaved here.
.ai_reload_study <- function(study_id, unsaved, home = tflplanner_home()) {
  if (isTRUE(unsaved)) {
    stop(structure(class = c("tflplanner_ai_unsaved", "error", "condition"),
                   list(message = "Save first: the study has unsaved changes.", call = NULL)))
  }
  reload_from_spec(study_id, home = home)
}

# ---- step 1: the requests prepared (tfl_ai_prepare()) -----------------------

# the data files of an ADaM folder (their names); NULL for no such folder
.ai_adam_files <- function(dir) {
  dir <- trimws(dir %||% "")
  if (!nzchar(dir) || !dir.exists(dir)) return(NULL)
  sort(list.files(dir, "[.](xpt|sas7bdat|rds)$", ignore.case = TRUE))
}

# The ADaM folder a study's data catalog points to: the folder of its first
# ADaM dataset that is there (a path from the study's folder, or one of its
# own), else the study's data/adam
.ai_adam_default <- function(study) {
  ds <- study$planner$ard$datasets
  path <- study$path
  if (!is.null(ds) && nrow(ds) && !is.null(ds$path)) {
    p <- ds$path[!is.na(ds$path) & nzchar(ds$path)]
    for (f in p) {
      abs <- if (grepl("^(/|[A-Za-z]:|\\\\\\\\)", f)) f else file.path(path, f)
      if (file.exists(abs)) return(normalizePath(dirname(abs), "/"))
    }
  }
  d <- file.path(path, study_layout()[["adam"]])
  if (dir.exists(d)) normalizePath(d, "/") else ""
}

# Why a new study cannot be made under `root` with this id, or NULL (`t`
# the app's words)
.ai_new_study_problem <- function(id, root = studies_root(), home = tflplanner_home(),
                                  t = identity) {
  id <- trimws(id %||% "")
  if (!nzchar(id)) return(t("Give the study an ID."))
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", id)) {
    return(t("Use letters, digits, '.', '_' or '-' only (e.g. ABC-101)."))
  }
  if (id %in% list_studies(home)$study_id) return(t("A study with this ID is already registered."))
  path <- tryCatch(.study_path(root, id), error = function(e) e)
  if (inherits(path, "error")) return(.error_view(conditionMessage(path), t))
  if (dir.exists(path)) {
    return(sprintf(t("A folder %s is already there: choose another ID, or register that folder (Studies > Register a folder)."),
                   path))
  }
  NULL
}

# tfl_ai_prepare() as the screen calls it: no printing, the ADaM folder
# absolute.  `args`: toc_sheet, sheets, size, language, workbook,
# instructions, max_chars, datasets, max_levels, adam_summary, mode, add,
# overwrite (NULL ones left out); `shells` the shell workbooks apart from
# the TOC's.
.ai_prepare <- function(dir, toc, adam, args = list(), dry_run = TRUE, study_id = NULL,
                        title = NULL, shells = NULL) {
  adam <- normalizePath(trimws(adam), "/", mustWork = FALSE)
  if (!dir.exists(adam)) stop(sprintf("No ADaM folder %s.", adam), call. = FALSE)
  a <- c(list(dir = dir, toc = toc, adam = adam, dry_run = dry_run, quiet = TRUE),
         if (!is.null(study_id)) list(study_id = study_id),
         if (!is.null(title) && nzchar(title)) list(title = title),
         if (length(shells)) list(shells = unname(shells)),
         Filter(Negate(is.null), args))
  # (add = TRUE, overwrite = TRUE: the requests asked again alone -- the
  # study's own files and the agent's are kept, tflspec >= 0.0.24.9178)
  # (a dry run on a folder not made yet: its path is not there to normalize)
  suppressWarnings(do.call(tflspec::tfl_ai_prepare, a))
}

# The reports of a dry run already asked for in the study's requests
# (add): output_id, request
.ai_already_asked <- function(path, ids) {
  r <- if (is.null(path)) NULL else tryCatch(tflspec::tfl_ai_requests(path), error = function(e) NULL)
  none <- data.frame(output_id = character(), request = character(), stringsAsFactors = FALSE)
  if (is.null(r) || !nrow(r)) return(none)
  asked <- strsplit(r$outputs, ", ", fixed = TRUE)
  had <- data.frame(output_id = unlist(asked), request = rep(r$request, lengths(asked)),
                    stringsAsFactors = FALSE)
  had[had$output_id %in% ids, , drop = FALSE]
}

# Whether an error of tfl_ai_prepare(add = TRUE) is a report asked for
# already (asked again with overwrite = TRUE)
.ai_is_asked_again <- function(msg) grepl("already asked for", msg, fixed = TRUE)

# The TOC / SHELL workbook kept in the study (ai/<its name>): what was
# read, beside the requests.  Gives the copy's path.
.ai_keep_toc <- function(dir, toc, name = basename(toc)) {
  msg <- .ai_toc_problem(toc)
  if (!is.null(msg)) stop(msg, call. = FALSE)
  to <- file.path(dir, "ai", basename(name))
  if (identical(normalizePath(toc, "/", mustWork = FALSE),
                normalizePath(to, "/", mustWork = FALSE))) return(to)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(suppressWarnings(file.copy(toc, to, overwrite = TRUE)))) {
    stop(sprintf("Could not copy %s to %s: is it open in another program (Excel)?", name, to),
         call. = FALSE)
  }
  to
}

# Why the TOC / SHELL workbook at `path` cannot be read, or NULL (`t` the
# app's words): no such file, not a workbook, or not readable (open in a
# program that locks it)
.ai_toc_problem <- function(path, t = identity) {
  path <- trimws(path %||% "")
  if (!nzchar(path)) return(t("Choose the TOC / SHELL workbook first."))
  if (!file.exists(path) || dir.exists(path)) return(sprintf(t("No file %s."), path))
  if (!grepl("[.]xls[xm]$", path, ignore.case = TRUE)) {
    return(t("The TOC / SHELL workbook is an .xlsx or .xlsm file."))
  }
  ok <- tryCatch({
    con <- file(path, "rb")
    on.exit(close(con))
    length(readBin(con, "raw", 4L)) == 4L
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) return(sprintf(t("%s cannot be read: is it open in another program (Excel)?"), path))
  NULL
}

# The shell workbooks kept in the study (ai/<their names>), as the TOC's:
# `shells` a list of list(path, name).  Gives the copies' paths.
.ai_keep_shells <- function(dir, shells) {
  vapply(shells, function(s) .ai_keep_toc(dir, s$path, s$name), "", USE.NAMES = FALSE)
}

# The requests written into a study open in the app (its study.yml and
# spec/ kept): the workbooks copied into ai/ first.  The tfl_ai_prepared.
.ai_write_requests <- function(path, toc, toc_name, adam, args = list(), shells = list()) {
  toc <- .ai_keep_toc(path, toc, toc_name)
  .ai_prepare(path, toc, adam, args, dry_run = FALSE, shells = .ai_keep_shells(path, shells))
}

# A new study made from the TOC and the ADaM folder, in this order:
# tfl_ai_prepare() writes study.yml, spec/data.yml, spec/defaults.yml and
# the requests (create_study() first would leave no data.yml: its
# study.yml would stop tfl_ai_study_init()), then register_study().
# Gives list(study, prepared).
.ai_new_study <- function(id, toc, toc_name, adam, args = list(), title = NULL,
                          root = studies_root(), home = tflplanner_home(), shells = list()) {
  msg <- .ai_new_study_problem(id, root, home)
  if (!is.null(msg)) stop(msg, call. = FALSE)
  id <- trimws(id)
  dir <- .study_path(root, id)
  p <- tryCatch({
    toc <- .ai_keep_toc(dir, toc, toc_name)
    .ai_prepare(dir, toc, adam, args, dry_run = FALSE, study_id = id, title = title,
                shells = .ai_keep_shells(dir, shells))
  }, error = function(e) {
    # nothing half made is left behind
    unlink(dir, recursive = TRUE)
    stop(e)
  })
  list(study = register_study(dir, home = home), prepared = p)
}

# ---- the computer's own dialogs (Browse...) ----------------------------------

# Whether a dialog to choose a folder or a file can be opened: on Windows
# (utils::choose.dir() / choose.files()), else with tcltk.  A server sets
# options(tflplanner.file_dialog = FALSE): the dialog would open on the
# server, not before the user.
.ai_dialog_ok <- function() {
  if (isFALSE(getOption("tflplanner.file_dialog", TRUE))) return(FALSE)
  .Platform$OS.type == "windows" ||
    (isTRUE(capabilities("tcltk")) && requireNamespace("tcltk", quietly = TRUE))
}

# A folder / a file chosen in the dialog (its path, "/" separated), NA when
# cancelled.  The dialog holds the app's R session while it is open (a
# local app's one user waits on it anyway).  The tests put their own chooser
# in options(tflplanner.choose_dir = , tflplanner.choose_file = ).
.ai_choose_dir <- function(default = "") {
  f <- getOption("tflplanner.choose_dir")
  out <- if (is.function(f)) f(default) else if (.Platform$OS.type == "windows") {
    # (Windows only: looked up, not named, for the check on other systems)
    getExportedValue("utils", "choose.dir")(default = if (dir.exists(default)) default else "",
                                            caption = "ADaM folder")
  } else {
    tcltk::tk_choose.dir(default = default, caption = "ADaM folder")
  }
  .ai_chosen(out)
}

.ai_choose_file <- function(default = "") {
  f <- getOption("tflplanner.choose_file")
  out <- if (is.function(f)) f(default) else if (.Platform$OS.type == "windows") {
    filters <- rbind(c("Excel workbook (*.xlsx, *.xlsm)", "*.xlsx;*.xlsm"), c("All files (*.*)", "*.*"))
    getExportedValue("utils", "choose.files")(default = if (file.exists(default)) default else "",
                                              caption = "TOC / SHELL workbook",
                                              multi = FALSE, filters = filters)
  } else {
    tcltk::tk_choose.files(default = default, caption = "TOC / SHELL workbook", multi = FALSE,
                           filters = matrix(c("Excel workbook", ".xlsx", "Excel workbook", ".xlsm"),
                                            ncol = 2, byrow = TRUE))
  }
  .ai_chosen(out)
}

# Shell workbooks chosen in the dialog (several), their paths; none when
# cancelled
.ai_choose_files <- function(default = "") {
  f <- getOption("tflplanner.choose_files")
  out <- if (is.function(f)) f(default) else if (.Platform$OS.type == "windows") {
    filters <- rbind(c("Excel workbook (*.xlsx, *.xlsm)", "*.xlsx;*.xlsm"), c("All files (*.*)", "*.*"))
    getExportedValue("utils", "choose.files")(default = "", caption = "Shell workbooks",
                                              multi = TRUE, filters = filters)
  } else {
    tcltk::tk_choose.files(default = default, caption = "Shell workbooks", multi = TRUE,
                           filters = matrix(c("Excel workbook", ".xlsx", "Excel workbook", ".xlsm"),
                                            ncol = 2, byrow = TRUE))
  }
  out <- as.character(out)
  out <- out[!is.na(out) & nzchar(out)]
  normalizePath(out, "/", mustWork = FALSE)
}

.ai_chosen <- function(x) {
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(NA_character_)
  normalizePath(x, "/", mustWork = FALSE)
}

# ---- a coding agent (tfl_ai_prepare(mode = "agent")) --------------------------

# A study's mode: "agent" when tfl_ai_prepare(mode = "agent") wrote its
# AGENTS.md, else "chat"
.ai_mode <- function(path) {
  if (!is.null(path) && file.exists(file.path(path, "AGENTS.md"))) "agent" else "chat"
}

# The agent's first message, as tfl_ai_prepare(mode = "agent") gives it
# (its `agent_prompt`): ai/agent_prompt.md, which it writes too; made again
# from the study's folder when there is none (a study of an older tflspec)
.ai_agent_prompt <- function(path) {
  f <- file.path(path, "ai", "agent_prompt.md")
  if (file.exists(f)) {
    txt <- tryCatch(.ai_read_text(f), error = function(e) NULL)
    if (!is.null(txt)) return(sub("\n+$", "", txt))
  }
  at <- normalizePath(path, winslash = "/", mustWork = FALSE)
  paste0("Follow ", at, "/AGENTS.md. Do every request of ", at, "/ai/requests/index.md, ",
         "in its order: save each answer as ", at, "/ai/answers/<request>.md and run ",
         "`Rscript ", at, "/run.R` after it, then read ", at, "/ai/status.md and repair. ",
         "Do not stop to ask; when every request is done, tell me which reports have an ",
         "RTF and that ", at, "/ai/questions.md waits for my answers.")
}

# The questions of ai/questions.md (tfl_ai_questions()), the open ones
# first: id, output_id, where, question, provisional, answer, status
.ai_questions <- function(path) {
  q <- tflspec::tfl_ai_questions(path)
  q[order(q$status == "decided"), , drop = FALSE]
}

# Answers (named by the questions' ids) written into ai/questions.md
.ai_answer_questions <- function(path, answers) {
  tflspec::tfl_ai_answer_questions(path, answers)
}
