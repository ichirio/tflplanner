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
  d <- d[c("request", "family", "outputs", "n", "chars", "file", "answer", "answer_time")]
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
.ai_status_label <- function(requests, reports) {
  st <- .ai_request_status(requests, reports)
  vapply(seq_along(requests), function(i) {
    if (is.na(st[i])) return(NA_character_)
    if (identical(st[i], "no answer")) return(st[i])
    paste(st[i], sum(reports$request == requests[i] & reports$status == st[i]))
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
