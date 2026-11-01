# ============================================================================
#  AI drafts (#431): tflspec's AI requests and the agent's SPEC drafts
# ----------------------------------------------------------------------------
#  tflspec writes a study's requests (ai/requests/<request>.md) and the
#  agent's first message; the agent asks in its chat what it cannot decide
#  and writes each report's draft (ai/spec/outputs/<id>.yml), which
#  tfl_ai_collect_answers() checks.  These functions know no Shiny: the AI
#  tab (R/ai_draft_ui.R) draws what they give.
#
#  Thin: tflspec's functions do the work (tfl_ai_requests(),
#  tfl_ai_collect_answers(), tfl_ai_confirm()); here are what the screen
#  adds -- a request's drafts and their time, its status, the counts, the
#  study read again after a check.
# ============================================================================

# the folders, under the study's
.ai_requests_dir <- file.path("ai", "requests")
.ai_drafts_dir <- file.path("ai", "spec", "outputs")
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
# drafts (how many of its reports have a draft), draft_time (the newest).
# No requests: a data frame of no rows.
.ai_requests <- function(path) {
  d <- as.data.frame(tflspec::tfl_ai_requests(path), stringsAsFactors = FALSE)
  if (is.null(d$family)) d$family <- rep(NA_character_, nrow(d))
  d$family <- ifelse(is.na(d$family), .ai_family_of(d$request), d$family)
  dr <- lapply(d$outputs, function(o) .ai_request_drafts(path, o))
  d$drafts <- vapply(dr, function(x) x$n, 0L)
  d$draft_time <- do.call(c, c(list(as.POSIXct(character())), lapply(dr, function(x) x$time)))
  # (the workbook a request's reports come from)
  if (is.null(d$book)) d$book <- rep(NA_character_, nrow(d))
  d <- d[c("request", "family", "outputs", "n", "chars", "file", "drafts", "draft_time", "book")]
  rownames(d) <- NULL
  d
}

# A request's drafts: how many of its reports (`outputs`, comma-separated)
# have one (ai/spec/outputs/<id>.yml), and when the newest was written
.ai_request_drafts <- function(path, outputs) {
  ids <- trimws(strsplit(outputs %||% "", ",", fixed = TRUE)[[1L]])
  ids <- ids[nzchar(ids)]
  f <- file.path(path, .ai_drafts_dir, paste0(ids, ".yml"))
  have <- file.exists(f)
  list(n = sum(have), of = length(ids),
       time = if (any(have)) max(file.mtime(f[have])) else as.POSIXct(NA))
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

# The reports that passed their checks but could not be built, from what
# tfl_ai_run() writes: ai/repairs/<request>-build.md
# (run.R removes it once the request's reports are built; a check leaves it),
# else the "## Could not be built" section of ai/repairs/<request>.md
# (an older run.R's), one "- <output_id>: <reason>" a line.
# A data frame (request, output_id, reason, from: "build" or "repair") with
# attr "texts" (each old-format request's whole fix request, named by
# request: a later check removes that file of an answer that passes, so the
# app keeps them) and attr "build" (the requests with a -build.md).  A study
# with neither file: no rows.
.ai_unbuilt <- function(path) {
  none <- data.frame(request = character(), output_id = character(), reason = character(),
                     from = character(), stringsAsFactors = FALSE)
  fs <- list.files(file.path(path, .ai_repairs_dir), "[.]md$", full.names = TRUE)
  is_build <- grepl("-build[.]md$", basename(fs))
  build <- sub("-build[.]md$", "", basename(fs[is_build]))
  rows <- list()
  texts <- character()
  for (f in fs) {
    from <- if (grepl("-build[.]md$", basename(f))) "build" else "repair"
    q <- sub(if (from == "build") "-build[.]md$" else "[.]md$", "", basename(f))
    # the separate file first
    if (from == "repair" && q %in% build) next
    txt <- tryCatch(.ai_read_text(f), error = function(e) NA_character_)
    if (is.na(txt)) next
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
    at <- which(trimws(lines) == "## Could not be built")
    if (!length(at)) next
    if (from == "repair") texts[[q]] <- txt
    body <- lines[-seq_len(at[1L])]
    nxt <- grep("^#{1,2} ", body)
    if (length(nxt)) body <- body[seq_len(nxt[1L] - 1L)]
    body <- grep("^- [^:]+: ", body, value = TRUE)
    if (!length(body)) next
    rows[[q]] <- data.frame(request = q, output_id = sub("^- ([^:]+): .*$", "\\1", body),
                            reason = sub("^- [^:]+: ", "", body), from = from,
                            stringsAsFactors = FALSE)
  }
  out <- if (length(rows)) do.call(rbind, unname(rows)) else none
  rownames(out) <- NULL
  attr(out, "texts") <- texts
  attr(out, "build") <- intersect(build, out$request)
  out
}

.ai_check_request_name <- function(request) {
  if (!is.character(request) || length(request) != 1L || is.na(request) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", request)) {
    stop("A request's name is letters, digits, '.', '_' and '-' (ae-1, t.15.1.5.1).", call. = FALSE)
  }
  invisible(request)
}

# The answers checked: tfl_ai_collect_answers()'s list (reports,
# next_steps, assumptions, stray, requests), with the time of the check
.ai_collect <- function(path) {
  .ai_ensure_area(path)
  x <- tflspec::tfl_ai_collect_answers(path)
  x$time <- Sys.time()
  # what is to confirm (the agent could not decide), the drafts' and the
  # study's: output_id, where, item, why, file
  x$confirm <- .ai_confirm(path)
  # each draft's check, ERROR / WARNING / OK, and the fix text of those
  # that are not OK (tflspec::tfl_check_spec() on ai/spec/outputs)
  x$checks <- .ai_check_drafts(path)
  x
}

# The drafts checked as the user's three levels say it: ERROR (the SPEC
# does not read: it cannot be copied), WARNING (it reads, no program can be
# made from it yet), OK; what is to confirm apart.  A list: `reports`
# (tflspec::tfl_check_reports(): output_id, level, errors, warnings, notes,
# confirms, confirm) and `fix` (the text for the agent's chat of each ERROR /
# WARNING report, named by output_id, in the study's language); NULL when
# tflspec cannot check
.ai_check_drafts <- function(path) {
  old <- options(tflspec.outputs_dir = file.path("ai", "spec", "outputs"))
  on.exit(options(old), add = TRUE)
  probs <- tryCatch(suppressMessages(suppressWarnings(tflspec::tfl_check_spec(path))),
                    error = function(e) NULL)
  if (is.null(probs)) return(NULL)
  reports <- tryCatch(tflspec::tfl_check_reports(probs), error = function(e) NULL)
  fix <- tryCatch(tflspec::tfl_check_fix_text(probs, language = .ai_fix_language(path)),
                  error = function(e) character())
  list(reports = reports, fix = fix)
}

# The fix text's language: the study's (study.yml's ai: language), "ja" for
# Japanese, else "en"
.ai_fix_language <- function(path) {
  y <- tryCatch(yaml::read_yaml(file.path(path, "study.yml")), error = function(e) NULL)
  l <- tolower(as.character(y$ai$language %||% "")[1L])
  if (!is.na(l) && l %in% c("ja", "japanese", "jp")) "ja" else "en"
}

# The places a report's SPEC leaves to confirm (tflspec::tfl_ai_confirm()):
# its drafts' and the adopted ones'; NULL when tflspec cannot say
.ai_confirm <- function(path) {
  d <- tryCatch(tflspec::tfl_ai_confirm(path, where = "both"), error = function(e) NULL)
  if (!is.data.frame(d)) return(NULL)
  for (k in c("output_id", "where", "item", "why", "file")) {
    if (is.null(d[[k]])) d[[k]] <- rep(NA_character_, nrow(d))
  }
  d
}

# A check's three levels as the user says them (the collect's status):
# ERROR the SPEC does not read; WARNING it reads, no program can be made
# from it yet; OK a program can be made.  "no answer": no draft yet.
.ai_check_level <- function(status) {
  unname(c(ok = "OK", warning = "WARNING", error = "ERROR", `no answer` = "no draft")[status])
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
  list(requests = nrow(requests), drafts = sum(requests$drafts),
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
    # (a path of its own, or from the study's folder: .data_file())
    for (f in .data_file(path, p)) {
      if (file.exists(f)) return(normalizePath(dirname(f), "/"))
    }
  }
  d <- file.path(path, study_layout()[["adam"]])
  if (dir.exists(d)) normalizePath(d, "/") else ""
}

# The sheets of the TOC workbook and of the shell workbooks, their names
# only (readxl::excel_sheets(), not their cells), named as
# tfl_ai_read_shell() names them: a shell workbook's sheet whose name is
# taken already is "<sheet> [<workbook>]".  sheet, book (the file's name),
# toc (in the TOC workbook); none for a file that cannot be read.
.ai_sheet_names <- function(toc = NULL, shells = character()) {
  read <- function(f) {
    if (!is.character(f) || length(f) != 1L || is.na(f) || !file.exists(f)) return(character())
    tryCatch(readxl::excel_sheets(f), error = function(e) character())
  }
  sheet <- read(toc %||% NA_character_)
  out <- data.frame(sheet = sheet, book = rep(basename(toc %||% ""), length(sheet)),
                    toc = rep(TRUE, length(sheet)), stringsAsFactors = FALSE)
  for (f in shells) {
    s <- read(f)
    s <- ifelse(s %in% out$sheet, sprintf("%s [%s]", s, basename(f)), s)
    out <- rbind(out, data.frame(sheet = s, book = rep(basename(f), length(s)),
                                 toc = rep(FALSE, length(s)), stringsAsFactors = FALSE))
  }
  out
}

# The sheets' labels in the lists: with their workbook when there are
# several ("Safety [more.xlsx]" named so already)
.ai_sheet_labels <- function(sheet, book = NULL) {
  if (is.null(book) || length(unique(book)) < 2L) return(sheet)
  ifelse(grepl("\\]$", sheet), sheet, sprintf("%s [%s]", sheet, book))
}

# The requests a dry run would write at another `size`, from its TOC
# (tfl_ai_prepared$toc): its tables in batches (tfl_ai_batches()), each
# figure a request of its own -- as tfl_ai_prepare(dry_run = TRUE) counts
# them.  request, n.
.ai_dry_requests <- function(toc, size) {
  # (the tables: tfl_ai_batches()'s types = "table"; each figure apart)
  b <- if (nrow(toc)) tflspec::tfl_ai_batches(toc, size = size) else
    data.frame(output_id = character(), batch = character())
  figs <- toc$output_id[toc$type %in% "figure"]
  b <- b[!b$output_id %in% figs, , drop = FALSE]
  bt <- c(unique(b$batch), if (length(figs)) paste0("figure-", seq_along(figs)))
  n <- c(as.integer(table(factor(b$batch, unique(b$batch)))), rep(1L, length(figs)))
  # (one report a request: named after it, as tfl_ai_write_requests() does)
  if (isTRUE(as.integer(size) == 1L)) bt <- .ai_one_names(c(b$output_id, figs))
  data.frame(request = bt, n = n, stringsAsFactors = FALSE)
}

# The requests' names when each holds one report (size = 1): the report's number (t.15.1.5.1), what a file name cannot
# hold made "-", a name twice made unique ("-1") -- the tables', then the
# figures', in order
.ai_one_names <- function(ids) {
  x <- trimws(as.character(ids))
  y <- gsub("^-+|-+$", "", gsub("[^A-Za-z0-9._-]+", "-", x))
  make.unique(ifelse(grepl("^[A-Za-z0-9._-]+$", x), x, y), sep = "-")
}

# Said when what the read was for has changed (the workbooks, their sheets)
.ai_stale_msg <- "The workbook or its sheets changed: press \"Read\" again to count the requests."

# The reports a request holds by default: one for a coding agent (it
# answers, checks and fixes each report in turn), tflspec's 3 for a chat
.ai_size_default <- function(mode = "chat") if (identical(mode, "agent")) 1 else 3

# The ADaM folder put in step 1's field: the open study's
# (.ai_adam_default()); for a new study (`study` NULL),
# studies_root/<new_id>/data/adam when it is there; else "" (never another
# study's folder)
.ai_adam_auto <- function(study = NULL, new_id = "", root = studies_root()) {
  if (!is.null(study)) return(.ai_adam_default(study))
  id <- trimws(as.character(new_id %||% ""))
  if (length(id) != 1L || is.na(id) || !nzchar(id) || grepl("[/\\\\]", id) ||
      id %in% c(".", "..")) {
    return("")
  }
  d <- file.path(root, id, study_layout()[["adam"]])
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
  # (a folder with no study.yml yet -- the ADaM put in its data/adam first --
  # becomes the study; one that is a study is registered instead)
  if (file.exists(file.path(path, "study.yml"))) {
    return(sprintf(t("A folder %s is already there: choose another ID, or register that folder (Studies > Register a folder)."),
                   path))
  }
  NULL
}

# Whether the file `path` is inside the study folder `dir` (any depth)
.ai_in_study <- function(path, dir) {
  if (!dir.exists(dir) || !file.exists(path)) return(FALSE)
  d <- sub("/+$", "", normalizePath(dir, "/"))
  startsWith(tolower(normalizePath(path, "/")), paste0(tolower(d), "/"))
}

# An ADaM folder inside the study folder, written relative to it
# (`data/adam`): the study's files then hold no path of this computer, so
# the folder copied elsewhere still finds its data (tflspec reads a relative
# `adam` from `dir`); a folder outside stays absolute.
.ai_adam_in_study <- function(adam, dir) {
  d <- sub("/+$", "", normalizePath(dir, "/", mustWork = FALSE))
  a <- sub("/+$", "", adam)
  inside <- startsWith(tolower(a), paste0(tolower(d), "/"))
  if (inside) substring(a, nchar(d) + 2L) else adam
}

# tfl_ai_prepare() as the screen calls it: no printing, the ADaM folder
# absolute.  `args`: toc_sheet, sheets, size, language, workbook,
# instructions, max_chars, datasets, max_levels, adam_summary, mode, templates, add,
# overwrite (NULL ones left out); `shells` the shell workbooks apart from
# the TOC's.
.ai_prepare <- function(dir, toc, adam, args = list(), dry_run = TRUE, study_id = NULL,
                        title = NULL, shells = NULL) {
  adam <- normalizePath(trimws(adam), "/", mustWork = FALSE)
  if (!dir.exists(adam)) stop(sprintf("No ADaM folder %s.", adam), call. = FALSE)
  adam <- .ai_adam_in_study(adam, dir)
  a <- c(list(dir = dir, toc = toc, adam = adam, dry_run = dry_run, quiet = TRUE),
         if (!is.null(study_id)) list(study_id = study_id),
         if (!is.null(title) && nzchar(title)) list(title = title),
         if (length(shells)) list(shells = unname(shells)),
         Filter(Negate(is.null), args),
         if (!isTRUE(dry_run) && is.null(args$area)) list(area = TRUE))
  # (the AI's drafts in their area, ai/spec/outputs, always -- for a study
  # with reports of its own too, whose files the AI must not write over;
  # step 4 lists the drafts and adopts them into spec/outputs)
  # (add = TRUE, overwrite = TRUE: the requests asked again alone -- the
  # study's own files and the agent's are kept)
  # (a dry run on a folder not made yet: its path is not there to normalize)
  # (a study answered before: its drafts put apart first, adopted as they are)
  if (!isTRUE(dry_run)) .ai_ensure_area(dir)
  p <- suppressWarnings(do.call(tflspec::tfl_ai_prepare, a))
  p
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

# A workbook chosen with Browse... for a study (`dir`, its folder): inside
# the study, its own path; outside it, copied into the study's docs/ (made
# when missing) and the copy's path -- the study then holds what it is read
# from.  No study folder yet (`dir` NULL or missing): the path as it is.
# A copy of another file of that name already in docs/ stops.
.ai_book_into_docs <- function(f, dir) {
  if (is.null(dir) || !nzchar(dir) || !dir.exists(dir) || !file.exists(f)) return(f)
  if (.ai_in_study(f, dir)) return(normalizePath(f, "/"))
  to <- file.path(dir, "docs", basename(f))
  if (file.exists(to)) {
    same <- identical(unname(tools::md5sum(f)), unname(tools::md5sum(to)))
    if (same) return(normalizePath(to, "/"))
    stop(sprintf("The study's docs/ has another %s already: rename one, or choose the one in docs/.",
                 basename(f)), call. = FALSE)
  }
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(file.copy(f, to))) {
    stop(sprintf("Could not copy %s into %s: is it open in another program (Excel)?",
                 basename(f), dirname(to)), call. = FALSE)
  }
  normalizePath(to, "/")
}

# The TOC / SHELL workbook kept in the study (docs/<its name>, where
# Browse... puts one too: .ai_book_into_docs()): what was read.  Gives the
# copy's path.  A workbook already inside the study folder (its docs/, say)
# is not copied: its own path (tflspec writes it from the study folder).
# Another file of that name already in docs/ stops, as for Browse....
.ai_keep_toc <- function(dir, toc, name = basename(toc)) {
  msg <- .ai_toc_problem(toc)
  if (!is.null(msg)) stop(msg, call. = FALSE)
  if (.ai_in_study(toc, dir)) return(normalizePath(toc, "/"))
  to <- file.path(dir, "docs", basename(name))
  if (identical(normalizePath(toc, "/", mustWork = FALSE),
                normalizePath(to, "/", mustWork = FALSE))) return(to)
  if (file.exists(to)) {
    if (identical(unname(tools::md5sum(toc)), unname(tools::md5sum(to)))) return(to)
    stop(sprintf("The study's docs/ has another %s already: rename one, or choose the one in docs/.",
                 basename(name)), call. = FALSE)
  }
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(suppressWarnings(file.copy(toc, to)))) {
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
# spec/ kept): the workbooks copied into docs/ first.  The tfl_ai_prepared.
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
  # what the folder held before (its data/adam put there first): kept when
  # making the study fails; only what this call wrote is taken away
  before <- if (dir.exists(dir)) list.files(dir, all.files = TRUE, no.. = TRUE) else NULL
  p <- tryCatch({
    toc <- .ai_keep_toc(dir, toc, toc_name)
    .ai_prepare(dir, toc, adam, args, dry_run = FALSE, study_id = id, title = title,
                shells = .ai_keep_shells(dir, shells))
  }, error = function(e) {
    # nothing half made is left behind -- but what was there stays
    if (is.null(before)) {
      unlink(dir, recursive = TRUE)
    } else {
      made <- setdiff(list.files(dir, all.files = TRUE, no.. = TRUE), before)
      unlink(file.path(dir, made), recursive = TRUE)
    }
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

# Where Browse... opens (a folder, "/" separated; "" when none is there):
# the folder of the path already given; else the study's folder (the open
# study's, or for a new one studies_root/<ID> when it is there, else
# studies_root) -- its data/adam for the ADaM folder, its docs for the
# workbooks, when there
.ai_browse_start <- function(kind = c("adam", "toc", "shells"), field = "", study_dir = NULL,
                             root = studies_root(), new_id = "") {
  kind <- match.arg(kind)
  here <- function(d) {
    d <- as.character(d)
    length(d) == 1L && !is.na(d) && nzchar(d) && dir.exists(d)
  }
  done <- function(d) normalizePath(d, "/", mustWork = FALSE)
  given <- trimws(unlist(strsplit(as.character(field %||% ""), "\n", fixed = TRUE)))
  given <- gsub('^"|"$', "", given[nzchar(given)])
  for (g in rev(given)) {
    if (here(g)) return(done(g))
    if (file.exists(g) && here(dirname(g))) return(done(dirname(g)))
  }
  id <- trimws(new_id %||% "")
  study <- if (here(study_dir)) {
    study_dir
  } else if (length(id) == 1L && !is.na(id) && nzchar(id) && !grepl("[/\\\\]", id) &&
             here(file.path(root, id))) {
    file.path(root, id)
  } else if (here(root)) {
    root
  }
  if (is.null(study)) return("")
  sub <- file.path(study, if (identical(kind, "adam")) study_layout()[["adam"]] else "docs")
  done(if (here(sub)) sub else study)
}

# What a file dialog is given to open in `start`: a file, itself; a folder,
# on Windows the folder's mask ("C:\\dir\\*.*", see ?choose.files), with
# tcltk a name in it (tk_choose.files() opens at its dirname())
.ai_file_default <- function(start, windows = .Platform$OS.type == "windows") {
  start <- as.character(start %||% "")
  if (length(start) != 1L || is.na(start) || !nzchar(start)) return("")
  if (dir.exists(start)) {
    if (windows) paste0(normalizePath(start, "\\", mustWork = FALSE), "\\*.*") else
      file.path(start, "*.xlsx")
  } else if (file.exists(start)) {
    if (windows) normalizePath(start, "\\", mustWork = FALSE) else start
  } else {
    ""
  }
}

# A folder / a file chosen in the dialog (its path, "/" separated), NA when
# cancelled.  The dialog holds the app's R session while it is open (a
# local app's one user waits on it anyway).  The tests put their own chooser
# in options(tflplanner.choose_dir = , tflplanner.choose_file = ).
.ai_choose_dir <- function(default = "") {
  f <- getOption("tflplanner.choose_dir")
  out <- if (is.function(f)) f(default) else if (.Platform$OS.type == "windows") {
    # (in front of the browser: R/ai_dialog_win.R; else utils's)
    .ps_dialog("dir", default, "ADaM folder") %||% .ai_dialog_try(
    # (Windows only: looked up, not named, for the check on other systems)
    getExportedValue("utils", "choose.dir")(default = if (dir.exists(default)) {
      normalizePath(default, "\\", mustWork = FALSE)
    } else "",
                                            caption = "ADaM folder"))
  } else {
    tcltk::tk_choose.dir(default = default, caption = "ADaM folder")
  }
  .ai_chosen(out)
}

.ai_choose_file <- function(default = "") {
  f <- getOption("tflplanner.choose_file")
  out <- if (is.function(f)) f(default) else if (.Platform$OS.type == "windows") {
    filters <- rbind(c("Excel workbook (*.xlsx, *.xlsm)", "*.xlsx;*.xlsm"), c("All files (*.*)", "*.*"))
    .ps_dialog("file", default, "TOC / SHELL workbook") %||% .ai_dialog_try(
      getExportedValue("utils", "choose.files")(default = .ai_file_default(default),
                                                caption = "TOC / SHELL workbook",
                                                multi = FALSE, filters = filters))
  } else {
    tcltk::tk_choose.files(default = .ai_file_default(default), caption = "TOC / SHELL workbook", multi = FALSE,
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
    .ps_dialog("file", default, "Shell workbooks", multi = TRUE) %||% .ai_dialog_try(
      getExportedValue("utils", "choose.files")(default = .ai_file_default(default),
                                                caption = "Shell workbooks",
                                                multi = TRUE, filters = filters))
  } else {
    tcltk::tk_choose.files(default = .ai_file_default(default), caption = "Shell workbooks", multi = TRUE,
                           filters = matrix(c("Excel workbook", ".xlsx", "Excel workbook", ".xlsm"),
                                            ncol = 2, byrow = TRUE))
  }
  out <- as.character(out)
  out <- out[!is.na(out) & nzchar(out)]
  normalizePath(out, "/", mustWork = FALSE)
}

# Reference files chosen in the dialog (any kind, several): their paths;
# none when cancelled
.ai_choose_refs <- function(default = "") {
  f <- getOption("tflplanner.choose_refs")
  out <- if (is.function(f)) f(default) else if (.Platform$OS.type == "windows") {
    .ps_dialog("file", default, "References", multi = TRUE, filter = "All files (*.*)|*.*") %||%
      .ai_dialog_try(getExportedValue("utils", "choose.files")(default = .ai_file_default(default),
                                                               caption = "References", multi = TRUE))
  } else {
    .ai_dialog_try(tcltk::tk_choose.files(default = .ai_file_default(default), caption = "References",
                                          multi = TRUE))
  }
  out <- as.character(out)
  out <- out[!is.na(out) & nzchar(out)]
  normalizePath(out, "/", mustWork = FALSE)
}

# A dialog of utils (or tcltk) that could not open: said so, with what to
# do instead
.ai_dialog_try <- function(expr) {
  tryCatch(expr, error = function(e) {
    stop("The computer's dialog cannot be opened here: paste the path into the field instead.",
         call. = FALSE)
  })
}

.ai_chosen <- function(x) {
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(NA_character_)
  normalizePath(x, "/", mustWork = FALSE)
}

# ---- the coding agent -------------------------------------------------------

# The agent's prompt, one for the whole study, as tfl_ai_prepare() gives it
# (its `agent_prompt`): ai/agent_prompt.md, which it writes too; a plain one
# when there is none (a study of an older tflspec), its paths from the study
# folder
.ai_agent_prompt <- function(path) {
  f <- file.path(path, "ai", "agent_prompt.md")
  if (file.exists(f)) {
    txt <- tryCatch(.ai_read_text(f), error = function(e) NULL)
    if (!is.null(txt)) return(sub("\n+$", "", txt))
  }
  paste0("Follow ai/AGENTS.md. Do every request of ai/requests/index.md, in its order: ",
         "ask me here what you cannot decide, then write each report's SPEC draft as ",
         "ai/spec/outputs/<id>.yml. When I say \"continue\", go on from where you stopped.")
}

# A study whose AI drafts are still written into spec/outputs (prepared
# before the drafts had their own folder, or a study with reports of its
# own): its drafts put apart, ai/spec/outputs (tflspec::tfl_ai_migrate();
# the study's files kept, as adopted), before answers are checked or run.R
# runs -- so neither writes over a report the person changed
.ai_ensure_area <- function(path) {
  if (is.null(path) || dir.exists(file.path(path, "ai", "spec", "outputs"))) return(invisible(FALSE))
  if (!dir.exists(file.path(path, "ai"))) return(invisible(FALSE))
  tryCatch(tflspec::tfl_ai_migrate(path), error = function(e) {
    message("tflplanner: the AI's drafts not put apart: ", conditionMessage(e))
  })
  invisible(TRUE)
}
