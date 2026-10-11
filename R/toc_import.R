# A study's table of contents (TOC) in the company's own layout, taken in
# -- the first time and again when the TOC changes.  tflspec reads it
# (tflspec::tfl_read_toc(): the report, titles and footnotes sheets); here
# it is compared with the study's definition and what the last TOC said,
# and put in by Q16's rule: what the TOC holds is updated, what tflplanner
# added is not touched, a line edited here and changed in the TOC as well
# is asked about, and a report no longer in the TOC is marked, not deleted.

.toc_sheets <- c("titles", "footnotes")
.toc_cells <- c("left", "center", "right")

# a row's printed cells as one string ("" = none), to compare rows by
.toc_text <- function(rows) {
  if (is.null(rows) || !nrow(rows)) return(character())
  for (k in .toc_cells) if (!k %in% names(rows)) rows[[k]] <- NA_character_
  v <- vapply(seq_len(nrow(rows)), function(i) {
    cells <- vapply(.toc_cells, function(k) {
      x <- rows[[k]][i]
      if (is.na(x)) "" else as.character(x)
    }, "")
    paste(cells, collapse = "\u001f")
  }, "")
  stats::setNames(v, as.character(rows$line))
}

# Does the study's header (its default rows) say a token ("OUTPUT_TITLE")?
.study_header_says <- function(x, token) {
  d <- x$sheets$header
  if (is.null(d) || !nrow(d)) return(FALSE)
  d <- d[is.na(d$output_id), intersect(.toc_cells, names(d)), drop = FALSE]
  any(grepl(paste0("{", token, "}"), unlist(d), fixed = TRUE))
}

# A report's own tokens a TOC gives (named by token; none: NA), each the
# study's bands say -- a study that says none is not changed
.toc_token_items <- c(OUTPUT_LABEL = "labels", OUTPUT_TITLE = "first_titles",
                      OUTPUT_POPULATION = "populations",
                      OUTPUT_SECTION = "sections")
.toc_tokens <- function(spec, id) {
  v <- vapply(.toc_token_items, function(a) {
    v <- attr(spec, a)
    if (is.null(v) || !id %in% names(v)) NA_character_ else unname(v[[id]])
  }, "")
  # a list, as the TOC's record keeps it (input/toc/last.json)
  v <- v[!is.na(v)]
  if (length(v)) as.list(v) else list()
}
.study_says <- function(x, token) {
  any(vapply(c("header", "footer", "titles", "footnotes"), function(sh) {
    d <- x$sheets[[sh]]
    !is.null(d) && any(grepl(paste0("{", token, "}"),
                             unlist(d[intersect(.toc_cells, names(d))]),
                             fixed = TRUE))
  }, NA))
}

# A study whose header says {OUTPUT_TITLE} prints a report's title (the
# TOC's first title line) and analysis set there, from its tokens: they are
# then not title lines too.  The TOC as read, without them (once: the
# result is marked).
.toc_titles_in_header <- function(x, spec) {
  if (isTRUE(attr(spec, "titles_in_header")) ||
      !.study_header_says(x, "OUTPUT_TITLE")) return(spec)
  d <- spec$titles
  if (!is.null(d) && nrow(d)) {
    first <- attr(spec, "first_titles")
    pop <- attr(spec, "populations")
    keep <- rep(TRUE, nrow(d))
    for (id in unique(stats::na.omit(d$output_id))) {
      i <- which(!is.na(d$output_id) & d$output_id == id)
      i <- i[order(as.integer(d$line[i]))]
      gone <- rep(FALSE, length(i))
      if (length(i) && !is.null(first) && !is.na(first[id])) gone[1L] <- TRUE
      if (length(i) && !is.null(pop) && !is.na(pop[id])) gone[length(i)] <- TRUE
      keep[i[gone]] <- FALSE
      d$line[i[!gone]] <- as.character(seq_len(sum(!gone)))
    }
    spec$titles <- d[keep, , drop = FALSE]
  }
  attr(spec, "titles_in_header") <- TRUE
  spec
}

# The lines a TOC gives a report, numbered as the study numbers them: after
# the study's own title lines (`title_offset`), footnotes from 1
.toc_lines <- function(spec, sheet, output_id, title_offset = 0L) {
  d <- spec[[sheet]]
  if (is.null(d)) return(NULL)
  d <- d[!is.na(d$output_id) & d$output_id == output_id, , drop = FALSE]
  if (!nrow(d)) return(d)
  if (identical(sheet, "titles")) {
    d$line <- as.character(as.integer(d$line) + as.integer(title_offset))
  }
  d
}

# Where a TOC's title lines start: after the study's own title lines (those
# every report prints), so a TOC's first title is the first line left
toc_title_offset <- function(x) {
  d <- sheet_rows(x, "titles", NA)
  ln <- suppressWarnings(as.integer(d$line))
  if (!length(ln) || all(is.na(ln))) 0L else max(ln, na.rm = TRUE)
}

#' What taking in a TOC would change
#'
#' Compares the report sheets a TOC gives ([tflspec::tfl_read_toc()]) with
#' the study's definition and with what the last TOC taken in said
#' (`last`, see [toc_snapshot()]).
#'
#' A report's title and footnote lines are of two kinds: those the TOC gave
#' (the lines the last TOC had; the first time, the lines where this TOC
#' has one) and those added here.  The TOC's lines are compared line by
#' line: one not edited here takes the TOC's new text; one edited here keeps
#' its text when the TOC did not change it, and is asked about when the TOC
#' changed it too.  The lines added here are not compared: they stay, after
#' the TOC's lines.
#'
#' @param x A `tflplanner`.
#' @param spec What [tflspec::tfl_read_toc()] returned.
#' @param last The snapshot of the TOCs taken in before, or `NULL` the first
#'   time.
#' @param title_offset The study's own title lines (see
#'   `toc_title_offset()`): a TOC's title line 1 is the line after them.
#' @return A list: `reports` (one row a report: `output_id`, `status` --
#'   `"new"`, `"changed"`, `"same"` or `"missing"` (taken in from a TOC
#'   before, not in this one) --, `type_now`, `type_toc`, `guessed`),
#'   `lines` (one row a title or footnote line that changes: `output_id`,
#'   `sheet`, `line` (its place among the TOC's lines), `now`, `toc`,
#'   `action` -- `"add"`, `"update"`, `"remove"` (the TOC dropped a line not
#'   edited here), `"ask"` (edited here and changed in the TOC) or `"move"`
#'   (a line added here, now after the TOC's lines: `to`) -- and `to`, the
#'   line it will be) and `kept` (the lines edited here the TOC did not
#'   change: kept, not asked about); and the `last` and `title_offset` it
#'   was given.
#' @export
toc_changes <- function(x, spec, last = NULL, title_offset = toc_title_offset(x)) {
  ids <- unique(stats::na.omit(spec$report$output_id))
  have <- x$outputs$output_id
  guessed <- attr(spec, "guessed") %||% character()
  lines <- list()
  kept <- list()
  status <- character()
  for (id in ids) {
    if (!id %in% have) {
      status[[id]] <- "new"
      next
    }
    changed <- FALSE
    for (sh in .toc_sheets) {
      m <- .toc_merge(x, spec, last, id, sh, title_offset)
      if (nrow(m$lines)) {
        changed <- TRUE
        lines[[length(lines) + 1L]] <- m$lines
      }
      if (nrow(m$kept)) kept[[length(kept) + 1L]] <- m$kept
    }
    tn <- report_info(x, id)$type
    tt <- spec$report$type[match(id, spec$report$output_id)]
    if (!is.na(tt) && !identical(tn, tt)) changed <- TRUE
    status[[id]] <- if (changed) "changed" else "same"
  }
  # every report a TOC gave before that this one has not (still there)
  gone <- intersect(setdiff(names(last %||% list()), ids), have)
  for (id in gone) status[[id]] <- "missing"
  all_ids <- names(status)
  reports <- data.frame(
    output_id = all_ids, status = unname(status),
    type_now = vapply(all_ids, function(id)
      if (id %in% have) report_info(x, id)$type else NA_character_, ""),
    type_toc = spec$report$type[match(all_ids, spec$report$output_id)],
    guessed = all_ids %in% guessed,
    stringsAsFactors = FALSE, row.names = NULL)
  list(reports = reports,
       lines = if (length(lines)) do.call(rbind, lines) else .toc_no_lines(),
       kept = if (length(kept)) do.call(rbind, kept) else .toc_no_lines(),
       last = last, title_offset = title_offset)
}

.toc_no_lines <- function() {
  data.frame(output_id = character(), sheet = character(), line = character(),
             now = character(), toc = character(), action = character(),
             to = character(), stringsAsFactors = FALSE)
}

# One report's lines of one sheet, merged: `rows` (what the sheet will hold
# when nothing asked about is taken from the TOC), `alt` (the TOC's row for
# each line asked about, by its place), `lines` (what changes) and `kept`.
.toc_merge <- function(x, spec, last, id, sh, title_offset) {
  base <- if (identical(sh, "titles")) as.integer(title_offset) + 1L else 1L
  toc <- .toc_lines(spec, sh, id, title_offset)
  if (is.null(toc)) toc <- data.frame(line = character())
  now <- sheet_rows(x, sh, id)
  now$output_id <- NULL
  now <- now[order(suppressWarnings(as.integer(now$line))), , drop = FALSE]
  was <- last[[id]][[sh]]
  t_txt <- .toc_text(toc)
  n_txt <- .toc_text(now)
  w_txt <- .toc_text(was)
  # the TOC's lines here: those the last TOC had (the first time, where this
  # TOC has one); the others were added here
  from_toc <- if (is.null(last[[id]])) names(n_txt) %in% names(t_txt) else
    names(n_txt) %in% names(w_txt)
  places <- union(names(t_txt), names(n_txt)[from_toc])
  places <- places[order(as.integer(places))]
  cells <- function(d, k) {
    r <- d[as.character(d$line) == k, , drop = FALSE][1L, , drop = FALSE]
    for (cn in .toc_cells) if (!cn %in% names(r)) r[[cn]] <- NA_character_
    r
  }
  out <- list()
  alt <- list()
  ch <- list()
  kp <- list()
  show <- function(s) if (is.na(s)) NA_character_ else trimws(gsub("\u001f", " ", s))
  note <- function(k, n_, t_, action, kind = "lines") {
    row <- data.frame(output_id = id, sheet = sh, line = k, now = show(n_),
                      toc = show(t_), action = action, to = NA_character_,
                      stringsAsFactors = FALSE)
    if (kind == "lines") ch[[length(ch) + 1L]] <<- row else kp[[length(kp) + 1L]] <<- row
  }
  for (k in places) {
    t_ <- unname(t_txt[k])
    n_ <- if (k %in% names(n_txt)[from_toc]) unname(n_txt[k]) else NA_character_
    w_ <- unname(w_txt[k])
    if (!is.na(t_)) {
      if (is.na(n_)) {
        out[[k]] <- cells(toc, k)
        note(k, n_, t_, "add")
      } else if (identical(n_, t_)) {
        out[[k]] <- cells(now, k)
      } else if (!is.na(w_) && identical(n_, w_)) {
        out[[k]] <- cells(toc, k)
        note(k, n_, t_, "update")
      } else if (!is.na(w_) && identical(t_, w_)) {
        # edited here, the TOC unchanged: kept, not asked again
        out[[k]] <- cells(now, k)
        note(k, n_, t_, "keep", "kept")
      } else {
        out[[k]] <- cells(now, k)
        alt[[k]] <- cells(toc, k)
        note(k, n_, t_, "ask")
      }
    } else if (!is.na(n_)) {
      if (!is.na(w_) && identical(n_, w_)) {
        note(k, n_, t_, "remove")
      } else {
        # edited here, the TOC dropped it: kept
        out[[k]] <- cells(now, k)
        note(k, n_, t_, "keep", "kept")
      }
    }
  }
  own <- now[!from_toc, , drop = FALSE]
  for (i in seq_len(nrow(own))) out[[paste0("own", i)]] <- own[i, , drop = FALSE]
  cols <- unique(c(names(now), "line", .toc_cells))
  rows <- if (length(out)) do.call(rbind, lapply(out, function(r) {
    for (cn in setdiff(cols, names(r))) r[[cn]] <- NA_character_
    r[cols]
  })) else now[0L, , drop = FALSE]
  old_line <- as.character(rows$line)
  rows$line <- as.character(base + seq_len(nrow(rows)) - 1L)
  rows$.place <- c(names(out))
  rownames(rows) <- NULL
  # the lines added here that move (after the TOC's lines)
  for (i in which(!rows$.place %in% places & old_line != rows$line)) {
    ch[[length(ch) + 1L]] <- data.frame(
      output_id = id, sheet = sh, line = old_line[i], now = show(.toc_text(rows[i, ])),
      toc = NA_character_, action = "move", to = rows$line[i], stringsAsFactors = FALSE)
  }
  list(rows = rows, alt = alt,
       lines = if (length(ch)) do.call(rbind, ch) else .toc_no_lines(),
       kept = if (length(kp)) do.call(rbind, kp) else .toc_no_lines())
}

#' Put a TOC into a study's definition
#'
#' Adds the TOC's new reports (their type, program, file, note, titles and
#' footnotes) and, for the reports already there, puts in what the TOC
#' holds by the rule [toc_changes()] describes: a line added, updated or
#' removed; one edited here and changed in the TOC only when `use_toc`
#' says so; the lines added here after the TOC's.  A report's type is not
#' changed, nor anything the TOC does not hold; a report no longer in the
#' TOC stays.
#'
#' @param x A `tflplanner`.
#' @param spec What [tflspec::tfl_read_toc()] returned.
#' @param changes What [toc_changes()] returned.
#' @param use_toc The `"ask"` lines to take from the TOC, as
#'   `"<output_id>|<sheet>|<line>"`.
#' @param types The types of new reports, named by output id (a guessed
#'   type corrected); the TOC's otherwise.
#' @param last,title_offset As [toc_changes()] was given them (kept in
#'   `changes`).
#' @param populations The reports' analysis sets, a population_id named by
#'   report (see [toc_populations()]): each one that differs from the
#'   report's is set with [set_report_population()] (its data made, its
#'   analyses moved).
#' @param make_data Make the analysis data of each new report's datasets
#'   (the TOC's `datasets`): `<dataset>_<set>`, kept to the subjects of its
#'   analysis set (`adsl_<set>`), found or made.
#' @param again With `make_data`, also for the reports taken in before
#'   (those deleted since are made again).
#' @return The `tflplanner`.
#' @export
toc_apply <- function(x, spec, changes, use_toc = character(), types = character(),
                      last = changes$last,
                      title_offset = changes$title_offset %||% toc_title_offset(x),
                      populations = character(), make_data = FALSE, again = FALSE) {
  rep <- changes$reports
  # new reports
  for (id in rep$output_id[rep$status == "new"]) {
    type <- if (id %in% names(types)) types[[id]] else
      spec$report$type[match(id, spec$report$output_id)]
    if (is.na(type) || !type %in% report_types()) type <- "table"
    x <- add_output(x, id, type = type, at = "end")
    for (sh in .toc_sheets) {
      d <- .toc_lines(spec, sh, id, title_offset)
      if (!is.null(d) && nrow(d)) {
        d$output_id <- NULL
        x <- set_sheet_rows(x, sh, id, d)
      }
    }
  }
  # the section each report is under (the TOC's heading row above it, or
  # its section column): written where the report has none yet -- one
  # given in the app is kept
  sec <- attr(spec, "sections")
  if (length(sec)) {
    o <- x$outputs
    if (!"section" %in% names(o)) o$section <- NA_character_
    k <- match(names(sec), o$output_id)
    put <- !is.na(k) & !is.na(sec) & is.na(o$section[k])
    o$section[k[put]] <- unname(sec[put])
    x$outputs <- o
  }
  # the reports' analysis sets, as the TOC says them (one value with the
  # report list's and step 1's: a change moves the report's analyses)
  made <- character()
  for (id in intersect(names(populations), x$outputs$output_id)) {
    v <- populations[[id]]
    if (.is_blank(v) || identical(report_population(x, id), v)) next
    x <- set_report_population(x, id, v)
    made <- c(made, attr(x, "made"))
  }
  attr(x, "made") <- attr(x, "left") <- NULL
  # the reports' datasets as the TOC says them (the report list shows them
  # until the ARD names its own); a new listing or figure reads them; the
  # analysis data of a table's, kept to its set's subjects
  ds <- attr(spec, "datasets")
  ds <- ds[!is.na(names(ds)) & names(ds) %in% x$outputs$output_id]
  if (length(ds)) {
    o <- x$outputs
    if (is.null(o$datasets)) o$datasets <- rep(NA_character_, nrow(o))
    k <- match(names(ds), o$output_id)
    o$datasets[k] <- unname(ds)
    x$outputs <- o
    new_ids <- rep$output_id[rep$status == "new"]
    for (id in intersect(names(ds), new_ids)) {
      v <- .split_bar(ds[[id]])
      if (!length(v)) next
      type <- report_info(x, id)$type
      if (identical(type, "listing")) {
        r <- lf_rows(x, "listings", id)
        if (!nrow(r)) r <- data.frame(dataset = v[1L])
        if (.is_blank(r$dataset[1L])) r$dataset[1L] <- v[1L]
        x <- set_lf_rows(x, "listings", id, r)
      } else if (type %in% c("figure", "user")) {
        r <- lf_rows(x, "figures", id)
        if (!nrow(r)) r <- data.frame(datasets = ds[[id]])
        if (.is_blank(r$datasets[1L])) r$datasets[1L] <- ds[[id]]
        x <- set_lf_rows(x, "figures", id, r)
      }
    }
    if (isTRUE(make_data)) {
      for (id in names(ds)) {
        if (!id %in% new_ids && !isTRUE(again)) next
        if (!identical(report_info(x, id)$type, "table")) next
        x <- .toc_make_data(x, id)
        made <- c(made, attr(x, "made"))
        attr(x, "made") <- NULL
      }
    }
  }
  if (length(made)) attr(x, "made") <- unique(made)
  # the report sheet's own items the TOC holds (not the type of a report
  # already there)
  cols <- intersect(c("program", "file", "note"), names(spec$report))
  r <- x$sheets$report
  for (id in rep$output_id[rep$status %in% c("new", "changed", "same")]) {
    k <- match(id, spec$report$output_id)
    vals <- spec$report[k, cols, drop = FALSE]
    vals <- vals[, !is.na(unlist(vals)), drop = FALSE]
    if (!ncol(vals)) next
    i <- which(!is.na(r$output_id) & r$output_id == id)
    if (!length(i)) {
      r[nrow(r) + 1L, ] <- NA
      i <- nrow(r)
      r$output_id[i] <- id
    }
    for (cn in names(vals)) r[[cn]][i] <- as.character(vals[[cn]])
  }
  x$sheets$report <- r
  # the lines of the reports already there
  ln <- changes$lines
  for (id in unique(ln$output_id)) {
    for (sh in unique(ln$sheet[ln$output_id == id])) {
      m <- .toc_merge(x, spec, last, id, sh, title_offset)
      rows <- m$rows
      for (k in names(m$alt)) {
        if (!paste(id, sh, k, sep = "|") %in% use_toc) next
        i <- which(rows$.place == k)
        for (cn in .toc_cells) rows[[cn]][i] <- m$alt[[k]][[cn]]
      }
      rows$.place <- NULL
      x <- set_sheet_rows(x, sh, id, rows)
    }
  }
  # the report's own tokens ({OUTPUT_TITLE} ...) the study says: written
  # where the report has none, or has what the last TOC gave (a value
  # changed here is kept)
  says <- names(.toc_token_items)[vapply(names(.toc_token_items),
                                         function(k) .study_says(x, k), NA)]
  tk <- x$sheets$tokens
  for (id in rep$output_id[rep$status %in% c("new", "changed", "same")]) {
    vals <- .toc_tokens(spec, id)
    vals <- vals[intersect(names(vals), says)]
    was <- last[[id]]$tokens
    for (k in names(vals)) {
      i <- which(!is.na(tk$output_id) & tk$output_id == id & tk$name == k)
      if (length(i) && !identical(tk$value[i[1L]], was[[k]])) next
      if (!length(i)) {
        tk[nrow(tk) + 1L, ] <- NA
        i <- nrow(tk)
        tk$output_id[i] <- id
        tk$name[i] <- k
      }
      tk$value[i[1L]] <- vals[[k]]
    }
  }
  x$sheets$tokens <- tk
  x
}

#' What a TOC said, kept for the next time one is taken in
#'
#' @param spec What [tflspec::tfl_read_toc()] returned.
#' @param title_offset As [toc_changes()].
#' @param last The snapshot of the TOCs taken in before: the reports they
#'   gave that this TOC has not are kept (`in_toc = FALSE`), so a report
#'   once taken in from a TOC is said to be missing every time.
#' @return A list by output id: its `titles` and `footnotes` rows (as the
#'   study numbers them), `type` and `in_toc`.
#' @export
toc_snapshot <- function(spec, title_offset = 0L, last = NULL) {
  ids <- unique(stats::na.omit(spec$report$output_id))
  now <- stats::setNames(lapply(ids, function(id) list(
    titles = .toc_lines(spec, "titles", id, title_offset),
    footnotes = .toc_lines(spec, "footnotes", id),
    type = spec$report$type[match(id, spec$report$output_id)],
    tokens = .toc_tokens(spec, id),
    in_toc = TRUE)), ids)
  old <- (last %||% list())[setdiff(names(last %||% list()), ids)]
  old <- lapply(old, function(r) {
    r$in_toc <- FALSE
    r
  })
  c(now, old)
}

# A table's analysis data from its datasets (the report list's, the TOC's):
# for each dataset of the study that is not its analysis set's own (ADSL),
# <dataset>_<set> kept to the set's subjects (adsl_<set>) -- the one there
# with that definition, else one made.  Its set: the report's, else the
# study's first.  The planner, with attr "made".
.toc_make_data <- function(x, output_id) {
  before <- .adata_rows(x, output_id)$data_id
  po <- x$ard$populations
  pop <- report_population(x, output_id)
  if (is.na(pop)) pop <- po$population_id[!is.na(po$population_id)][1L]
  out <- function(x) {
    attr(x, "made") <- setdiff(.adata_rows(x, output_id)$data_id, before)
    x
  }
  if (is.na(pop %||% NA)) return(out(x))
  v <- .split_bar(x$outputs$datasets[match(output_id, x$outputs$output_id)])
  have <- x$ard$datasets$dataset
  v <- have[match(toupper(v), toupper(have))]
  pds <- po$dataset[match(pop, po$population_id)]
  v <- setdiff(v[!is.na(v)], c(pds, "ADSL"))
  if (!length(v)) return(out(x))
  x <- .ensure_pop_adata(x, output_id, pop)
  subj <- attr(x, "data_id")
  attr(x, "data_id") <- attr(x, "added") <- NULL
  for (d in v) {
    if (!is.na(.adata_same_as(.adata_rows(x, output_id), d, NA, subj, NA))) next
    nm <- .adata_free_name(x, output_id, .adata_default_name(d, pop))
    x <- set_analysis_data(x, output_id, nm, from = d, subjects = subj)
  }
  out(x)
}

# The analysis data a report starts with from its datasets (the report
# list's, the TOC's): those .toc_make_data() makes, the set's first
.report_toc_data <- function(x, output_id) {
  pop <- report_population(x, output_id)
  if (is.na(pop)) return(character())
  ad <- .adata_rows(x, output_id)
  po <- x$ard$populations
  pds <- po$dataset[match(pop, po$population_id)]
  subj <- .adata_same_as(ad, if (is.na(pds)) "ADSL" else pds, pop, NA, NA)
  if (is.na(subj)) return(character())
  v <- .split_bar(x$outputs$datasets[match(output_id, x$outputs$output_id)] %||% NA)
  have <- x$ard$datasets$dataset
  v <- setdiff(have[match(toupper(v), toupper(have))], c(NA, pds))
  own <- vapply(v, function(d) .adata_same_as(ad, d, NA, subj, NA), "")
  c(own[!is.na(own)], subj)
}

#' The analysis set each report of a TOC names
#'
#' The TOC's population column, by report: its text (`text`, "Safety
#' Population") and the study's analysis set it is (`population_id`: its id,
#' its label, a usual word for its flag, or its id in the text; `NA` when
#' none).
#'
#' @param x A `tflplanner`.
#' @param path,sheet,skip The TOC, as [tflspec::tfl_read_toc()] reads it.
#' @param id_col,pop_col Its columns of the report IDs and the populations.
#' @param data The data of the analysis sets (ADSL), for its flags' labels.
#' @return A data frame: output_id, text, population_id.
#' @export
toc_populations <- function(x, path, id_col, pop_col, sheet = NULL, skip = 0L,
                            data = NULL) {
  d <- .toc_raw(path, sheet, skip)
  if (is.null(d) || !all(c(id_col, pop_col) %in% names(d))) {
    return(data.frame(output_id = character(), text = character(),
                      population_id = character(), stringsAsFactors = FALSE))
  }
  id <- trimws(as.character(d[[id_col]]))
  v <- trimws(as.character(d[[pop_col]]))
  keep <- !is.na(id) & nzchar(id) & !is.na(v) & nzchar(v)
  id <- id[keep]
  v <- v[keep]
  u <- unique(v)
  m <- stats::setNames(vapply(u, function(s) .pop_match(x, s, data), ""), u)
  data.frame(output_id = id, text = v, population_id = unname(m[v]),
             stringsAsFactors = FALSE)
}

.toc_raw <- function(path, sheet = NULL, skip = 0L) {
  tryCatch(
    if (tolower(tools::file_ext(path)) == "csv") {
      suppressWarnings(utils::read.csv(path, skip = skip, check.names = FALSE,
                                       colClasses = "character",
                                       fileEncoding = "UTF-8-BOM"))
    } else {
      as.data.frame(readxl::read_excel(path, sheet = sheet %||% 1L, skip = skip,
                                       col_types = "text"))
    }, error = function(e) NULL)
}

# ---- the mapping: which of a TOC's columns is what -------------------------

.toc_items <- c("output_id", "type", "title", "population", "footnote",
                "program", "file", "note", "section", "datasets", "label")

# The map tflspec::tfl_read_toc() takes, from a TOC's column names and the
# company's toc_map (an item: the first of its names the TOC has; title and
# footnote: every one, in the TOC's order)
toc_map_for <- function(headers, std = company_standards()$toc_map) {
  norm <- function(x) tolower(trimws(x))
  out <- list()
  for (it in .toc_items) {
    cand <- std$columns[match(it, std$item)]
    if (is.na(cand)) next
    names_ <- trimws(strsplit(cand, "|", fixed = TRUE)[[1L]])
    hit <- headers[norm(headers) %in% norm(names_)]
    if (!length(hit)) next
    out[[it]] <- if (it %in% c("title", "footnote")) hit else
      hit[order(match(norm(hit), norm(names_)))][1L]
  }
  out
}

# The company's toc_map with a TOC's column names added to its items'
# candidates (what "remember this mapping" writes)
.toc_map_learn <- function(std, map) {
  for (it in names(map)) {
    cols <- map[[it]]
    cols <- cols[!is.na(cols) & nzchar(cols)]
    if (!length(cols)) next
    i <- match(it, std$item)
    if (is.na(i)) {
      std[nrow(std) + 1L, ] <- NA
      i <- nrow(std)
      std$item[i] <- it
    }
    have <- if (is.na(std$columns[i])) character() else
      trimws(strsplit(std$columns[i], "|", fixed = TRUE)[[1L]])
    new <- cols[!tolower(cols) %in% tolower(have)]
    std$columns[i] <- paste(c(have, new), collapse = " | ")
  }
  std
}

# Remember a mapping in the company standards in tflplanner's home (the
# workbook is written there, from the standards in use, when there is none)
remember_toc_map <- function(map, home = tflplanner_home()) {
  s <- company_standards(home)
  s$toc_map <- .toc_map_learn(s$toc_map, map)
  f <- .standards_file(home)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(c(list(`_README` = .standards_readme()), s), f)
  invisible(f)
}

# A TOC's column names (its header row, after `skip` rows)
toc_headers <- function(path, sheet = NULL, skip = 0L) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    names(suppressWarnings(utils::read.csv(
      path, skip = skip, nrows = 1L, check.names = FALSE,
      colClasses = "character", fileEncoding = "UTF-8-BOM")))
  } else {
    names(readxl::read_excel(path, sheet = sheet %||% 1L, skip = skip, n_max = 0L))
  }
}

# ---- the record: input/toc/ ---------------------------------------------------

.toc_dir <- function(study) file.path(study$path, study_layout()[["toc_import"]])

#' TOCs taken into a study
#'
#' `toc_imports()`: the record of every TOC taken in (`input/toc/imports.csv`:
#' `import_id`, `file`, `original`, `imported`, `user`, `md5`, `reports`,
#' `new`, `changed`, `missing`).  `toc_last()`: what the last TOC taken in
#' said, by report ([toc_snapshot()]), or `NULL`.
#'
#' @param study An `rtfstudy`.
#' @return A data frame; a list or `NULL`.
#' @export
toc_imports <- function(study) {
  f <- file.path(.toc_dir(study), "imports.csv")
  cols <- c("import_id", "file", "original", "imported", "user", "md5",
            "reports", "new", "changed", "missing")
  if (!file.exists(f)) {
    return(as.data.frame(stats::setNames(
      replicate(length(cols), character(), simplify = FALSE), cols)))
  }
  utils::read.csv(f, colClasses = "character", na.strings = "")
}

#' @rdname toc_imports
#' @export
toc_last <- function(study) {
  f <- file.path(.toc_dir(study), "last.json")
  if (!file.exists(f)) return(NULL)
  j <- jsonlite::read_json(f, simplifyVector = TRUE)
  lapply(j, function(r) {
    for (sh in .toc_sheets) {
      d <- r[[sh]]
      if (is.null(d) || !length(d)) {
        r[sh] <- list(NULL)
        next
      }
      d <- as.data.frame(d, stringsAsFactors = FALSE)
      d[] <- lapply(d, as.character)
      r[[sh]] <- d
    }
    r
  })
}

# Keep a TOC taken in: its copy (read-only), the record's row, and what it
# said (for the next time)
.toc_record <- function(study, path, original, changes, snapshot) {
  dir <- .toc_dir(study)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  log <- toc_imports(study)
  id <- paste0("TOC", formatC(nrow(log) + 1L, width = 3L, flag = "0"))
  nm <- paste0(id, "_", basename(original))
  dest <- file.path(dir, nm)
  file.copy(path, dest, overwrite = TRUE)
  Sys.chmod(dest, "0444")
  st <- changes$reports$status
  row <- data.frame(
    import_id = id, file = nm, original = basename(original),
    imported = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    user = Sys.info()[["user"]], md5 = unname(tools::md5sum(dest)),
    reports = as.character(sum(st != "missing")), new = as.character(sum(st == "new")),
    changed = as.character(sum(st == "changed")), missing = as.character(sum(st == "missing")),
    stringsAsFactors = FALSE)
  utils::write.csv(rbind(log, row), file.path(dir, "imports.csv"), row.names = FALSE, na = "")
  jsonlite::write_json(snapshot, file.path(dir, "last.json"), auto_unbox = TRUE,
                       null = "null", na = "null", pretty = TRUE)
  invisible(row)
}

# A TOC's report IDs given on more than one row: one row an ID, with the
# rows of the file they are on (the header is row skip + 1)
.toc_dups <- function(path, sheet = NULL, skip = 0L, column) {
  d <- tryCatch(
    if (tolower(tools::file_ext(path)) == "csv") {
      suppressWarnings(utils::read.csv(path, skip = skip, check.names = FALSE,
                                       colClasses = "character",
                                       fileEncoding = "UTF-8-BOM"))
    } else {
      as.data.frame(readxl::read_excel(path, sheet = sheet %||% 1L, skip = skip,
                                       col_types = "text"))
    }, error = function(e) NULL)
  if (is.null(d) || !column %in% names(d)) return(NULL)
  v <- trimws(as.character(d[[column]]))
  v[!is.na(v) & !nzchar(v)] <- NA
  dup <- unique(v[!is.na(v) & duplicated(v)])
  if (!length(dup)) return(NULL)
  data.frame(output_id = dup, rows = vapply(dup, function(id)
    paste(which(v == id) + skip + 1L, collapse = ", "), ""),
    stringsAsFactors = FALSE, row.names = NULL)
}
