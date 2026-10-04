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
#' @param x A `tflplanner`.
#' @param spec What [tflspec::tfl_read_toc()] returned.
#' @param last The snapshot of the last TOC taken in, or `NULL` the first
#'   time.
#' @param title_offset The study's own title lines (see
#'   `toc_title_offset()`): a TOC's title line 1 is the line after them.
#' @return A list: `reports` (one row a report: `output_id`, `status` --
#'   `"new"`, `"changed"`, `"same"` or `"missing"` (in the last TOC, not
#'   in this one) --, `type_now`, `type_toc`, `guessed`) and `lines` (one
#'   row a title or footnote line the TOC and the definition differ on:
#'   `output_id`, `sheet`, `line`, `now`, `toc`, `action` -- `"add"`,
#'   `"update"`, `"remove"` (the TOC dropped a line not edited here),
#'   `"ask"` (edited here and changed in the TOC) or `"keep"`).
#' @export
toc_changes <- function(x, spec, last = NULL, title_offset = toc_title_offset(x)) {
  ids <- unique(stats::na.omit(spec$report$output_id))
  have <- x$outputs$output_id
  guessed <- attr(spec, "guessed") %||% character()
  lines <- list()
  status <- character()
  for (id in ids) {
    if (!id %in% have) {
      status[[id]] <- "new"
      next
    }
    changed <- FALSE
    for (sh in .toc_sheets) {
      toc <- .toc_text(.toc_lines(spec, sh, id, title_offset))
      now <- .toc_text(sheet_rows(x, sh, id))
      was <- .toc_text(last[[id]][[sh]])
      for (ln in union(names(toc), names(was))) {
        t_ <- toc[ln]
        n_ <- now[ln]
        w_ <- was[ln]
        action <- if (!is.na(t_)) {
          if (!is.na(n_) && identical(unname(n_), unname(t_))) NA_character_
          else if (is.na(n_)) "add"
          else if (!is.na(w_) && identical(unname(n_), unname(w_))) "update"
          else "ask"
        } else {
          # the TOC no longer has this line: removed only when not edited here
          if (is.na(n_)) NA_character_
          else if (!is.na(w_) && identical(unname(n_), unname(w_))) "remove"
          else NA_character_
        }
        if (is.na(action)) next
        changed <- TRUE
        lines[[length(lines) + 1L]] <- data.frame(
          output_id = id, sheet = sh, line = ln,
          now = if (is.na(n_)) NA_character_ else gsub("\u001f", " ", trimws(n_)),
          toc = if (is.na(t_)) NA_character_ else gsub("\u001f", " ", trimws(t_)),
          action = action, stringsAsFactors = FALSE)
      }
    }
    tn <- report_info(x, id)$type
    tt <- spec$report$type[match(id, spec$report$output_id)]
    if (!is.na(tt) && !identical(tn, tt)) changed <- TRUE
    status[[id]] <- if (changed) "changed" else "same"
  }
  gone <- setdiff(names(last %||% list()), ids)
  gone <- intersect(gone, have)
  for (id in gone) status[[id]] <- "missing"
  all_ids <- names(status)
  reports <- data.frame(
    output_id = all_ids, status = unname(status),
    type_now = vapply(all_ids, function(id)
      if (id %in% have) report_info(x, id)$type else NA_character_, ""),
    type_toc = spec$report$type[match(all_ids, spec$report$output_id)],
    guessed = all_ids %in% guessed,
    stringsAsFactors = FALSE, row.names = NULL)
  lines <- if (length(lines)) do.call(rbind, lines) else data.frame(
    output_id = character(), sheet = character(), line = character(),
    now = character(), toc = character(), action = character())
  list(reports = reports, lines = lines)
}

#' Put a TOC into a study's definition
#'
#' Adds the TOC's new reports (their type, program, file, note, titles and
#' footnotes) and, for the reports already there, puts in what the TOC
#' holds by the changes [toc_changes()] found: a line added, updated or
#' removed; one edited here and changed in the TOC only when `use_toc`
#' says so.  A report's type is not changed, nor anything the TOC does not
#' hold; a report no longer in the TOC stays.
#'
#' @param x A `tflplanner`.
#' @param spec What [tflspec::tfl_read_toc()] returned.
#' @param changes What [toc_changes()] returned.
#' @param use_toc The `"ask"` lines to take from the TOC, as
#'   `"<output_id>|<sheet>|<line>"`.
#' @param types The types of new reports, named by output id (a guessed
#'   type corrected); the TOC's otherwise.
#' @param title_offset As [toc_changes()].
#' @return The `tflplanner`.
#' @export
toc_apply <- function(x, spec, changes, use_toc = character(), types = character(),
                      title_offset = toc_title_offset(x)) {
  rep <- changes$reports
  # new reports
  for (id in rep$output_id[rep$status == "new"]) {
    type <- if (id %in% names(types)) types[[id]] else
      spec$report$type[match(id, spec$report$output_id)]
    if (is.na(type) || !type %in% report_types()) type <- "table"
    x <- add_output(x, id, type = type)
    for (sh in .toc_sheets) {
      d <- .toc_lines(spec, sh, id, title_offset)
      if (!is.null(d) && nrow(d)) {
        d$output_id <- NULL
        x <- set_sheet_rows(x, sh, id, d)
      }
    }
  }
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
    for (sh in .toc_sheets) {
      l <- ln[ln$output_id == id & ln$sheet == sh, , drop = FALSE]
      if (!nrow(l)) next
      take <- l$action %in% c("add", "update") |
        (l$action == "ask" & paste(id, sh, l$line, sep = "|") %in% use_toc)
      drop <- l$action == "remove"
      if (!any(take | drop)) next
      own <- sheet_rows(x, sh, id)
      own$output_id <- NULL
      toc <- .toc_lines(spec, sh, id, title_offset)
      own <- own[!as.character(own$line) %in% l$line[take | drop], , drop = FALSE]
      add <- toc[as.character(toc$line) %in% l$line[take], , drop = FALSE]
      add$output_id <- NULL
      for (cn in setdiff(names(own), names(add))) add[[cn]] <- rep(NA_character_, nrow(add))
      all <- rbind(own, add[names(own)])
      all <- all[order(suppressWarnings(as.integer(all$line))), , drop = FALSE]
      x <- set_sheet_rows(x, sh, id, all)
    }
  }
  x
}

#' What a TOC said, kept for the next time it is taken in
#'
#' @param spec What [tflspec::tfl_read_toc()] returned.
#' @param title_offset As [toc_changes()].
#' @return A list by output id: its `titles` and `footnotes` rows (as the
#'   study numbers them), `type` and `population`.
#' @export
toc_snapshot <- function(spec, title_offset = 0L) {
  ids <- unique(stats::na.omit(spec$report$output_id))
  stats::setNames(lapply(ids, function(id) list(
    titles = .toc_lines(spec, "titles", id, title_offset),
    footnotes = .toc_lines(spec, "footnotes", id),
    type = spec$report$type[match(id, spec$report$output_id)])), ids)
}

# ---- the mapping: which of a TOC's columns is what -------------------------

.toc_items <- c("output_id", "type", "title", "population", "footnote",
                "program", "file", "note")

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
    names(utils::read.csv(path, skip = skip, nrows = 1L, check.names = FALSE,
                          colClasses = "character", fileEncoding = "UTF-8-BOM"))
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
