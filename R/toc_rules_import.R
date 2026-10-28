# Taking a TOC in by the company's rules: the import pipeline
# (toc_import_read() and friends).  Split from toc_rules.R (no code change).

# ---- the pipeline -------------------------------------------------------------

#' Take a TOC in by the company's rules
#'
#' `toc_import_read()` reads one TOC or several (a TFL workbook and a
#' listing workbook of the same layout) by the company's rules: each source
#' read once, its TOC sheet and header row found ([toc_find_sheet()]), the
#' rule set picked ([toc_pick_rules()]), the columns mapped
#' ([toc_map_for()]), the output ids made by the set's rule (or the one
#' that gives unique ids for the most rows, [toc_id_candidates()]), one
#' report a phase ([toc_expand_phases()]), the shell sheets linked
#' ([toc_link_shells()]), the Topline batch ([toc_batches()]) and what is
#' wrong ([toc_problems()]).  Nothing is written.
#'
#' `toc_import_spec()` gives the reports as [tflspec::tfl_read_toc()] gives
#' a TOC, for [toc_changes()] and [toc_apply()].
#'
#' What the user chose in the dialog (`choices`), else what the study's
#' profile remembers (`profile`), wins over what is found: `rule_set`,
#' `sheet` and `header_row` (named by the file's name), `map`, `id_rule`,
#' `type_from`, `overrides` (the set's settings changed for the study).
#'
#' Two example TOCs come with the package, made from the sample study
#' (SAMPLE-01): `system.file("extdata", "SAMPLE-01_TOC.xlsx", package =
#' "tflplanner")` -- its reports as a company TOC (the header on row 2,
#' section heading rows, Shell Number, Phase, Topline, Title1-3,
#' Population, Footnote1-2, a shell sheet per report) -- and
#' `SAMPLE-01_TOC_changed.xlsx`, the same with two rows changed (a title,
#' a footnote).  Taken into the sample study, the first gives its reports
#' their sections (and the Topline batch) and, taken in again, changes
#' nothing; the second, taken in after it, shows those two.
#'
#' @param sources The TOC files (paths) or `toc_source`s.
#' @param rules What [toc_rules()] returned.
#' @param profile The study's TOC profile ([toc_profile()]) or `NULL`.
#' @param choices A list (see above) or `NULL`.
#' @param names The sources' names as the user knows them (paths: their
#'   base names).
#' @return A `toc_import`: `sources`, `detection` (a row a source: `file`,
#'   `sheet`, `header_row`, `how` -- `"name"`, `"score"`, `"rows"`,
#'   `"chosen"`, `"profile"` --, `rows`, `shells`, `topline_sheet`, `kind`,
#'   `note`), `sheets` (each source's [toc_find_sheet()]), `rule_set` (the
#'   set in use), `rule_sets` (the scores), `map` (+ `attr(, "missing")`),
#'   `id_rule`, `id_candidates`, `type_from`, `rows`, `reports`, `unread`,
#'   `batches`, `problems`, `profile`, `notes` (what was not as the profile
#'   said: `message`).
#' @examples
#' f <- tempfile(fileext = ".csv")
#' writeLines(c("Output ID,Title 1,Title 2,Population",
#'              "T-14-1-1,Demographics,,Safety Population",
#'              "T-14-2-1,Adverse events,Overview,Safety Population"), f)
#' r <- toc_import_read(f, toc_rules(home = tempfile()))
#' r$reports[c("output_id", "label")]
#' toc_import_spec(r)$report
#'
#' # the example TOC made from the sample study
#' f <- system.file("extdata", "SAMPLE-01_TOC.xlsx", package = "tflplanner")
#' r <- toc_import_read(f, toc_rules(home = tempfile()))
#' r$id_rule
#' utils::head(r$reports[c("output_id", "label", "shell_sheet")])
#' @export
toc_import_read <- function(sources, rules = toc_rules(), profile = NULL,
                            choices = NULL, names = NULL) {
  hr_max <- max(vapply(rules, function(s) s$settings$header_rows, 1L))
  srcs <- lapply(seq_along(sources), function(k) {
    s <- sources[[k]]
    if (inherits(s, "toc_source")) s else
      toc_source(s, hr_max, name = names[k] %||% basename(s))
  })
  pick_set <- choices$rule_set %||% profile$rule_set
  if (!is.null(pick_set) && !pick_set %in% names(rules)) pick_set <- NULL
  det_rules <- if (is.null(pick_set)) rules else rules[pick_set]
  prof_src <- function(file, key) {
    for (p in profile$sources %||% list()) {
      if (identical(p$file, file)) return(p[[key]])
    }
    NULL
  }
  sheets <- list()
  det <- list()
  notes <- data.frame(message = character(), template = character(), file = character(),
                      sheet = character(), stringsAsFactors = FALSE)
  for (s in srcs) {
    fs <- toc_find_sheet(s, det_rules)
    sheets[[s$name]] <- fs
    kind <- attr(fs, "kind")
    want <- .toc_pick(choices$sheet, s$name)
    how <- if (nrow(fs)) fs$why[1L] else ""
    if (is.null(want)) {
      want <- prof_src(s$name, "toc_sheet")
      if (!is.null(want) && want %in% s$sheets) how <- "profile"
      if (!is.null(want) && !want %in% s$sheets) {
        tpl <- "%s: the sheet %s is not in the file any more: found again."
        notes <- rbind(notes, data.frame(message = sprintf(tpl, s$name, want), template = tpl,
                                         file = s$name, sheet = want, stringsAsFactors = FALSE))
        want <- NULL
      }
    } else if (want %in% s$sheets) how <- "chosen" else want <- NULL
    sheet <- if (!is.null(want)) want else if (kind == "toc") fs$sheet[1L] else NA_character_
    hrow <- if (!is.na(sheet)) fs$header_row[match(sheet, fs$sheet)] else NA_integer_
    h_want <- .toc_pick(choices$header_row, s$name) %||%
      (if (identical(how, "profile")) prof_src(s$name, "header_row"))
    if (!is.null(h_want) && !is.na(suppressWarnings(as.integer(h_want)))) {
      hrow <- as.integer(h_want)
      if (!identical(how, "profile")) how <- "chosen"
    }
    if (!is.na(sheet) && is.na(hrow)) {
      hrow <- toc_find_header(s$head[[sheet]], det_rules)$row
    }
    if (!is.na(sheet) && sheet != fs$sheet[1L]) kind <- "toc"
    tl <- fs$sheet[fs$role == "topline" & fs$sheet != .or_na(sheet, "")]
    det[[s$name]] <- data.frame(
      file = s$name, sheet = sheet, header_row = hrow, how = how,
      rows = if (!is.na(sheet)) fs$rows[match(sheet, fs$sheet)] else 0L,
      shells = sum(.toc_shell_named(sub("-[0-9]+$", "", s$sheets))),
      topline_sheet = if (length(tl)) tl[1L] else NA_character_,
      kind = if (s$kind == "other") "other" else kind,
      two_toc = isTRUE(attr(fs, "two_toc")) && !how %in% c("chosen", "profile"),
      others = paste(fs$sheet[fs$role %in% c("other", "topline")], collapse = ", "),
      stringsAsFactors = FALSE)
  }
  detection <- do.call(rbind, det)
  rownames(detection) <- NULL
  readable <- which(!is.na(detection$sheet) & !is.na(detection$header_row))
  headers <- lapply(readable, function(k)
    .toc_header_names(srcs[[k]], detection$sheet[k], detection$header_row[k]))
  scores <- toc_pick_rules(if (length(headers)) headers else list(character()), rules)
  set_name <- pick_set %||% scores$rule_set[1L]
  set <- toc_rule_set(rules, set_name, overrides = c(choices$overrides, profile$overrides))
  # the columns: the set's map on each source's header; the user's map
  # where the source has its columns.  The dialog's selects show the first
  # source: a blank there ("none") leaves that source's column out, and
  # says nothing of another source's (a listing workbook's Population when
  # the TFL TOC has none)
  maps <- lapply(seq_along(srcs), function(k) {
    if (!k %in% readable) return(list())
    h <- headers[[match(k, readable)]]
    m <- toc_map_for(h, set)
    miss <- attr(m, "missing")
    for (it in names(choices$map)) {
      v <- choices$map[[it]]
      v <- v[!is.na(v) & nzchar(v)]
      if (!length(v)) {
        if (identical(k, readable[1L])) m[[it]] <- NULL
      } else if (all(.toc_norm(v) %in% .toc_norm(h))) {
        m[[it]] <- v
      }
    }
    attr(m, "missing") <- setdiff(miss, names(m))
    m
  })
  map <- if (length(readable)) maps[[readable[1L]]] else structure(list(), missing = character())
  rows <- do.call(rbind, c(list(.toc_no_rows()), lapply(readable, function(k)
    toc_rows(srcs[[k]], detection$sheet[k], detection$header_row[k], maps[[k]]))))
  # the output id and the type
  type_from <- choices$type_from %||% profile$type_from
  types <- .toc_types(rows, set, type_from)
  cands <- toc_id_candidates(rows, set, types$from)
  # the way chosen here; else the last time's, while it gives unique ids
  # to as many rows as the best way (a TOC of another layout: the best is
  # proposed, and said); else the rule set's; else the best
  id_rule <- choices$id_rule
  last_rule <- profile$id_rule
  if (is.null(id_rule) && !is.null(last_rule) && last_rule %in% cands$rule &&
      cands$ok[match(last_rule, cands$rule)] < cands$ok[1L]) {
    tpl <- "The report IDs as the last time (%s) are unique for fewer rows of this TOC: %s is proposed."
    notes <- rbind(notes, data.frame(message = sprintf(tpl, last_rule, cands$rule[1L]), template = tpl,
                                     file = last_rule, sheet = cands$rule[1L],
                                     stringsAsFactors = FALSE))
    last_rule <- cands$rule[1L]
  }
  id_rule <- id_rule %||% last_rule %||% set$settings$id_rule
  if (is.null(id_rule) || is.na(id_rule) || !id_rule %in% cands$rule) {
    id_rule <- if (nrow(cands)) cands$rule[1L] else "column"
  }
  labs <- .toc_labels(rows)
  # each row's cells as given (a heading row's text is in any column)
  given <- lapply(seq_len(nrow(rows)), function(i) {
    v <- c(unlist(rows[i, intersect(c(.toc_items, .toc_extra_items), names(rows))]),
           rows$titles[[i]], rows$footnotes[[i]])
    unique(v[!is.na(v)])
  })
  rows$raw_id <- rows$output_id
  rows$output_id <- .toc_ids_by(rows, id_rule, set, types)
  rows$output_id[!is.na(rows$output_id) & !nzchar(rows$output_id)] <- NA_character_
  rows$type <- types$type
  rows$type_column <- types$by$column
  rows$type_from <- vapply(seq_len(nrow(rows)), function(i) {
    k <- which(!is.na(unlist(types$by[i, c(types$from, setdiff(.toc_type_sources, types$from))])))
    if (length(k)) c(types$from, setdiff(.toc_type_sources, types$from))[k[1L]] else NA_character_
  }, "")
  rows$label <- labs$label
  rows$titles <- labs$titles
  # sections: the row's own, else the heading row above it (a row with no
  # id and one cell at most), per sheet
  cells_of <- function(i) given[[i]]
  n_cells <- vapply(seq_len(nrow(rows)), function(i) length(cells_of(i)), 1L)
  no_id <- is.na(rows$output_id)
  heading <- no_id & n_cells <= 1L
  cur <- NA_character_
  last_sheet <- ""
  sec <- rep(NA_character_, nrow(rows))
  for (i in seq_len(nrow(rows))) {
    here <- paste(rows$.source[i], rows$.sheet[i])
    if (!identical(here, last_sheet)) cur <- NA_character_
    last_sheet <- here
    if (heading[i]) cur <- cells_of(i)[1L]
    sec[i] <- if (!is.na(rows$section[i])) rows$section[i] else cur
  }
  rows$section <- sec
  rows$heading <- heading
  detection$rows <- vapply(detection$file, function(f) sum(rows$.source == f), 1L,
                           USE.NAMES = FALSE)
  has_title <- vapply(rows$titles, function(v) any(!is.na(v)), NA)
  # rows not read: no id (but more than a heading), or no title
  unread <- .toc_no_unread()
  for (i in which(no_id & !heading)) {
    unread[nrow(unread) + 1L, ] <- list(rows$.source[i], rows$.sheet[i], rows$.row[i],
                                        "TOC04", "output_id", NA_character_, NA_character_)
  }
  for (i in which(!no_id & !has_title)) {
    unread[nrow(unread) + 1L, ] <- list(rows$.source[i], rows$.sheet[i], rows$.row[i],
                                        "TOC04b", "title", NA_character_, rows$output_id[i])
  }
  take <- !no_id & has_title
  reports <- toc_expand_phases(rows[take, , drop = FALSE], set)
  unread <- rbind(unread, attr(reports, "unread"))
  attr(reports, "unread") <- NULL
  # the shells, across every source (not the TOC or Topline sheets)
  skip <- data.frame(source = detection$file,
                     sheet = detection$sheet, stringsAsFactors = FALSE)
  skip <- rbind(skip, data.frame(source = detection$file, sheet = detection$topline_sheet,
                                 stringsAsFactors = FALSE))
  for (k in seq_along(srcs)) {
    fs <- sheets[[k]]
    o <- fs$sheet[fs$role %in% c("other", "topline")]
    if (length(o)) skip <- rbind(skip, data.frame(source = srcs[[k]]$name, sheet = o,
                                                  stringsAsFactors = FALSE))
  }
  skip <- skip[!is.na(skip$sheet), , drop = FALSE]
  reports <- toc_link_shells(reports, srcs, set, skip)
  unlinked <- attr(reports, "unlinked")
  attr(reports, "unlinked") <- NULL
  # the Topline sheet, a cross-check: read through the same header
  # detection, map and id rule
  topline_ids <- NULL
  tl_where <- NULL
  for (k in seq_along(srcs)) {
    tl <- detection$topline_sheet[k]
    if (is.na(tl) || is.null(map$topline)) next
    hr <- toc_find_header(srcs[[k]]$head[[tl]], set)$row
    if (is.na(hr)) next
    tm <- toc_map_for(.toc_header_names(srcs[[k]], tl, hr), set)
    tr <- toc_rows(srcs[[k]], tl, hr, tm)
    tt <- .toc_types(tr, set, types$from)
    ids <- .toc_ids_by(tr, id_rule, set, tt)
    topline_ids <- unique(c(topline_ids, ids[!is.na(ids)]))
    tl_where <- tl
  }
  batches <- toc_batches(reports, set, topline_ids)
  reports$batches <- ifelse(reports$output_id %in% batches$output_id,
                            set$settings$topline_batch, NA_character_)
  res <- structure(list(
    sources = srcs, detection = detection, sheets = sheets,
    rule_set = set, rule_sets = scores, map = map, maps = maps,
    id_rule = id_rule, id_candidates = cands, type_from = types$from,
    rows = rows, reports = reports, unread = unread, batches = batches,
    unlinked = unlinked, notes = notes), class = "toc_import")
  res$problems <- .toc_find_problems(res, tl_where)
  res$profile <- .toc_profile_of(res, choices, profile)
  res
}

# a named vector's or list's element, NULL when it has none of that name
.toc_pick <- function(x, name) {
  if (is.null(x) || !name %in% names(x)) NULL else x[[name]]
}

#' @export
print.toc_import <- function(x, ...) {
  n <- table(factor(x$problems$level, c("error", "hand", "check")))
  cat(sprintf(paste0("<toc_import> %d source(s), rule set %s, id rule %s: %d reports ",
                     "(%d rows); %d errors, %d not read, %d checks\n"),
              length(x$sources), x$rule_set$name, x$id_rule, nrow(x$reports),
              nrow(x$rows), n[["error"]], n[["hand"]], n[["check"]]))
  invisible(x)
}

# Where a row is: its row number, with its file when there are several
.toc_where <- function(x, source, row) {
  if (length(x$sources) > 1L) paste(source, row) else as.character(row)
}

.toc_find_problems <- function(x, topline_sheet = NULL) {
  out <- list(.toc_no_problems())
  add <- function(...) out[[length(out) + 1L]] <<- .toc_problem(...)
  det <- x$detection
  set <- x$rule_set
  labs <- c(title = "the titles")
  for (k in seq_len(nrow(det))) {
    if (is.na(det$sheet[k])) next
    m <- x$maps[[k]]
    where <- paste0(det$file[k], ": ", det$sheet[k])
    if (is.null(m$title)) {
      add("TOC01", row = det$file[k], field = "title", args = c(labs[["title"]], where))
    } else if (nrow(x$rows) && !any(x$id_candidates$ok > 0L)) {
      add("TOC01b", row = det$file[k], field = "output_id", args = where)
    }
    if (isTRUE(det$two_toc[k]) && nzchar(det$others[k])) {
      add("TOC11", row = det$file[k],
          args = c(paste(c(det$sheet[k], det$others[k]), collapse = ", "), det$file[k],
                   det$sheet[k]))
    }
  }
  # rows not read
  u <- x$unread
  for (i in seq_len(nrow(u))) {
    w <- .toc_where(x, u$.source[i], u$.row[i])
    if (u$rule[i] == "TOC04") add("TOC04", row = w, field = "output_id", args = w)
    else if (u$rule[i] == "TOC04b") add("TOC04b", u$output_id[i], row = w, field = "title",
                                        args = c(w, u$output_id[i]))
    else add("TOC05", u$output_id[i], row = w, field = "phase",
             args = c(w, u$value[i], set$settings$phase_values))
  }
  r <- x$reports
  # ids twice (also across two workbooks), ids a file name cannot hold
  dup <- unique(r$output_id[duplicated(r$output_id)])
  for (id in dup) {
    i <- which(r$output_id == id)
    add("TOC02", id, row = paste(.toc_where(x, r$.source[i], r$.row[i]), collapse = ", "),
        field = "output_id",
        args = c(id, paste(.toc_where(x, r$.source[i], r$.row[i]), collapse = ", ")))
  }
  for (id in unique(r$output_id[grepl("[\\\\/:*?\"<>|]", r$output_id)])) {
    add("TOC03", id, field = "output_id", args = id)
  }
  miss <- attr(x$map, "missing") %||% character()
  # SAP numbers: blank, or on two reports (the phases of one row are one)
  if (!"sap_no" %in% miss && "sap_no" %in% set$map$item && .toc_has_item(x, "sap_no")) {
    # only the rows of a source whose TOC has the column: a listing workbook
    # with no SAP column says nothing of SAP numbers (#299: an optional item
    # whose column is absent is skipped silently)
    has <- vapply(x$maps %||% list(x$map), function(m) !is.null(m$sap_no), NA)
    from <- if (length(x$maps)) x$detection$file[has] else unique(r$.source)
    for (i in which(is.na(r$sap_no) & r$.source %in% from)) {
      add("TOC06", r$output_id[i], field = "sap_no", args = r$output_id[i])
    }
    key <- paste(r$.source, r$.sheet, r$.row)
    s <- r$sap_no
    for (v in unique(stats::na.omit(s))) {
      i <- which(s == v)
      if (length(unique(key[i])) > 1L) {
        add("TOC07", r$output_id[i[1L]], field = "sap_no",
            args = c(v, paste(r$output_id[i], collapse = ", ")))
      }
    }
  }
  # shells: none for a report; a shell sheet no report links to (only when
  # the sources hold shells at all)
  if (any(det$shells > 0L)) {
    for (i in which(is.na(r$shell_sheet))) add("TOC08", r$output_id[i], field = "shell",
                                               args = r$output_id[i])
    ul <- x$unlinked
    for (i in seq_len(nrow(ul))) {
      w <- paste0(ul$source[i], ": ", ul$sheet[i])
      add("TOC09", row = w, field = "shell", args = w)
    }
  }
  # the Topline sheet against the flag column
  d <- attr(x$batches, "differ")
  b <- set$settings$topline_batch
  for (i in seq_len(nrow(d))) {
    if (d$what[i] == "flagged_not_on_sheet") {
      add("TOC10", d$id[i], field = "topline", args = c(d$id[i], b, topline_sheet %||% ""))
    } else {
      add("TOC10b", d$id[i], field = "topline", args = c(d$id[i], topline_sheet %||% "", b))
    }
  }
  # optional items the set names that the TOC lacks: the feature skipped
  for (it in intersect(c("phase", "topline", "sap_no"), miss)) {
    add("TOC12", field = it, args = it)
  }
  p <- do.call(rbind, out)
  p <- p[order(match(p$level, c("error", "hand", "check"))), , drop = FALSE]
  rownames(p) <- NULL
  p
}

# What the study's profile keeps of an import (input/toc/profile.yml)
.toc_profile_of <- function(x, choices, profile) {
  det <- x$detection
  ov <- c(choices$overrides, profile$overrides)
  ov <- ov[!duplicated(names(ov))]
  list(rule_set = x$rule_set$name, id_rule = x$id_rule, type_from = x$type_from,
       overrides = if (length(ov)) ov else NULL,
       sources = lapply(seq_len(nrow(det)), function(k) {
         s <- list(file = det$file[k], toc_sheet = det$sheet[k],
                   header_row = det$header_row[k])
         if (!is.na(det$topline_sheet[k])) s$topline_sheet <- det$topline_sheet[k]
         s
       }))
}

# Whether any source's TOC has the item's column (x$map is the first
# source's: a listing workbook's Population when the TFL TOC has none)
.toc_has_item <- function(x, item) {
  any(vapply(x$maps %||% list(x$map), function(m) !is.null(m[[item]]), NA))
}

# The fixed map of the table toc_import_spec() hands to tfl_read_toc()
.toc_spec_frame <- function(x) {
  r <- x$reports
  n <- nrow(r)
  lines_of <- function(l) lapply(l, function(v) {
    v <- v[!is.na(v)]
    unlist(lapply(v, function(s) {
      p <- trimws(unlist(strsplit(s, "\n| \\| ")))
      p[nzchar(p)]
    }))
  })
  tl <- lines_of(r$titles)
  fl <- lines_of(r$footnotes)
  nt <- max(1L, lengths(tl))
  nf <- max(0L, lengths(fl))
  d <- data.frame(output_id = r$output_id, type = r$type, stringsAsFactors = FALSE)
  for (k in seq_len(nt)) d[[paste0("title_", k)]] <- vapply(tl, function(v)
    if (length(v) >= k) v[k] else NA_character_, "")
  d$population <- r$population
  for (k in seq_len(nf)) d[[paste0("footnote_", k)]] <- vapply(fl, function(v)
    if (length(v) >= k) v[k] else NA_character_, "")
  for (cn in c("program", "file", "note", "section", "datasets", "label")) d[[cn]] <- r[[cn]]
  map <- list(output_id = "output_id", type = "type",
              title = paste0("title_", seq_len(nt)), population = "population",
              program = "program", file = "file", note = "note", section = "section",
              datasets = "datasets", label = "label")
  if (nf) map$footnote <- paste0("footnote_", seq_len(nf))
  list(data = d, map = map)
}

#' @rdname toc_import_read
#' @return `toc_import_spec()`: what [tflspec::tfl_read_toc()] returns for
#'   the reports (a table spec with the report sheets and their
#'   attributes); `attr(, "guessed")` the reports whose type no type
#'   column said (read from the id or the title: to check).
#' @export
toc_import_spec <- function(x) {
  f <- .toc_spec_frame(x)
  d <- f$data
  if (!nrow(d)) {
    d[1L, ] <- NA
    d$output_id <- "(none)"
    d$title_1 <- "(none)"
  }
  sp <- .toc_read_frame(d, f$map)
  if (!nrow(f$data)) {
    sp$report <- sp$report[0L, , drop = FALSE]
    for (s in c("titles", "footnotes")) if (!is.null(sp[[s]])) sp[[s]] <- sp[[s]][0L, , drop = FALSE]
    for (a in c("sections", "datasets", "labels", "first_titles", "populations")) {
      attr(sp, a) <- attr(sp, a)[0L]
    }
  }
  r <- x$reports
  attr(sp, "guessed") <- r$output_id[is.na(r$type_column)]
  sp
}

# tflspec reads a data frame (D10); an older tflspec, the same rows as a
# .csv
.toc_read_frame <- function(d, map) {
  tryCatch(tflspec::tfl_read_toc(d, map = map), error = function(e) {
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f), add = TRUE)
    utils::write.csv(d, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    tflspec::tfl_read_toc(f, map = map)
  })
}
