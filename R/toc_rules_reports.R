# Taking a TOC in by the company's rules: phases, shell sheets and the
# Topline batch.  Split from toc_rules.R (no code change).

# ---- phases -----------------------------------------------------------------

# The title cell that gets a phase's suffix: `title_line` (1, 2, "first",
# "last") among the title columns, else the last cell given
.toc_title_target <- function(cells, title_line) {
  given <- which(!is.na(cells))
  if (!length(given)) return(NA_integer_)
  if (identical(title_line, "last")) return(given[length(given)])
  if (identical(title_line, "first")) return(given[1L])
  k <- suppressWarnings(as.integer(title_line))
  if (is.na(k) || k < 1L) return(given[length(given)])
  if (k <= length(cells) && !is.na(cells[k])) k else given[length(given)]
}

#' One report a phase
#'
#' A TOC row whose phase value means two phases (`both`) gives two reports,
#' one that means one phase gives one, each with the phase's suffix on its
#' output id and label (`number_suffix`) and on one title line
#' (`title_suffix` on `title_line`); the analysis set is not changed.  No
#' phase column: one report a row, no suffix.  A blank phase is
#' `phase_blank` (`none`: one report, no suffix; `all`: every phase; or a
#' value of `phase_values`).  A value the rules do not know leaves the row
#' out (listed in `attr(, "unread")`).
#'
#' @param rows The TOC's rows with their ids: [toc_rows()] plus
#'   `output_id`, `type`, `label`, `raw_id`.
#' @param rule_set A rule set (with a study's overrides).
#' @return The reports: a row a report, `output_id`, `base_id`, `phase`
#'   (1, 2, ... or `NA`), `label`, `titles` and `footnotes` (list
#'   columns), the rest of the row; `attr(, "unread")`: the rows left out.
#' @noRd
toc_expand_phases <- function(rows, rule_set) {
  st <- rule_set$settings
  nsuf <- .toc_list_keep(st$number_suffix)
  tsuf <- .toc_list_keep(st$title_suffix)
  pv <- .toc_phase_values(st$phase_values)
  n_ph <- max(length(nsuf), unlist(pv), 1L)
  has_phase <- any(!is.na(rows$phase))
  unread <- .toc_no_unread()
  out <- list()
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, , drop = FALSE]
    ph <- NA_integer_
    if (has_phase) {
      v <- r$phase
      if (is.na(v)) {
        b <- st$phase_blank
        ph <- if (identical(b, "all")) seq_len(n_ph) else
          if (identical(b, "none") || is.na(b)) NA_integer_ else pv[[.toc_norm(b)]] %||% NA_integer_
      } else {
        ph <- pv[[.toc_norm(v)]]
        if (is.null(ph)) {
          unread[nrow(unread) + 1L, ] <- list(r$.source, r$.sheet, r$.row, "TOC05", "phase",
                                              v, r$output_id)
          next
        }
      }
    }
    for (p in ph) {
      e <- r
      e$base_id <- r$output_id
      e$phase <- p
      if (!is.na(p)) {
        sfx <- if (p <= length(nsuf)) nsuf[p] else ""
        e$output_id <- paste0(r$output_id, sfx)
        if (!is.na(r$label)) e$label <- paste0(r$label, sfx)
        if (p <= length(tsuf)) {
          tc <- r$titles[[1L]]
          k <- .toc_title_target(tc, st$title_line)
          if (!is.na(k)) tc[k] <- paste0(tc[k], tsuf[p])
          e$titles <- list(tc)
        }
      }
      out[[length(out) + 1L]] <- e
    }
  }
  d <- if (length(out)) do.call(rbind, out) else {
    z <- rows[0L, , drop = FALSE]
    z$base_id <- character()
    z$phase <- integer()
    z
  }
  d$phase <- as.integer(d$phase)
  rownames(d) <- NULL
  attr(d, "unread") <- unread
  d
}

.toc_no_unread <- function() {
  data.frame(.source = character(), .sheet = character(), .row = integer(),
             rule = character(), field = character(), value = character(),
             output_id = character(), stringsAsFactors = FALSE)
}

# ---- shells -------------------------------------------------------------------

#' Each report's shell sheet
#'
#' Across every source, the sheet named as the report's shell (its `shell`
#' column), its id column as it is or its output id -- the phase's
#' `shell_phase_suffix` first (`t.14.1.1-1`), else without -- matched
#' ignoring case; then the same by key (`T-14-1-1` is `t.14.1.1`).  Only the
#' link is kept: nothing of the sheet is read.
#'
#' @param reports What [toc_expand_phases()] returned.
#' @param sources The `toc_source`s.
#' @param rule_set A rule set.
#' @param skip Sheets that are not shells: a data frame `source`, `sheet`
#'   (the TOC sheets, the Topline sheet).
#' @return `reports` with `shell_file`, `shell_sheet` (`NA`: none);
#'   `attr(, "unlinked")`: sheets named like a report number no report
#'   links to (`source`, `sheet`).
#' @noRd
toc_link_shells <- function(reports, sources, rule_set, skip = NULL) {
  sh <- do.call(rbind, lapply(sources, function(s) {
    if (!length(s$sheets)) return(NULL)
    data.frame(source = s$name, sheet = s$sheets, stringsAsFactors = FALSE)
  }))
  if (is.null(sh)) sh <- data.frame(source = character(), sheet = character())
  if (!is.null(skip) && nrow(skip)) {
    sh <- sh[!paste(sh$source, sh$sheet) %in% paste(skip$source, skip$sheet), , drop = FALSE]
  }
  low <- tolower(trimws(sh$sheet))
  key <- .toc_key(sh$sheet)
  psuf <- .toc_list_keep(rule_set$settings$shell_phase_suffix)
  used <- rep(FALSE, nrow(sh))
  reports$shell_file <- rep(NA_character_, nrow(reports))
  reports$shell_sheet <- rep(NA_character_, nrow(reports))
  for (i in seq_len(nrow(reports))) {
    r <- reports[i, , drop = FALSE]
    bases <- unique(stats::na.omit(c(r$shell, r$raw_id, r$base_id)))
    sfx <- if (!is.na(r$phase) && r$phase <= length(psuf)) psuf[r$phase] else character()
    tries <- list(
      function() match(tolower(paste0(bases, if (length(sfx)) sfx else NA)), low),
      function() match(tolower(bases), low),
      function() if (length(sfx)) match(.toc_key(paste0(bases, sfx)), key) else NA,
      function() match(.toc_key(bases), key))
    for (f in tries) {
      k <- f()
      k <- k[!is.na(k)][1L]
      if (!is.na(k)) {
        reports$shell_file[i] <- sh$source[k]
        reports$shell_sheet[i] <- sh$sheet[k]
        used[k] <- TRUE
        break
      }
    }
  }
  named <- .toc_shell_named(sub("-[0-9]+$", "", sh$sheet))
  attr(reports, "unlinked") <- sh[named & !used, , drop = FALSE]
  reports
}

# ---- batches ------------------------------------------------------------------

#' The Topline batch of a TOC
#'
#' The reports whose Topline flag is one of the set's `topline_values` (a
#' row of two phases gives both reports) form the batch `topline_batch`.
#' No flag column: no batch.  The full run is every report and is not a
#' batch.  A Topline sheet, when there is one, is compared: a report
#' flagged but not on it, or on it but not flagged, is listed (a check,
#' never an error; the flag decides).
#'
#' @param reports What [toc_link_shells()] (or [toc_expand_phases()])
#'   returned.
#' @param rule_set A rule set.
#' @param topline_ids The output ids (before the phases) of a Topline
#'   sheet, or `NULL`.
#' @return A data frame `batch`, `output_id`; `attr(, "differ")`: `id`,
#'   `what` (`"flagged_not_on_sheet"`, `"on_sheet_not_flagged"`).
#' @noRd
toc_batches <- function(reports, rule_set, topline_ids = NULL) {
  out <- data.frame(batch = character(), output_id = character(),
                    stringsAsFactors = FALSE)
  differ <- data.frame(id = character(), what = character(), stringsAsFactors = FALSE)
  if (!nrow(reports) || all(is.na(reports$topline))) {
    attr(out, "differ") <- differ
    return(out)
  }
  st <- rule_set$settings
  vals <- .toc_norm(.toc_cands(st$topline_values))
  on <- !is.na(reports$topline) & .toc_norm(reports$topline) %in% vals
  if (any(on)) {
    out <- data.frame(batch = st$topline_batch, output_id = reports$output_id[on],
                      stringsAsFactors = FALSE)
  }
  if (!is.null(topline_ids)) {
    flagged <- unique(reports$base_id[on])
    tk <- .toc_key(topline_ids)
    fk <- .toc_key(flagged)
    a <- flagged[!fk %in% tk]
    b <- topline_ids[!tk %in% .toc_key(reports$base_id[on])]
    differ <- data.frame(id = c(a, b),
                         what = c(rep("flagged_not_on_sheet", length(a)),
                                  rep("on_sheet_not_flagged", length(b))),
                         stringsAsFactors = FALSE)
  }
  attr(out, "differ") <- differ
  out
}
