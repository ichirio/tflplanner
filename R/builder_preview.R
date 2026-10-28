# The table builder's ARD card summary and the preview of the table as it
# will print.  Split from builder.R (no code change).

# What a report's ARD holds, as the builder's card says it (#343): `meta`
# is ard_info()'s (the keys, the variables and their statistics, when it
# was read); `page_by`: the layout's page variables.  `line`: the one-line
# summary; `rows`: role, variable, kind, the levels or statistics, the
# label (NULL with no meta).  `tr`: the app's translation.
ard_card_summary <- function(meta, page_by = character(), tr = function(x) x) {
  if (is.null(meta) || (!length(meta$by) && !NROW(meta$variables))) {
    return(list(line = tr("ARD: not made yet (Update preview makes it)"),
                rows = NULL))
  }
  cut <- function(x, k) if (length(x) > k) c(utils::head(x, k), "\u2026") else x
  levels_of <- function(l) {
    l <- as.character(l)
    if (!length(l)) return("")
    sprintf(tr("%d levels: %s"), length(l), paste(cut(l, 4L), collapse = ", "))
  }
  by <- meta$by
  hier <- setdiff(meta$hierarchy %||% character(), page_by)
  v <- meta$variables
  if (is.null(v)) v <- data.frame(variable = character(), kind = character())
  v <- v[!v$variable %in% c(by, hier, page_by), , drop = FALSE]
  n_cat <- sum(v$kind %in% "categorical")
  n_con <- sum(v$kind %in% "continuous")
  st <- unique(stats::na.omit(as.character(meta$stats %||% character())))
  when <- meta$fetched %||% NULL
  parts <- c(
    if (length(by)) sprintf(tr("columns %s (%s groups)"), paste(by, collapse = " \u00d7 "),
                            paste(lengths(meta$keys[by]), collapse = " \u00d7 ")),
    if (length(page_by)) sprintf(tr("pages %s"), paste(page_by, collapse = ", ")),
    if (length(hier)) sprintf(tr("rows %s"), paste(hier, collapse = " \u203a ")),
    # a hierarchy table with no analysis variable says nothing of them
    if (nrow(v) || !length(hier))
      sprintf(tr("%d variables (%d categorical, %d continuous)"), nrow(v), n_cat, n_con),
    if (length(st)) sprintf(tr("statistics %s"), paste0(
      paste(utils::head(st, 6L), collapse = ", "), if (length(st) > 6L) " \u2026")),
    if (!is.null(when) && !all(is.na(when)))
      sprintf(tr("read %s"), format(as.POSIXct(when), "%m/%d %H:%M")))
  line <- paste0("ARD: ", paste(parts, collapse = " | "))
  row <- function(role, var, kind, detail, label = NA_character_) {
    data.frame(role = role, variable = var, kind = kind, detail = detail,
               label = label, stringsAsFactors = FALSE)
  }
  lab <- function(x) {
    if (!"label" %in% names(v)) return(NA_character_)
    l <- v$label[match(x, v$variable)]
    if (is.na(l) || identical(l, x)) NA_character_ else l
  }
  rows <- c(
    lapply(by, function(k) row(tr("column"), k, tr("group"), levels_of(meta$keys[[k]]))),
    lapply(page_by, function(k) row(tr("page split"), k, tr("group"), levels_of(meta$keys[[k]]))),
    lapply(hier, function(k) row(tr("row: hierarchy"), k, tr("hierarchy"),
                                 levels_of(meta$keys[[k]]))),
    lapply(seq_len(nrow(v)), function(i) {
      cont <- identical(v$kind[i], "continuous")
      det <- if (cont) gsub(" | ", ", ", v$stats[i] %||% "", fixed = TRUE) else
        levels_of(.split_list(v$levels[i] %||% NA_character_))
      row(tr("row: variable"), v$variable[i],
          tr(if (cont) "continuous" else "categorical"), det, lab(v$variable[i]))
    }))
  list(line = line, rows = do.call(rbind, rows))
}

# ------------------------------------------------------------ preview

#' The table as it will print
#'
#' Plans the report's table from the definition as it stands (saved or
#' not) and the report's normalized data, and gives its pages -- what
#' [tflspec::tfl_report()] lays out -- or an HTML rendering of them.
#'
#' @param x An `tflplanner`.
#' @param output_id The report.
#' @param data Its normalized data ([ard_data()]).
#' @param pages `rtftable` pages.
#' @param max_pages How many pages to render.
#' @param align How value cells align: `"center"` (a table) or `"left"` (a
#'   listing).
#' @return `preview_pages()`: a list of `rtftable`; `preview_html()`: HTML.
#' @export
preview_pages <- function(x, output_id, data) {
  sheets <- lapply(x$sheets[table_sheets()], function(d) {
    d <- d[is.na(d$output_id) | d$output_id == output_id, , drop = FALSE]
    if (all(is.na(d$note))) d$note <- NULL
    d
  })
  st <- x$study["rounding"]
  sheets$study <- if (!is.na(st)) st
  spec <- tflspec::tfl_table_spec(sheets)
  plan <- rtfreporter::plan_cells(tflspec::tfl_table_plan(data, spec, output_id),
                                  notes = FALSE)
  res <- suppressMessages(rtfreporter::plan_apply(plan))
  # what the column header's tokens hold here (the builder's insert chips)
  tokens <- tryCatch(rtfreporter::plan_header_tokens(plan),
                     error = function(e) NULL)
  # a plan with no layout gives its table, not pages: one page of it
  if (is.data.frame(res)) {
    res <- list(structure(list(data = res, col_header = list(names(res)),
                               blank_rows = integer()),
                          class = "rtftable"))
  }
  if (inherits(res, "rtftable")) res <- list(res)
  attr(res, "header_tokens") <- tokens
  # the populations a header's {n} can say, for the builder's choice
  attr(res, "n_candidates") <- tryCatch(rtfreporter::plan_n_candidates(plan),
                                        error = function(e) NULL)
  res
}

# The table's data for the preview from its ARD as read (ard_raw(), #428):
# normalized here with normalize_ard()'s arguments as the report's item
# says them now -- the overall row, the hierarchy, the stack -- so a change
# of them shows without making the ARD again.  The same call the program
# writes (its plan's pipe); NULL when the report has no such item.
.preview_normalized <- function(x, output_id, ard) {
  its <- builder_items(x, output_id)
  it <- Filter(function(i) identical(i$fn, "normalize_ard"), its)
  args <- if (length(it)) it[[1L]]$args %||% list() else list()
  txt <- tflspec::tfl_args_to_r("normalize_ard", args)
  call <- if (length(txt) && nzchar(txt)) paste0("rtfreporter::normalize_ard(.ard, ", txt, ")") else
    "rtfreporter::normalize_ard(.ard)"
  eval(parse(text = call), envir = list2env(list(.ard = ard), parent = baseenv()))
}

# The insert chips' labels: a token and what it holds here
# ("{n} = 86 / 84 / 84"); the token alone when the preview has not said
header_token_labels <- function(choices, tokens = NULL) {
  if (is.null(tokens) || !nrow(tokens)) return(stats::setNames(choices, choices))
  lab <- vapply(choices, function(tk) {
    k <- match(tk, tokens$token)
    if (is.na(k) || !isTRUE(tokens$resolved[k])) return(tk)
    v <- unlist(tokens$values[[k]], use.names = FALSE)
    v <- v[!is.na(v)]
    if (!length(v)) return(tk)
    if (length(v) > 4L) v <- c(utils::head(v, 4L), "\u2026")
    paste(tk, "=", paste(v, collapse = " / "))
  }, "")
  stats::setNames(choices, lab)
}

# The lines of a report's header / titles / footnotes / footer: its own,
# and the study defaults for the lines it has not (by line number).
.page_lines <- function(x, sheet, output_id) {
  # the report's own over its pattern's over Standard's; a "(none)" line
  # prints nothing (the line of that number below is left out)
  d <- page_lines_from(x, sheet, output_id)
  d <- d[!d$omitted, setdiff(names(d), c("from", "omitted")), drop = FALSE]
  rownames(d) <- NULL
  d
}

# A page's sample in HTML: each line in three parts (left, centre, right),
# the {PLACEHOLDERS} filled as a first page would have them.
.page_sample_html <- function(x, output_id, study_id, body, program = "") {
  # the report's tokens, as its program gives them ({OUTPUT_LABEL} ...)
  tok <- tryCatch(tflspec::tfl_report_tokens(
    .spec_object_last(x, c(table_sheets(), report_sheets()),
                      unique(c(.study_keys$table, .study_keys$report))),
    output_id), error = function(e) character())
  # a study with no STUDY_ID token: its id, as before
  if (is.na(tok["STUDY_ID"]) || !nzchar(tok[["STUDY_ID"]])) {
    tok <- tok[names(tok) != "STUDY_ID"]
  }
  rx <- "\\{[A-Z][A-Z0-9_]*\\}"
  # a line whose tokens are all empty, the rest blanks or brackets, is not
  # printed (rtfreporter's drop_empty_rows)
  empty <- function(cells) {
    txt <- paste(cells[!is.na(cells)], collapse = " ")
    m <- regmatches(txt, gregexpr(rx, txt))[[1L]]
    nm <- substr(m, 2L, nchar(m) - 1L)
    length(m) && all(nm %in% names(tok)) && !any(nzchar(trimws(tok[nm]))) &&
      !nzchar(gsub("[][[:space:]<>():;,.|/-]", "", gsub(rx, "", txt)))
  }
  fill <- function(s) {
    if (is.na(s)) return("")
    for (k in names(tok)) s <- gsub(paste0("{", k, "}"), tok[[k]], s, fixed = TRUE)
    s <- gsub("{PAGE}", "1", s, fixed = TRUE)
    s <- gsub("{TOTAL_PAGES}", "N", s, fixed = TRUE)
    s <- gsub("{STUDY_ID}", study_id, s, fixed = TRUE)
    s <- gsub("{PROGRAM}", program %||% "", s, fixed = TRUE)
    s <- gsub("{DATETIME}", format(Sys.time(), "%Y-%m-%d %H:%M"), s, fixed = TRUE)
    s <- gsub("{output_id}", output_id, s, fixed = TRUE)
    s
  }
  block <- function(sheet, cls) {
    d <- .page_lines(x, sheet, output_id)
    for (k in .toc_cells) if (!k %in% names(d)) d[[k]] <- rep(NA_character_, nrow(d))
    d <- d[!vapply(seq_len(nrow(d)), function(i)
      empty(unlist(d[i, .toc_cells])), NA), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    lapply(seq_len(nrow(d)), function(i) htmltools::div(
      class = paste("rp-page-line", cls),
      htmltools::span(class = "l", fill(d$left[i])),
      htmltools::span(class = "c", fill(d$center[i])),
      htmltools::span(class = "r", fill(d$right[i]))))
  }
  pg <- rbind(sheet_rows(x, "page", output_id), inherited_rows(x, "page", output_id))
  land <- any(tolower(pg$orientation) %in% "landscape")
  htmltools::div(
    class = paste("rp-page", if (land) "rp-landscape"),
    htmltools::tags$style(htmltools::HTML("
      .rp-page { border: 1px solid #ccc; padding: .6rem .8rem; font-size: .7rem;
                 font-family: 'Courier New', monospace; background: #fff; }
      .rp-page-wrap { overflow-x: auto; max-width: 100%; }
      .rp-page-wrap .rp-page { width: max-content; min-width: 100%; }
      .rp-page-wrap .rp-page-body { overflow-x: visible; }
      .rp-page-full .rp-page { zoom: 1 !important; }
      .rp-page.rp-landscape { min-width: 60rem; }
      .rp-page-line { display: grid; grid-template-columns: 1fr auto 1fr; gap: .5rem; }
      .rp-page-line .c { text-align: center; } .rp-page-line .r { text-align: right; }
      .rp-page-body { margin: .5rem 0; overflow-x: auto; }
      .rp-page-foot { border-top: 1px solid #ddd; margin-top: .4rem; padding-top: .2rem; }")),
    block("header", "head"),
    block("titles", "title"),
    htmltools::div(class = "rp-page-body", body),
    block("footnotes", "note"),
    htmltools::div(class = "rp-page-foot", block("footer", "foot")))
}

#' @rdname preview_pages
#' @export
preview_html <- function(pages, max_pages = 3L, align = "center") {
  one <- function(pg, i) {
    d <- pg$data
    blank_after <- attr(d, "rtf_blank_rows") %||% pg$blank_rows %||%
      integer()
    stub <- attr(d, "rtf_stub_src")
    val <- function(v) if (is.na(v)) "" else as.character(v)
    text <- function(x) htmltools::HTML(gsub("\n", "<br>", htmltools::htmlEscape(
      val(x)), fixed = TRUE))
    # a header cell's own look, as the RTF prints it: bold, a rule above or
    # below it (the builder's "underline"), its alignment
    look <- function(x) {
      line <- function(b) !is.null(b) && !identical(b$style, "none")
      css <- c(if (isTRUE(x$bold)) "font-weight: bold;",
               if (line(x$border$top)) "border-top: 1px solid #333;",
               if (line(x$border$bottom)) "border-bottom: 1px solid #333;",
               if (!is.null(x$align) && x$align %in% c("left", "center", "right"))
                 paste0("text-align: ", x$align, ";"))
      if (length(css)) paste(css, collapse = " ")
    }
    head <- lapply(pg$col_header, function(line) {
      # a line of spanning cells (list(from, to, label)): each over its
      # columns, the columns no cell covers left empty
      spans <- is.list(line) && length(line) &&
        all(vapply(line, function(x) is.list(x) && !is.null(x$from), NA))
      if (spans) {
        cells <- list()
        at <- 1L
        for (x in line[order(vapply(line, function(x) as.integer(x$from), 1L))]) {
          from <- as.integer(x$from)
          to <- as.integer(x$to %||% x$from)
          while (at < from) {
            cells <- c(cells, list(htmltools::tags$th(
              class = if (at == 1L) "rp-pv-stub" else "rp-pv-val")))
            at <- at + 1L
          }
          cells <- c(cells, list(htmltools::tags$th(
            colspan = to - from + 1L,
            class = if (from == 1L) "rp-pv-stub" else "rp-pv-val rp-pv-span",
            style = look(x),
            text(x$label %||% ""))))
          at <- to + 1L
        }
        return(htmltools::tags$tr(cells))
      }
      htmltools::tags$tr(lapply(seq_along(line), function(k)
        htmltools::tags$th(
          class = if (k == 1L) "rp-pv-stub" else "rp-pv-val",
          text(line[[k]]))))
    })
    blank <- htmltools::tags$tr(class = "rp-pv-blank",
                                htmltools::tags$td(colspan = ncol(d),
                                                   htmltools::HTML("&nbsp;")))
    body <- list()
    if (0L %in% blank_after) body <- c(body, list(blank))
    for (r in seq_len(nrow(d))) {
      group <- !is.null(stub) && is.na(stub[r])
      body <- c(body, list(htmltools::tags$tr(
        class = if (group) "rp-pv-group",
        lapply(seq_len(ncol(d)), function(k) htmltools::tags$td(
          class = if (k == 1L) {
            if (!is.null(stub) && !group) "rp-pv-stub rp-pv-indent" else
              "rp-pv-stub"
          } else "rp-pv-val",
          val(d[r, k]))))))
      if (r %in% blank_after) body <- c(body, list(blank))
    }
    htmltools::tagList(
      if (length(pages) > 1L) htmltools::div(
        class = "rp-pv-page", sprintf("%d / %d", i, length(pages))),
      htmltools::tags$table(
        class = paste(c("rp-pv", if (identical(align, "left")) "rp-pv-left"),
                      collapse = " "),
        htmltools::tags$thead(head), htmltools::tags$tbody(body)))
  }
  n <- min(length(pages), max_pages)
  htmltools::tagList(lapply(seq_len(n), function(i) one(pages[[i]], i)))
}
