# The table builder: a summary table (columns = one key, rows = variables)
# described the way one thinks of it -- which arms in which order, which
# variables in which order with which label and levels, which statistics
# with how many decimals -- instead of sheet rows.  It reads that from the
# same sheets the grids show and writes it back to them, so the two views
# never disagree; and it shows the table as it will print.

#' The statistics the builder offers for a continuous variable
#'
#' Each has its row label, its template and its digits as a function of
#' the decimals the data are collected with (`d`): the usual convention is
#' the mean and median one more, the SD two more, the extremes as
#' collected.
#'
#' @return A data frame: `key`, `row`, `template`, `digits` (a function of
#'   `d`).
#' @export
builder_stats <- function() {
  d <- company_standards()$statistics
  d[c("key", "row", "template", "digits")]
}

# the statistics a new table shows for a continuous variable
.builder_default_stats <- c("n", "mean_sd", "median", "min_max")

# The statistics a template reads: "{mean} ({sd:.2f})" -> mean, sd
.template_stats <- function(tpl) {
  tok <- regmatches(tpl, gregexpr("\\{[^}:]+", tpl))[[1L]]
  unique(substring(tok, 2L))
}

# For each builder statistic, what its template needs that the ARD does not
# have ("" when the ARD has it all).  With no statistics known, nothing is
# said to be missing.
.builder_stats_lacking <- function(templates, have) {
  if (!length(have)) return(rep("", length(templates)))
  vapply(templates, function(tp)
    paste(setdiff(.template_stats(tp), have), collapse = ", "), "",
    USE.NAMES = FALSE)
}

# a statistic's digits from its rule: "d", "d+1", "d+1,d+2", "0" ...
.stat_digits <- function(key, d) {
  rule <- builder_stats()$digits[match(key, builder_stats()$key)]
  if (is.na(rule)) return(NA_character_)
  parts <- trimws(strsplit(rule, ",")[[1L]])
  paste(vapply(parts, function(x) {
    x <- gsub("d", as.character(d), x, fixed = TRUE)
    as.character(eval(parse(text = x), baseenv()))
  }, ""), collapse = ",")
}

# the categorical formats: key, label, template with <p> for the decimals
.cat_formats <- function() company_standards()$categorical_formats

.cat_template <- function(key, pct) {
  f <- .cat_formats()
  gsub("<p>", as.character(pct), f$template[match(key, f$key)], fixed = TRUE)
}

# which format, with how many decimals, a template is
.cat_read <- function(tpl) {
  f <- .cat_formats()
  if (is.na(tpl)) return(list(key = f$key[1L], pct = 1))
  for (i in seq_len(nrow(f))) {
    for (p in 0:3) {
      if (identical(gsub("<p>", p, f$template[i], fixed = TRUE), tpl)) {
        return(list(key = f$key[i], pct = p))
      }
    }
  }
  list(key = f$key[1L], pct = 1)
}

.split_list <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(character())
  trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
}

# a report's rows of a sheet, its own before the defaults it inherits
.rows_for <- function(p, sheet, id) {
  rbind(sheet_rows(p, sheet, id), inherited_rows(p, sheet, id))
}

#' Read and write a table the builder way
#'
#' `builder_read()` describes a report's summary table from its sheets (and
#' what its ARD holds, for what the sheets do not say yet);
#' `builder_write()` writes such a description back to the report's own
#' rows of `tables`, `variables`, `cells` and `col_header`.  Rows it does
#' not manage (a statistic it does not offer, a variable of its own
#' template) are left as they are.
#'
#' @param x An `tflplanner`.
#' @param output_id The report.
#' @param meta Its [ard_meta()], or `NULL`.
#' @param state What `builder_read()` returns, as edited.
#' @return `builder_read()`: a list -- `key` (the column variables,
#'   outermost first), `arms` (a named list: each column variable's levels
#'   in order), `variables` (a data frame: `variable`,
#'   `kind`, `label`), `levels` (named list), `stats` (keys of
#'   [builder_stats()] in order), `decimals`, `cat_format` (`npct`,
#'   `nNpct`, `n`), `pct_decimals`, `header` (`keep` or a name of
#'   [header_presets()]).  `builder_write()`: the `tflplanner`.
#' @export
builder_read <- function(x, output_id, meta = NULL) {
  id <- output_id
  tb <- .rows_for(x, "tables", id)
  # the column variables, outermost first (tables$cols "A | B")
  key <- .split_list(tb$cols[1L])
  if (!length(key) && !is.null(meta) && length(meta$by)) key <- meta$by[1L]
  if (!length(key)) key <- NA_character_

  vr <- .rows_for(x, "variables", id)
  vr <- vr[!duplicated(vr$variable), , drop = FALSE]
  # levels no sheet states are the ARD's order; remembered, so that
  # writing them back unchanged writes nothing
  auto <- list()
  lev_of <- function(v) {
    l <- .split_list(vr$levels[match(v, vr$variable)])
    if (!length(l) && !is.null(meta)) {
      l <- if (v %in% names(meta$keys)) meta$keys[[v]] else
        .split_list(meta$variables$levels[match(v, meta$variables$variable)])
      auto[[v]] <<- l
    }
    l
  }
  arms <- if (!anyNA(key)) stats::setNames(lapply(key, lev_of), key) else
    list()

  mv <- if (!is.null(meta)) meta$variables else
    data.frame(variable = character(), kind = character(),
               label = character(), stringsAsFactors = FALSE)
  own <- vr[!is.na(vr$variable) & !vr$variable %in% c(key, names(meta$keys)),
            , drop = FALSE]
  vars <- unique(c(mv$variable, own$variable))
  ord <- suppressWarnings(as.numeric(vr$order[match(vars, vr$variable)]))
  vars <- vars[order(is.na(ord), ord, match(vars, mv$variable))]
  label <- vr$label[match(vars, vr$variable)]
  mlab <- mv$label[match(vars, mv$variable)]
  label[is.na(label)] <- mlab[is.na(label)]
  kind <- mv$kind[match(vars, mv$variable)]
  kind[is.na(kind)] <- ifelse(
    vapply(vars[is.na(kind)], function(v) length(lev_of(v)) > 0, NA),
    "categorical", "continuous")
  variables <- data.frame(variable = vars, kind = kind, label = label,
                          stringsAsFactors = FALSE)
  levels <- stats::setNames(lapply(vars, function(v)
    if (identical(kind[match(v, vars)], "categorical")) lev_of(v) else
      character()), vars)

  ce <- .rows_for(x, "cells", id)
  bs <- builder_stats()
  cont <- ce[!is.na(ce$variable) & ce$variable == "continuous", , drop = FALSE]
  stats <- bs$key[match(cont$row, bs$row)]
  stats <- stats[!is.na(stats)]
  dig <- function(k) {
    v <- cont$digits[match(bs$row[bs$key == k], cont$row)]
    suppressWarnings(as.numeric(strsplit(v %||% "", ",")[[1L]][1L]))
  }
  decimals <- if ("min_max" %in% stats && !is.na(dig("min_max"))) {
    dig("min_max")
  } else if ("median" %in% stats && !is.na(dig("median"))) {
    dig("median") - 1
  } else 0
  if (!length(stats)) stats <- .builder_default_stats

  cat_row <- ce[(!is.na(ce$variable) & ce$variable == "categorical") |
                  (is.na(ce$variable) & is.na(ce$row)), , drop = FALSE]
  cr <- .cat_read(cat_row$template[1L])
  cat_format <- cr$key
  pct_decimals <- cr$pct

  list(key = key, arms = arms, variables = variables, levels = levels,
       stats = stats, decimals = decimals, cat_format = cat_format,
       pct_decimals = pct_decimals, header = "keep", auto_levels = auto)
}

#' @rdname builder_read
#' @export
builder_write <- function(x, output_id, state) {
  id <- output_id
  st <- state
  # tables: the column key, and one group per variable unless said otherwise
  tb <- sheet_rows(x, "tables", id)
  tb$output_id <- NULL
  if (!nrow(tb)) tb[1L, ] <- NA
  if (!anyNA(st$key)) tb$cols[1L] <- paste(st$key, collapse = " | ")
  # whose {n} the header prints (the form asks only when a cell uses {n})
  if (!is.null(st$header_n) && "header_n" %in% names(tb)) tb$header_n[1L] <- st$header_n
  if (is.na(tb$rows[1L])) {
    inh <- inherited_rows(x, "tables", id)
    if (!nrow(inh) || is.na(inh$rows[1L])) tb$rows[1L] <- "group = variable"
  }
  x <- set_sheet_rows(x, "tables", id, tb)

  # variables: the arms' order, each variable's label, order and levels
  vr <- sheet_rows(x, "variables", id)
  vr$output_id <- NULL
  put <- function(vr, v, ...) {
    val <- list(...)
    i <- match(v, vr$variable)
    if (is.na(i)) {
      vr[nrow(vr) + 1L, ] <- NA
      i <- nrow(vr)
      vr$variable[i] <- v
    }
    for (k in names(val)) vr[[k]][i] <- val[[k]]
    vr
  }
  j <- function(l) if (length(l)) paste(l, collapse = " | ") else NA_character_
  lv <- function(v, l) {
    if (identical(l, st$auto_levels[[v]])) NA_character_ else j(l)
  }
  if (!anyNA(st$key)) for (k in st$key) {
    vr <- put(vr, k, levels = lv(k, st$arms[[k]] %||% st$auto_levels[[k]]))
  }
  v <- st$variables
  for (i in seq_len(nrow(v))) {
    vr <- put(vr, v$variable[i],
              label = if (is.na(v$label[i]) || !nzchar(v$label[i])) NA else
                v$label[i],
              order = as.character(i),
              levels = if (identical(v$kind[i], "categorical"))
                lv(v$variable[i], st$levels[[v$variable[i]]]) else
                  NA_character_)
  }
  empty <- rowSums(!is.na(vr[setdiff(names(vr), "variable")])) == 0 &
    vr$variable %in% st$key
  x <- set_sheet_rows(x, "variables", id, vr[!empty, , drop = FALSE])

  # cells: the continuous statistics and the categorical format it manages
  ce <- sheet_rows(x, "cells", id)
  ce$output_id <- NULL
  bs <- builder_stats()
  mine <- (!is.na(ce$variable) & ce$variable == "continuous" &
             ce$row %in% bs$row) |
    (!is.na(ce$variable) & ce$variable == "categorical" & is.na(ce$row))
  keep <- ce[!mine, , drop = FALSE]
  new <- ce[0, , drop = FALSE]
  # a table of categorical variables only has no statistics to state
  had <- any(!is.na(ce$variable) & ce$variable == "continuous")
  stats <- if (had || any(st$variables$kind == "continuous")) st$stats else
    character()
  for (k in stats) {
    b <- bs[bs$key == k, ]
    new[nrow(new) + 1L, ] <- NA
    new$variable[nrow(new)] <- "continuous"
    new$row[nrow(new)] <- b$row
    new$template[nrow(new)] <- b$template
    new$digits[nrow(new)] <- .stat_digits(k, st$decimals)
  }
  inh <- inherited_rows(x, "cells", id)
  inh_cont <- inh[!is.na(inh$variable) & inh$variable == "continuous", ,
                  drop = FALSE]
  same <- function(a, b) {
    identical(paste(a$row, a$template, a$digits),
              paste(b$row, b$template, b$digits))
  }
  if (nrow(inh_cont) && same(new, inh_cont)) new <- new[0, , drop = FALSE]
  tpl <- .cat_template(st$cat_format, st$pct_decimals)
  inh_cat <- inh[(!is.na(inh$variable) & inh$variable == "categorical") |
                   (is.na(inh$variable) & is.na(inh$row)), , drop = FALSE]
  if (!identical(inh_cat$template[1L], tpl)) {
    new[nrow(new) + 1L, ] <- NA
    new$variable[nrow(new)] <- "categorical"
    new$template[nrow(new)] <- tpl
  }
  x <- set_sheet_rows(x, "cells", id, rbind(new, keep))

  # the column header: the form's rows (a data frame), written as the
  # report's own only when they differ from the header it has now
  if (is.data.frame(st$header)) {
    own <- sheet_rows(x, "col_header", id)
    own$output_id <- NULL
    eff <- if (nrow(own)) own else {
      inh <- inherited_rows(x, "col_header", id)
      inh$output_id <- NULL
      inh
    }
    if (!.same_header(eff, st$header)) {
      x <- set_sheet_rows(x, "col_header", id, st$header)
    }
  } else if (!identical(st$header, "keep") &&
             st$header %in% names(header_presets())) {
    x <- add_preset(x, id, st$header)
  }
  x
}

# Do two sets of col_header rows say the same?
.same_header <- function(a, b) {
  cols <- c("line", "cols", "span", "text", "align", "bold", "border_top",
            "border_bottom")
  norm <- function(d) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    for (k in cols) if (!k %in% names(d)) d[[k]] <- NA_character_
    d <- d[cols]
    d[] <- lapply(d, function(v) {
      v <- as.character(v)
      v[!is.na(v) & !nzchar(v)] <- NA_character_
      v
    })
    rownames(d) <- NULL
    d
  }
  identical(norm(a), norm(b))
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
  res
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
  own <- sheet_rows(x, sheet, output_id)
  inh <- inherited_rows(x, sheet, output_id)
  inh <- inh[!inh$line %in% own$line, , drop = FALSE]
  d <- rbind(own, inh)
  d[order(suppressWarnings(as.numeric(d$line))), , drop = FALSE]
}

# A page's sample in HTML: each line in three parts (left, centre, right),
# the {PLACEHOLDERS} filled as a first page would have them.
.page_sample_html <- function(x, output_id, study_id, body, program = "") {
  fill <- function(s) {
    if (is.na(s)) return("")
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
                 font-family: 'Courier New', monospace; background: #fff;
                 zoom: .6; }
      .rp-page-full .rp-page { zoom: 1; }
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
