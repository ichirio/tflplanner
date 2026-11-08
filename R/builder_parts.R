# A table's part (rtfreporter's plan_append(), tflspec's content item with
# its own data and items): its card in the table builder.  The part's
# name, what to do on a page group with no rows of it, where its rows
# come from (the report's ARD, or a dataset) and its own items, as YAML
# (the items tflspec reads and writes: table_plan, plan_cells ...).  A
# quick start writes the usual part: the ARD's summary statistics under
# the table, by the table's own columns.

# The part's items as YAML lines (as the report's file says them)
.part_items_yaml <- function(content) {
  if (!length(content)) return("")
  o <- tflspec::tfl_output("PART", "table", content = content)
  f <- tempfile("tflplanner-part", fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tflspec::tfl_write_output(o, f)
  y <- readLines(f, encoding = "UTF-8", warn = FALSE)
  at <- match("content:", y)
  if (is.na(at)) return("")
  body <- y[-seq_len(at)]
  # (the next top-level key ends it)
  end <- which(nzchar(body) & !startsWith(body, " ") & !startsWith(body, "-"))
  if (length(end)) body <- body[seq_len(end[1L] - 1L)]
  paste(sub("^  ", "", body), collapse = "\n")
}

# The part's items read from YAML lines (NULL and its message when they do
# not read)
.part_items_parse <- function(text) {
  text <- gsub("\r", "", as.character(text %||% ""))
  if (!nzchar(trimws(text))) return(list())
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  y <- c("id: PART", "kind: table", "content:", paste0("  ", lines))
  f <- tempfile("tflplanner-part", fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(enc2utf8(y), f, useBytes = TRUE)
  o <- tflspec::tfl_read_output(f)
  o$content %||% list()
}

# The usual part: the report's ARD summarised under the table, by its
# columns (normalize_ard(), table_plan() with a block heading, the
# continuous statistics)
.part_quickstart <- function(cols = NULL) {
  tp <- c(if (length(cols)) list(cols = cols),
          list(rows = list(block = tflspec::tfl_r("~ \"Summary Statistics\""))))
  list(tflspec::tfl_item("normalize_ard", list()),
       tflspec::tfl_item("table_plan", tp),
       tflspec::tfl_item("plan_cells", list(continuous = list(
         n = "{N:.0f}", Mean = "{mean:.3s}", SD = "{sd:.3s}", Median = "{median:.3s}",
         "Min, Max" = "{min:.3s}, {max:.3s}"))))
}

# The card's body: `cid` its input ids' stem, `it` the item, `datasets` the
# study's datasets, `mode` how the part can be edited (.part_modes(): its
# name and "cards", "dataset" or "defaults"; NULL a part not written yet)
.part_body <- function(cid, it, datasets = character(), lang = "en", mode = NULL) {
  t <- function(x) tr(x, lang)
  a <- it$args %||% list()
  d <- a$data %||% list(source = "ard")
  src <- if (identical(d[["source"]], "dataset")) "dataset" else "ard"
  btn <- function(what, label, class = "btn-sm btn-outline-primary") {
    shiny::tags$button(
      type = "button", class = paste("btn", class), `data-path` = it$path,
      onclick = sprintf("Shiny.setInputValue('b_part_act', {act: '%s', path: this.dataset.path, n: Date.now()}, {priority: 'event'})", what),
      label)
  }
  shiny::div(
    class = "rp-part",
    shiny::div(
      class = "d-flex flex-wrap gap-2",
      shiny::div(style = "flex: 1 1 10rem",
                 shiny::textInput(paste0(cid, "__name"), paste0(t("The part's name"), " (name)"),
                                  value = a$name %||% "", width = "100%", placeholder = "part2")),
      shiny::div(style = "flex: 1 1 10rem",
                 shiny::selectInput(paste0(cid, "__absent"),
                                    with_tip(paste0(t("A page group with no rows of it"), " (absent)"),
                                             t("error: stop; skip: the page without the part.")),
                                    c("error", "skip"), selected = a$absent %||% "error", width = "100%"))),
    shiny::div(
      class = "d-flex flex-wrap gap-2",
      shiny::div(style = "flex: 1 1 10rem",
                 shiny::radioButtons(paste0(cid, "__source"), paste0(t("Its rows come from"), " (data)"),
                                     stats::setNames(c("ard", "dataset"), c(t("The report's ARD"), t("A dataset"))),
                                     selected = src, inline = TRUE)),
      shiny::div(style = "flex: 1 1 10rem",
                 shiny::selectInput(paste0(cid, "__dataset"), t("Dataset"),
                                    c("", datasets), selected = d[["dataset"]] %||% "", width = "100%"))),
    shiny::textAreaInput(paste0(cid, "__content"),
                         with_tip(paste0(t("The part's items"), " (content)"),
                                  t("Its own table items, as the report's file writes them: table_plan, plan_cells, ... (normalize_ard first for the ARD).")),
                         value = .part_items_yaml(a$content), rows = 9, width = "100%", resize = "vertical"),
    shiny::div(
      class = "d-flex flex-wrap gap-2",
      btn("apply", paste0(t("Apply"), " (plan_append)"), "btn-sm btn-primary"),
      btn("start", t("Start: the ARD's summary statistics under the table")),
      if (identical(mode$mode, "cards")) shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-secondary", `data-name` = mode$name,
        `data-rp-scope-jump` = "1",
        onclick = "Shiny.setInputValue('b_scope', {name: this.dataset.name, n: Date.now()}, {priority: 'event'})",
        t("Open in cards"))),
    # (why the cards are not offered: one line, under the YAML and the buttons)
    if (!is.null(mode) && !identical(mode$mode, "cards")) shiny::div(
      class = "small text-muted mt-1",
      t(switch(mode$mode,
               dataset = "No cards for a dataset part (the cards read the ARD): edit its items in the YAML above.",
               "From the study defaults: no cards. Change the YAML here to make it this table's own, or edit spec/defaults.yml."))))
}

# The item's arguments from the card's fields (an error when its items do
# not read)
.part_args_read <- function(input, cid, it) {
  src <- input[[paste0(cid, "__source")]] %||% "ard"
  ds <- input[[paste0(cid, "__dataset")]] %||% ""
  if (identical(src, "dataset") && !nzchar(ds)) stop("Choose the part's dataset.", call. = FALSE)
  content <- .part_items_parse(input[[paste0(cid, "__content")]])
  nm <- trimws(input[[paste0(cid, "__name")]] %||% "")
  ab <- input[[paste0(cid, "__absent")]] %||% "error"
  c(if (nzchar(nm)) list(name = nm),
    if (!identical(ab, "error")) list(absent = ab),
    list(data = if (identical(src, "dataset")) list(source = "dataset", dataset = ds) else list(source = "ard"),
         content = content))
}

# ---- a part's items in the builder's own cards --------------------------------
# A part is edited with the table's own cards: the builder's id for it is
# the report's and the part's name ("T-PK-1::summary"), and the spec's
# reading and writing of a report's items (.spec_items(),
# .spec_set_items()) take such an id as the part's items inside the
# report's plan_append() item.  A part id is for the cards only: it is no
# report -- no rows in the planner's sheets (sheet_rows() gives the part's
# own, made from its items), no program, no file.

.part_sep <- "::"

# The cards a part has none of.  Its own part, the Total column (the
# report's ARD's overall rows) and the decimals' rule (the study's rounding)
# are the table's.  The rest is what rtfreporter says a part may not carry
# (?plan_append, "Parts"; plan_append() stops at it): the whole table's
# pages (plan_paginate_rows / _group / _cols), column header, column widths
# and alignment, and table style are the first plan's.  Kept though a part
# may use them only in part: plan_cell_style (only with `where`, not header
# or cols), plan_blanks (first / last, not where / counted) and plan_levels
# (its row keys, not the column key); plan_hide is allowed.
.part_skip <- c("plan_append", "plan_total", "plan_digits",
                "plan_paginate_rows", "plan_paginate_group", "plan_paginate_cols",
                "plan_col_header", "plan_columns", "plan_style")

# The mark of a part's items: "This part" for "This table" (the part's own
# are the part's), and only table_plan() required (tflspec stops a part with
# none; normalize_ard() and plan_cells() may be left out)
.part_items_mark <- function(items) {
  lapply(items, function(it) {
    it$own_word <- "This part"
    it$required <- identical(it$fn, "table_plan")
    it
  })
}

# the builder's id of a report's part
.part_id <- function(output_id, name) paste0(output_id, .part_sep, name)

# a part's id split: list(id, name); NULL for a report's own id
.part_of <- function(id) {
  if (length(id) != 1L || is.na(id) || !grepl(.part_sep, id, fixed = TRUE)) return(NULL)
  at <- regexpr(.part_sep, id, fixed = TRUE)
  list(id = substr(id, 1L, at - 1L), name = substr(id, at + nchar(.part_sep), nchar(id)))
}

# the report an id is of (a part's: its report's)
.part_parent <- function(id) (.part_of(id) %||% list(id = id))$id

# a part's name, as the program calls it (plan_append(name = )): `i` its
# place among the content's parts
.part_name_of <- function(it, i) it$args$name %||% paste0("part", i + 1L)

# the parts of a report's content: their names
.part_names <- function(content) {
  parts <- Filter(function(it) identical(it$fn, "plan_append"), content %||% list())
  vapply(seq_along(parts), function(i) .part_name_of(parts[[i]], i), "")
}

# where the part called `name` is in a content (its index; NA none)
.part_at <- function(content, name) {
  at <- which(vapply(content %||% list(), function(it) identical(it$fn, "plan_append"), NA))
  nm <- vapply(seq_along(at), function(i) .part_name_of(content[[at[i]]], i), "")
  at[match(name, nm)]
}

# How each part of a report's items (.spec_items()$items, each with where
# it comes from) can be edited, in order: list(name, mode) -- "cards" (the
# report's own, its rows the report's ARD's), "dataset" (its rows come from
# a dataset: the cards work on an ARD) or "defaults" (the study's defaults
# say it; a report with a part of its own has the defaults' parts replaced,
# so a report's parts are all of the one kind or all of the other)
.part_modes <- function(items) {
  parts <- Filter(function(it) identical(it$fn, "plan_append") && !isFALSE(it$present), items %||% list())
  lapply(seq_along(parts), function(i) {
    it <- parts[[i]]
    mode <- if (!identical(it$from %||% "own", "own")) "defaults" else
      if (identical(it$args$data[["source"]], "dataset")) "dataset" else "cards"
    list(name = .part_name_of(it, i), mode = mode)
  })
}

# The bar over the cards of a table with a part the cards can edit: the
# table's own cards, or a part's (`part` the one shown, "" the table's);
# NULL for a table with none
.part_scope_bar <- function(modes, part = "", lang = "en") {
  t <- function(z) tr(z, lang)
  nm <- vapply(Filter(function(m) identical(m$mode, "cards"), modes), function(m) m$name, "")
  if (!length(nm)) return(NULL)
  btn <- function(name, label) {
    on <- identical(name, part)
    shiny::tags$button(
      type = "button", class = paste("btn btn-sm", if (on) "btn-primary" else "btn-outline-primary"),
      `data-name` = name, `aria-pressed` = if (on) "true" else "false",
      onclick = "Shiny.setInputValue('b_scope', {name: this.dataset.name, n: Date.now()}, {priority: 'event'})",
      label)
  }
  shiny::div(
    class = "rp-part-scope d-flex flex-wrap gap-1 align-items-center mb-2",
    shiny::span(class = "small text-muted me-1", t("Editing:")),
    btn("", t("This table")),
    lapply(nm, function(n) btn(n, sprintf(t("Part: %s (plan_append)"), n))),
    if (nzchar(part)) shiny::span(
      class = "small text-muted ms-2",
      sprintf(t("These cards edit the part \"%s\" (its rows print under the table)."), part),
      " ",
      help_tip(t("The part's name, where its rows come from and its YAML are in its card under this table."))))
}

# Stop for an id that is a part: what writes a program or a file for a
# report is not for a part
.stop_part <- function(output_id) {
  if (!is.null(.part_of(output_id))) {
    stop("A part (", output_id, ") is no report: it has no program or file of its own.", call. = FALSE)
  }
  invisible(output_id)
}

# A part's own sheets, made from its items as a report's are (the bridge,
# over a report of nothing but the part's content): the table sheets' rows,
# `output_id` the part's id.  A part has no rows in the planner's sheets,
# so these are what its cards read.  NULL: no such part.  Made again only
# when its items change.
.part_virtual <- new.env()
.part_sheets <- function(x, part_id) {
  it <- .spec_part_items(x, part_id)
  if (is.null(it)) return(NULL)
  hit <- .part_virtual$last
  if (!is.null(hit) && identical(hit$key, it$own)) return(hit$sheets)
  o <- tflspec::tfl_output("PART", "table", content = it$own)
  back <- tryCatch(
    tflspec::tfl_sheets_from_spec(.spec_study_lean(x$spec$study), list(PART = o)),
    error = function(e) NULL)
  out <- list()
  for (s in intersect(table_sheets(), names(back$table))) {
    d <- .normalize_sheet(back$table[[s]], s)
    out[[s]] <- d[!is.na(d$output_id) & d$output_id == "PART", , drop = FALSE]
  }
  .part_virtual$last <- list(key = it$own, sheets = out)
  out
}

# ... one sheet's rows of it, as sheet_rows() gives a report's (no row: the
# planner's sheet with none)
.part_sheet_rows <- function(x, sheet, part_id) {
  d <- x$sheets[[sheet]]
  v <- .part_sheets(x, part_id)[[sheet]]
  if (is.null(v) || !nrow(v)) return(d[0L, , drop = FALSE])
  v$output_id <- rep(part_id, nrow(v))
  v[names(d)]
}

# ---- the preview, in part scope ------------------------------------------------
# The preview is the whole table, its pages one under the other; a part's
# rows are under the parent's, usually on a later page.  The table before
# it is cut into pages has each row's part (the `.part` column); the pages
# do not.  So the first row of each part is kept (.part_first_rows()), and
# the page it is on found in the pages (.part_start_page()).

# The first row of each part but the first plan's (part1): its cells as
# trimmed text, by the part's name.  NULL: not a table of parts.
.part_first_rows <- function(full) {
  if (!is.data.frame(full) || !".part" %in% names(full)) return(NULL)
  nm <- setdiff(unique(as.character(full$.part)), c("part1", NA))
  cells <- setdiff(names(full), ".part")
  out <- lapply(nm, function(n) {
    r <- full[match(n, full$.part), cells, drop = FALSE]
    v <- trimws(as.character(unlist(r, use.names = FALSE)))
    v[is.na(v)] <- ""
    v
  })
  stats::setNames(out, nm)
}

# The page a part's rows start on: the first page that has the part's first
# row, `pages` the preview's (attr "part_first": .part_first_rows()).  NA:
# not known.
.part_start_page <- function(pages, part) {
  first <- attr(pages, "part_first")[[part]]
  if (is.null(first)) return(NA_integer_)
  for (i in seq_along(pages)) {
    d <- pages[[i]]$data
    if (!is.data.frame(d) || ncol(d) != length(first)) next
    for (r in seq_len(nrow(d))) {
      v <- trimws(as.character(unlist(d[r, , drop = FALSE], use.names = FALSE)))
      v[is.na(v)] <- ""
      if (identical(v, first)) return(i)
    }
  }
  NA_integer_
}

# The parts that read a dataset but whose items are for an ARD: normalize_ard()
# reads cards (context, stat_name), not a dataset, so the program stops there
# too.  `content`: the report's items; the positions of those plan_append items.
.dataset_parts_with_ard_items <- function(content) {
  which(vapply(content %||% list(), function(it) {
    identical(it$fn, "plan_append") &&
      identical(it$args$data$source, "dataset") &&
      any(vapply(it$args$content %||% list(), function(z) identical(z$fn, "normalize_ard"), NA))
  }, NA))
}

# The items of a part that read a dataset which cannot run on it: they read
# a cards ARD (a statistic name, a context).  normalize_ard() and
# plan_stat_groups() always; plan_cells() when its templates are for the
# kinds of variable (continuous / categorical) with no `default` for the
# rest.  (Its `default`, a column's template and plan_filter() / plan_compare()
# run on a dataset.)  Returns list(content, dropped): the items that can run
# and the verbs taken out.
.part_dataset_items_split <- function(content) {
  ard_only <- function(it) {
    fn <- it$fn
    if (fn %in% c("normalize_ard", "plan_stat_groups")) return(TRUE)
    identical(fn, "plan_cells") && any(c("continuous", "categorical") %in% names(it$args)) &&
      !"default" %in% names(it$args)
  }
  gone <- vapply(content %||% list(), ard_only, NA)
  list(content = (content %||% list())[!gone],
       dropped = unique(vapply((content %||% list())[gone], function(it) it$fn, "")))
}

# What was taken out (`fn`: the verbs), in one line
.part_dataset_dropped_note <- function(name, fn, tr = function(x) x) {
  name <- if (is.na(name) || !nzchar(name)) "(no name)" else name
  sprintf(tr("The part %s now reads a dataset: %s read the ARD and cannot run on it, so they were taken out of its items."),
          name, paste(fn, collapse = ", "))
}

# What to say of such a part (`name`: the parts' names)
.dataset_part_ard_items_note <- function(name, tr = function(x) x) {
  name <- ifelse(is.na(name) | !nzchar(name), "(no name)", name)
  sprintf(tr("The part %s reads a dataset, but its items are for an ARD: normalize_ard reads cards, not a dataset. Take normalize_ard (and the items for cards) out of its items, or set its data back to the report's ARD."),
          paste(name, collapse = ", "))
}

# What to say over a preview that is shown without the parts that stopped it
# (`pages`: .preview_content_pages()'s, with attr "dataset_parts" and
# "part_errors"): the note of a dataset part with items for an ARD, and for
# any other part its name with its error
.dropped_parts_note <- function(pages, tr = function(x) x) {
  nm <- attr(pages, "dataset_parts")
  if (!length(nm)) return(NULL)
  why <- attr(pages, "part_errors")[nm]
  why <- if (is.null(why)) rep(NA_character_, length(nm)) else unname(why)
  other <- !is.na(why)
  # (the error names the part already: "plan_append("name"): ...")
  why <- sub("^plan_append\\(\"[^\"]*\"\\): ", "", why)
  paste(c(if (any(!other)) .dataset_part_ard_items_note(nm[!other], tr),
          sprintf(tr("The part %s stopped: %s"), ifelse(nzchar(nm[other]) & !is.na(nm[other]), nm[other], "(no name)"),
                  why[other]),
          tr("The table is shown without it.")), collapse = " ")
}

# The one line over the preview in part scope: the page the part's rows are
# on (`data-part-page`: the page scrolls to it when the preview is drawn)
.part_page_note <- function(pages, part, lang = "en") {
  t <- function(z) tr(z, lang)
  at <- .part_start_page(pages, part)
  shiny::div(
    # (stays in view as the pages scroll)
    class = "small text-muted mb-1 py-1 rp-pv-part", `data-part-page` = if (!is.na(at)) at,
    style = "position: sticky; top: 0; z-index: 2; background: var(--bs-body-bg, #fff)",
    if (is.na(at)) t("The part's rows print under the table: see the later pages.") else
      sprintf(t("The part is on page %s."), at))
}
