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
# study's datasets
.part_body <- function(cid, it, datasets = character(), lang = "en") {
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
      btn("start", t("Start: the ARD's summary statistics under the table"))))
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
