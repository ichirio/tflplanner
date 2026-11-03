# The AI drafts tab's card for the look of past reports: an agent reads
# the RTF files of a folder (their numbers only -- column widths, borders,
# fonts, alignment: tflspec::tfl_rtf_style_scan()) and says the usual
# forms in a file tflspec names (its result); the app shows the prompt to
# give it and, when the file is there, sets what it learned against the
# company standard a line at a time (tflspec::tfl_ai_style_diff()).  The
# lines a person ticks are adopted (tflspec::tfl_ai_style_adopt()) and
# written as the company's reference pack (tflspec::tfl_ai_reference_build(),
# <tflplanner home>/ai/reference), which every study's agent reads
# (tflspec::tfl_ai_prepare() copies it).  Not a study's: the survey's
# files are in <tflplanner home>/style.  The company standards themselves
# (the workbook) are not changed here.

# Which tflspec API the card talks to, found once from the prompt's
# arguments: "old" (tfl_ai_style_prompt() takes `out`; the survey's files
# are in <home>/style and the agent's result is a YAML file there) or "new"
# (the company's AI folder <home>/ai holds the files and the company hints
# file).  The old path goes when the new tflspec is on every machine.
.ai_style_api <- function() {
  if ("out" %in% names(formals(tflspec::tfl_ai_style_prompt))) "old" else "new"
}

# The company's AI folder (the home's): the app sets tflspec's option
# tflspec.ai_home to it while it runs (.app_options())
.ai_home <- function() file.path(tflplanner_home(), "ai")

# The folder of the survey's files (the home's, not a study's)
.ai_style_dir <- function(api = .ai_style_api()) {
  if (identical(api, "new")) .ai_home() else file.path(tflplanner_home(), "style")
}

# The folder the card offers for past reports under the new API
.ai_past_dir <- function() file.path(.ai_home(), "past_tfl")

# The company hints file: the agent's result under the new API
.ai_hints_file <- function() file.path(.ai_home(), "company_hints.yml")

# The company's reference pack for AI agents (the home's): the app sets
# tflspec's option tflspec.ai_reference to it while it runs (.app_options())
.ai_reference_home <- function() file.path(.ai_home(), "reference")

# The agent's result: the new API's company hints file; the old one's, the
# path tflspec gave with the prompt, else the folder's YAML file
.ai_style_result <- function(dir, files = NULL, api = .ai_style_api()) {
  if (identical(api, "new")) return(.ai_hints_file())
  r <- if (!is.null(files) && "result" %in% names(files)) files[["result"]]
  if (length(r) && !is.na(r) && nzchar(r)) return(r)
  y <- if (dir.exists(dir)) list.files(dir, "[.]ya?ml$", full.names = TRUE) else character()
  if (length(y)) y[1L] else NA_character_
}

# ---- the company standard's conventions -------------------------------------

# A cell's template in the agent's words: `{n:.0f} ({p:.1f%})` is
# `xx (xx.x%)`, `(N={n})` is `(N=xx)`; the study's tokens ({OUTPUT_LABEL})
# stay as they are
.std_placeholders <- function(x) {
  dec <- function(m) {
    d <- as.integer(sub("^\\{[A-Za-z]+:\\.(\\d+)f%?\\}$", "\\1", m))
    paste0("xx", ifelse(d > 0L, paste0(".", strrep("x", d)), ""), ifelse(grepl("%", m, fixed = TRUE), "%", ""))
  }
  m <- gregexpr("\\{[A-Za-z]+:\\.\\d+f%?\\}", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), dec)
  gsub("\\{[A-Za-z]\\}", "xx", x, perl = TRUE)
}

# a sheet's lines (left / center / right) as "<where>, <alignment>: a / b"
.std_lines <- function(d, where) {
  if (is.null(d) || !nrow(d)) return(character())
  out <- character()
  sides <- c(left = "left", center = "centred", right = "right")
  for (col in names(sides)) {
    side <- sides[[col]]
    v <- if (col %in% names(d)) d[[col]] else NULL
    v <- trimws(v[!is.na(v) & nzchar(trimws(v))])
    if (length(v)) out <- c(out, sprintf("%s, %s: %s", where, side, paste(v, collapse = " / ")))
  }
  out
}

# The company standard (company_standards()) as tflspec sets it against
# what an agent learned: a data frame of kind, key and value, one row a
# convention of tfl_ai_style_prompt()'s that the standards hold (kind
# `all`: the standards are not by kind).  A convention the standards do
# not hold is not here: tfl_ai_style_diff() shows a learned one as new.
.std_conventions <- function(s = company_standards()) {
  first <- function(v) {
    v <- as.character(v)
    v <- trimws(v[!is.na(v) & nzchar(trimws(v))])
    if (length(v)) v[1L] else NA_character_
  }
  set <- function(key) {
    st <- s$settings
    if (is.null(st)) NA_character_ else first(st$value[st$key == key])
  }
  page <- s$default_page
  col <- function(d, name) if (!is.null(d) && name %in% names(d)) d[[name]] else NULL
  out <- list()
  add <- function(key, value) {
    if (length(value) == 1L && !is.na(value) && nzchar(value)) out[[key]] <<- value
  }
  # font: the page's, else the settings'; its size in points
  font <- first(c(col(page, "font"), set("font")))
  half <- suppressWarnings(as.numeric(first(col(page, "font_size_half_points"))))
  size <- if (!is.na(half)) format(half / 2) else set("font_size")
  if (!is.na(font)) add("font", if (is.na(size)) font else sprintf("%s %s pt", font, size))
  add("orientation", first(col(page, "orientation")))
  # the stub's width to a value column's
  cw <- s$default_columns
  if (!is.null(cw) && nrow(cw)) {
    w <- suppressWarnings(as.numeric(cw$rel_width))
    stub <- w[cw$column %in% "row_label"][1L]
    val <- w[cw$column %in% ".values"][1L]
    if (!is.na(stub) && !is.na(val) && val > 0) add("stub_ratio", format(round(stub / val, 1)))
  }
  # titles: the titles' lines, and the page header's centred ones; the
  # footnotes: theirs and the page footer's
  hdr <- s$default_header
  if (!is.null(hdr)) hdr <- hdr[, intersect(c("line", "center"), names(hdr)), drop = FALSE]
  tt <- c(.std_lines(s$default_titles, "titles"), .std_lines(hdr, "page header"))
  if (length(tt)) add("titles", paste(tt, collapse = "; "))
  fn <- c(.std_lines(s$default_footnotes, "footnotes"), .std_lines(s$default_footer, "page footer"))
  if (length(fn)) add("footnotes", paste(fn, collapse = "; "))
  # the column header: the value columns' N, the stub's heading
  ch <- s$default_col_header
  if (!is.null(ch) && nrow(ch)) {
    txt <- ch$text
    n_txt <- txt[ch$cols %in% ".values" & !is.na(txt) & grepl("{n", txt, fixed = TRUE)]
    if (length(n_txt)) {
      ln <- strsplit(n_txt[1L], "\n", fixed = TRUE)[[1L]]
      add("n_form", .std_placeholders(trimws(ln[grepl("{n", ln, fixed = TRUE)][1L])))
    }
    stub <- txt[ch$cols %in% "row_label" & !is.na(txt) & nzchar(trimws(txt))]
    if (length(stub)) add("stub_heading", stub[length(stub)])
  }
  # a count with its percent: the cell every categorical row takes
  cells <- s$default_cells
  if (!is.null(cells) && nrow(cells)) {
    all_vars <- if ("variable" %in% names(cells)) is.na(cells$variable) | !nzchar(cells$variable) else TRUE
    add("categorical", .std_placeholders(first(cells$template[all_vars])))
  }
  # a continuous variable's statistics: the first preset's rows
  pre <- s$cell_presets
  if (!is.null(pre) && nrow(pre)) {
    pre <- pre[pre$variable %in% "continuous", , drop = FALSE]
    if (nrow(pre)) add("continuous", paste(pre$row[pre$preset == pre$preset[1L]], collapse = " | "))
  }
  # the decimals: each statistic's, the same for every variable
  dg <- s$default_digits
  if (!is.null(dg) && nrow(dg)) {
    dg <- dg[is.na(dg$variable) | !nzchar(dg$variable), , drop = FALSE]
    dg <- dg[!is.na(dg$statistic) & !is.na(dg$digits), , drop = FALSE]
    if (nrow(dg)) add("digits", paste(sprintf("%s %s", dg$statistic, dg$digits), collapse = ", "))
  }
  if (!length(out)) {
    return(data.frame(kind = character(), key = character(), value = character(),
                      stringsAsFactors = FALSE))
  }
  data.frame(kind = "all", key = names(out), value = unname(unlist(out)), stringsAsFactors = FALSE)
}

# The prompt through the API tflspec has: the old one's files go to
# <home>/style; the new one's to <home>/ai, with the card's company
# standard so the agent leaves out what the standard already says
.ai_style_prompt <- function(rtf, api = .ai_style_api()) {
  if (identical(api, "new")) {
    tflspec::tfl_ai_style_prompt(rtf_dir = rtf, dir = .ai_home(), standard = .std_conventions())
  } else {
    tflspec::tfl_ai_style_prompt(rtf, dir = .ai_style_dir("old"))
  }
}

# The result through the API tflspec has: the new one's file is always the
# company hints file in the AI folder
.ai_style_read <- function(file, api = .ai_style_api()) {
  if (identical(api, "new")) tflspec::tfl_ai_style_read(.ai_hints_file()) else tflspec::tfl_ai_style_read(file)
}

# The agent's notes (a named character vector, by kind) and the points where
# the past reports differ from the standard (a data frame): two short lists
.style_notes_ui <- function(s, t) {
  n <- attr(s, "notes")
  n <- n[!is.na(n) & nzchar(n)]
  cf <- attr(s, "conflicts")
  items <- function(x) shiny::tags$ul(class = "small mb-1", lapply(x, shiny::tags$li))
  shiny::tagList(
    if (length(n)) shiny::div(
      shiny::div(class = "small fw-semibold", t("Notes from the agent")),
      items(ifelse(nzchar(names(n)), paste0(names(n), ": ", n), n))),
    if (is.data.frame(cf) && nrow(cf)) shiny::div(
      shiny::div(class = "small fw-semibold", t("Where the past reports differ from the company standard")),
      items(sprintf(t("%s / %s: the standard says %s, the files show %s (%s of %s)"),
                    cf$kind, cf$key, cf$standard, cf$files,
                    ifelse(is.na(cf$seen), "?", cf$seen), ifelse(is.na(cf$of), "?", cf$of)))))
}

# ---- the card ---------------------------------------------------------------

# the lines ticked when the diff is shown: the proposed ones that differ
# from the standard or are new to it
.style_default_ticks <- function(diff) {
  diff$adopt %in% "proposed" & diff$status %in% c("differs", "new")
}

# The card's content: on its own, so it can move to the tool's settings
# later (the AI tab shows it as a sub-tab for now: ai_style_panel())
#' @noRd
ai_style_ui <- function(ns, t) {
  shiny::tagList(
    shiny::p(class = "small text-muted", t("Not a study's: for every study. The agent reads the numbers of the RTF files (column widths, borders, fonts, alignment), never their text, and writes the usual forms. Set against the company standard here, the lines you adopt become the company's reference pack, which every study's agent reads.")),
    .ai_path_input(ns("style_rtf"), t("Folder of past RTF reports"),
                   if (identical(.ai_style_api(), "new")) .ai_past_dir() else "C:/.../tfl",
                   ns("style_browse"), t("Browse...")),
    shiny::div(class = "d-flex flex-wrap gap-2 align-items-center my-2",
               .btn(ns("style_prompt"), t("Write the prompt"), class = "btn-sm btn-primary"),
               .btn(ns("style_read"), t("Read the result")),
               .btn(ns("style_open"), t("Open the folder"))),
    shiny::uiOutput(ns("style_out")),
    shiny::uiOutput(ns("style_diff")))
}

#' @noRd
ai_style_panel <- function(ns, t) {
  bslib::nav_panel(t("Past reports' look"), value = "style",
                   ai_style_ui(ns, t))
}

# The diff's table: a row a convention, a tick box for the learned ones
# (`gen` keeps a new reading's boxes apart from the last one's)
.style_diff_table <- function(diff, ns, gen, ticks, t) {
  status <- c(same = t("Same as the standard"), differs = t("Differs from the standard"),
              new = t("Not in the standard"), `not learned` = t("Not learned"))
  tint <- c(same = "", differs = "background:#fff3cd;", new = "background:#cfe2ff;",
            `not learned` = "")
  tint[] <- paste0(tint, "white-space:nowrap")
  cell <- function(v) if (is.na(v)) shiny::span(class = "text-muted", "-") else v
  rows <- lapply(seq_len(nrow(diff)), function(i) {
    d <- diff[i, ]
    box <- if (is.na(d$learned)) shiny::span(class = "text-muted", "-") else
      shiny::checkboxInput(ns(sprintf("style_adopt_%d_%d", gen, i)), NULL, value = ticks[i],
                           width = "auto")
    seen <- if (is.na(d$seen) && is.na(d$of)) NA_character_ else
      sprintf("%s / %s", ifelse(is.na(d$seen), "?", d$seen), ifelse(is.na(d$of), "?", d$of))
    shiny::tags$tr(
      shiny::tags$td(box),
      shiny::tags$td(shiny::code(d$kind)),
      shiny::tags$td(shiny::code(d$key)),
      shiny::tags$td(cell(d$learned)),
      shiny::tags$td(cell(seen),
                     if (identical(d$adopt, "by hand")) shiny::span(class = "badge text-bg-secondary ms-1", t("by hand"))),
      shiny::tags$td(cell(d$standard)),
      shiny::tags$td(style = tint[[d$status]], status[[d$status]] %||% d$status))
  })
  shiny::tags$table(
    class = "table table-sm small align-middle style-diff",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(t("Adopt")), shiny::tags$th(t("Kind")), shiny::tags$th(t("Key")),
      shiny::tags$th(t("Learned value")), shiny::tags$th(t("Seen in (files)")),
      shiny::tags$th(t("Company standard's value")), shiny::tags$th(t("Status")))),
    shiny::tags$tbody(rows))
}

# its server part: the same module's input, output and session, wherever
# the card is drawn
#' @noRd
ai_style_server <- function(input, output, session, notify, guarded, t) {
  ns <- session$ns
  state <- shiny::reactiveValues(prompt = NULL, files = NULL, summary = NULL, diff = NULL,
                                 standard = NULL, gen = 0L, built = NULL)
  shiny::observeEvent(input$style_browse, {
    d <- tryCatch(.ai_choose_dir(input$style_rtf %||% ""), error = function(e) {
      notify(conditionMessage(e), "warning")
      NULL
    })
    session$sendCustomMessage("ai-browse-done", TRUE)
    if (length(d) && !is.na(d) && nzchar(d)) shiny::updateTextInput(session, "style_rtf", value = d)
  })
  shiny::observeEvent(input$style_prompt, {
    rtf <- trimws(gsub("^\"|\"$", "", input$style_rtf %||% ""))
    # the new API: a blank field is the AI folder's past_tfl
    if (!nzchar(rtf) && identical(.ai_style_api(), "new")) rtf <- .ai_past_dir()
    if (!nzchar(rtf) || !dir.exists(rtf)) return(notify(t("Choose the folder of past RTF reports."), "warning"))
    out <- guarded(.ai_style_prompt(rtf))
    if (is.null(out)) return()
    state$prompt <- paste(as.character(out), collapse = "\n")
    state$files <- unlist(attr(out, "files"))
    state$summary <- NULL
    state$diff <- NULL
    state$built <- NULL
  })
  shiny::observeEvent(input$style_read, {
    f <- .ai_style_result(.ai_style_dir(), state$files)
    if (is.na(f) || !file.exists(f)) {
      return(notify(sprintf(t("No result yet: the agent writes it in %s."), .ai_style_dir()), "warning"))
    }
    s <- guarded(.ai_style_read(f))
    if (is.null(s)) return()
    std <- guarded(.std_conventions())
    if (is.null(std)) return()
    d <- guarded(tflspec::tfl_ai_style_diff(s, std))
    if (is.null(d)) return()
    state$summary <- s
    state$standard <- std
    state$diff <- d
    state$gen <- state$gen + 1L
    state$built <- NULL
  })
  shiny::observeEvent(input$style_open, .open_folder(.ai_style_dir()))
  # the rows ticked now: a box's value, else its first one
  ticked <- shiny::reactive({
    d <- state$diff
    if (is.null(d) || !nrow(d)) return(logical())
    first <- .style_default_ticks(d)
    vapply(seq_len(nrow(d)), function(i) {
      if (is.na(d$learned[i])) return(FALSE)
      v <- input[[sprintf("style_adopt_%d_%d", state$gen, i)]]
      if (is.null(v)) first[i] else isTRUE(v)
    }, NA)
  })
  shiny::observeEvent(input$style_build, {
    d <- state$diff
    if (is.null(d)) return()
    rows <- which(ticked())
    conv <- guarded(tflspec::tfl_ai_style_adopt(d, rows, state$standard))
    if (is.null(conv)) return()
    files <- guarded(tflspec::tfl_ai_reference_build(conv, dir = .ai_reference_home()))
    if (is.null(files)) return()
    state$built <- list(files = files, n = length(rows))
    notify(sprintf(t("The pack is built: %d line(s) adopted."), length(rows)))
  })
  shiny::observeEvent(input$style_pack_open, .open_folder(.ai_reference_home()))
  output$style_out <- shiny::renderUI({
    f <- state$files
    prompt <- if (!is.null(state$prompt)) {
      shiny::tagList(
        if (length(f)) shiny::div(class = "small", t("Written:"), " ", shiny::code(paste(f, collapse = ", "))),
        shiny::div(class = "small mt-1",
                   t("Give the agent this prompt (it reads the numbers and writes the company hints file):")),
        shiny::tags$textarea(class = "form-control small", rows = 8, readonly = NA, state$prompt))
    }
    if (is.null(prompt) && is.null(state$diff)) {
      return(shiny::p(class = "small text-muted", sprintf(t("The files go to %s."), .ai_style_dir())))
    }
    prompt
  })
  output$style_diff <- shiny::renderUI({
    d <- state$diff
    if (is.null(d)) return(NULL)
    s <- state$summary
    head <- sprintf(t("Learned from %s file(s), %s table(s)."), attr(s, "files") %||% "?",
                    attr(s, "tables") %||% "?")
    gen <- state$gen
    built <- state$built
    shiny::tagList(
      shiny::h6(class = "mt-3", t("Set against the company standard")),
      shiny::div(class = "small text-muted", head),
      .style_notes_ui(s, t),
      shiny::p(class = "small text-muted mb-1",
               t("Ticked: the lines most files show that differ from the company standard or are not in it. A line marked by hand is yours to decide. The company standards themselves are not changed.")),
      if (!nrow(d)) shiny::p(class = "small", t("Nothing learned yet.")) else
        .style_diff_table(d, ns, gen, shiny::isolate(.style_default_ticks(d)), t),
      shiny::div(class = "d-flex flex-wrap gap-2 align-items-center my-2",
                 .btn(ns("style_build"), t("Adopt the ticked lines and build the pack"),
                      class = "btn-sm btn-primary")),
      if (!is.null(built)) shiny::div(
        class = "small alert alert-success py-2",
        sprintf(t("%d line(s) adopted. Every study's agent reads the pack from its next preparation (tfl_ai_prepare):"),
                built$n), " ",
        shiny::code(built$files[["conventions"]]), " ",
        shiny::actionLink(ns("style_pack_open"), t("Open the folder"))))
  })
}
