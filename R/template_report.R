# Template reports (#440, _archive/template-spec-input-design-2026-10-20.md):
# a report whose YAML file says `template: {name, args}` -- one of
# tflspec's templates (tflspec::tfl_templates()) and the values of its
# parameters (tflspec::tfl_template_params()).  The view (the sheets) has
# the template's items, as tflspec::tfl_sheets_from_spec() expands them, so
# the ARD, the preview and the program are made as for any report; the
# report's file keeps the template and its arguments only.

# one line on each template (tflspec's templates; the app translates it)
.tpl_desc <- c(
  ae_overview = "Overview of adverse events",
  ae_soc_pt = "Adverse events by SOC and PT",
  demog = "Demographics",
  stf_t02_demog = "Demographics, after FDA STF IG v2 Table 2",
  disposition = "Disposition",
  hierarchical = "A hierarchy's counts",
  km_simple = "Kaplan-Meier curves",
  km_table = "Kaplan-Meier estimates",
  lab_shift = "Shift table of a lab parameter",
  listing = "A listing",
  ae_max_grade = "Adverse events by maximum grade",
  ae_pt_freq = "Adverse events by PT (frequent ones)",
  orr = "Objective response",
  shift = "Shift table",
  summary = "Continuous and categorical variables by a column key",
  vs_change = "A vital sign's change")

# The templates of a kind of report: name and kind
.tpl_list <- function(kind) {
  x <- tryCatch(tflspec::tfl_templates(), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(data.frame(name = character(), kind = character()))
  x <- x[x$kind %in% kind, c("name", "kind"), drop = FALSE]
  rownames(x) <- NULL
  x
}

# A template's parameters, named, in its order (none: a template unknown)
.tpl_params <- function(name) {
  p <- tryCatch(tflspec::tfl_template_params(name), error = function(e) list())
  if (length(p) && is.null(names(p))) {
    names(p) <- vapply(p, function(x) x$name, "")
  }
  p
}

# A report's template, as its file says it (NULL: none; a figure
# designer's `template: km_simple`, a name, is no template SPEC)
.tpl_of <- function(p, output_id) {
  o <- p$spec$outputs[[output_id]]
  if (is.null(o) || !is.list(o$template) || is.null(o$template$name)) return(NULL)
  list(name = as.character(o$template$name), args = o$template$args %||% list())
}

# The study's datasets: their columns, by dataset (upper case), read once
# a study folder
.tpl_cols_cache <- new.env()
.tpl_data_cols <- function(root) {
  if (is.null(root)) return(list())
  key <- normalizePath(root, "/", mustWork = FALSE)
  if (!is.null(.tpl_cols_cache[[key]])) return(.tpl_cols_cache[[key]])
  files <- list.files(file.path(root, "data"), "\\.(rds|RDS)$", recursive = TRUE, full.names = TRUE)
  out <- list()
  for (f in files) {
    d <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.data.frame(d)) next
    out[[toupper(sub("\\.[^.]*$", "", basename(f)))]] <- d
  }
  .tpl_cols_cache[[key]] <- out
  out
}

# The columns a parameter chooses from: the dataset's the report reads
# (its `data` argument or default), else every dataset's
.tpl_columns <- function(data, dataset = NULL) {
  ds <- toupper(as.character(dataset %||% "")[1L])
  if (!is.na(ds) && nzchar(ds) && !is.null(data[[ds]])) return(names(data[[ds]]))
  unique(unlist(lapply(data, names)))
}

.tpl_is_varlike <- function(x) {
  is.character(x) && length(x) > 0L && is.null(names(x)) &&
    all(grepl("^[A-Z][A-Z0-9_]*$", x))
}

# The input a parameter gets: population, data, var (one column), vars
# (columns, in order), levels (a variable's values), flag, number, text,
# chips (several texts), choice (one of tfl_param(choices = )), choices
# (several of them), list (a small table of lines, "a | b").  By the type
# tflspec declares (tfl_param(type = )); guessed from the default and the
# doc only when it declares none.
.tpl_type <- function(prm, columns = character()) {
  n <- prm$name
  if (identical(n, "population")) return("population")
  if (identical(n, "data")) return("data")
  several <- isTRUE(prm$multiple)
  ty <- prm$type
  if (length(ty) == 1L && nzchar(ty)) {
    return(switch(ty,
      vars = if (several) "vars" else "var",
      levels = "levels",
      flag = "flag",
      number = "number",
      text = if (several) "chips" else "text",
      choice = if (several) "choices" else "choice",
      list = "list",
      "text"))
  }
  d <- prm$default
  doc <- prm$doc %||% ""
  if (is.logical(d) && length(d) == 1L) return("flag")
  if (is.numeric(d) && length(d) == 1L && is.null(names(d))) return("number")
  several <- n %in% c("continuous", "categorical", "hierarchy", "variables", "params")
  if (.tpl_is_varlike(d) && (!length(columns) || all(d %in% columns))) {
    return(if (length(d) > 1L || several) "vars" else "var")
  }
  if (is.null(d)) {
    if (grepl("variable|flag", doc, ignore.case = TRUE)) return("var")
    return("text")
  }
  if (is.character(d) && is.null(names(d))) {
    return(if (length(d) == 1L) "text" else "chips")
  }
  "list"
}

# The variable whose values a levels parameter chooses from: its `of` -- a
# parameter's name (that parameter's value, or its default), else a
# variable's name itself
.tpl_levels_of <- function(prm, params, args) {
  of <- as.character(prm$of %||% "")[1L]
  if (is.na(of) || !nzchar(of)) return(NA_character_)
  if (of %in% names(params)) {
    v <- args[[of]] %||% params[[of]]$default
    v <- as.character(v %||% character())
    return(if (length(v)) v[1L] else NA_character_)
  }
  of
}

# The values of a variable in the study's data: the dataset's the report
# reads first, else the first dataset that has it
.tpl_levels <- function(data, variable, dataset = NULL) {
  if (is.na(variable) || !nzchar(variable)) return(character())
  ds <- toupper(as.character(dataset %||% "")[1L])
  sets <- c(if (!is.na(ds) && nzchar(ds)) ds, names(data))
  for (s in unique(sets)) {
    d <- data[[s]]
    if (is.data.frame(d) && variable %in% names(d)) {
      x <- d[[variable]]
      lv <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
      return(lv[nzchar(lv)])
    }
  }
  character()
}

# A list parameter as lines of a small table, "a | b" a line: a named
# vector or list of one value each (fields name, text -- "AGE | Age
# (years)"), or records (tfl_param(multiple = TRUE, fields = )) a line
# each, a field's several values comma separated.  NULL: a value the lines
# cannot hold (edited in R).
.tpl_list_fields <- function(prm) {
  f <- as.character(prm$fields %||% character())
  if (length(f)) f else c("name", "text")
}
.tpl_list_records <- function(prm) isTRUE(prm$multiple) && length(prm$fields) > 0L

.tpl_list_text <- function(x, prm) {
  if (is.null(x) || !length(x)) return("")
  cell <- function(v) {
    if (is.null(v)) return("")
    if (!is.atomic(v) || !is.null(names(v))) return(NULL)
    v <- as.character(v)
    if (any(grepl("[|\n]", v))) return(NULL)
    paste(v, collapse = ", ")
  }
  if (.tpl_list_records(prm)) {
    if (!is.list(x)) return(NULL)
    f <- .tpl_list_fields(prm)
    lines <- character()
    for (r in x) {
      if (!is.list(r) || !all(names(r) %in% f)) return(NULL)
      cs <- lapply(f, function(k) cell(r[[k]]))
      if (any(vapply(cs, is.null, NA))) return(NULL)
      lines <- c(lines, sub("( \\| )+$", "", paste(unlist(cs), collapse = " | ")))
    }
    return(paste(lines, collapse = "\n"))
  }
  if (is.null(names(x)) || any(!nzchar(names(x)))) return(NULL)
  vs <- lapply(x, cell)
  if (any(vapply(vs, is.null, NA)) || any(grepl("[|\n]", names(x)))) return(NULL)
  paste(paste(names(x), unlist(vs), sep = " | "), collapse = "\n")
}

# ... and the lines read back (NULL: none).  A cell of a number reads as a
# number, TRUE / FALSE as a flag; a records' field of several values
# (comma separated) as a vector.
.tpl_list_parse <- function(text, prm) {
  lines <- strsplit(paste(as.character(text %||% ""), collapse = "\n"), "\r?\n")[[1L]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(NULL)
  cells <- lapply(lines, function(l) trimws(strsplit(l, "|", fixed = TRUE)[[1L]]))
  val <- function(v, several = FALSE) {
    if (!nzchar(v)) return(NULL)
    if (v %in% c("TRUE", "FALSE")) return(v == "TRUE")
    if (grepl("^-?[0-9]+(\\.[0-9]+)?$", v)) return(as.numeric(v))
    if (several && grepl(",", v, fixed = TRUE)) return(trimws(strsplit(v, ",", fixed = TRUE)[[1L]]))
    v
  }
  if (.tpl_list_records(prm)) {
    f <- .tpl_list_fields(prm)
    return(lapply(cells, function(cs) {
      r <- list()
      for (i in seq_along(f)) {
        if (i > length(cs)) break
        v <- val(cs[[i]], several = TRUE)
        if (!is.null(v)) r[[f[[i]]]] <- v
      }
      r
    }))
  }
  keys <- vapply(cells, function(cs) cs[[1L]], "")
  vals <- vapply(cells, function(cs) if (length(cs) > 1L) paste(cs[-1L], collapse = " | ") else "", "")
  keep <- nzchar(keys)
  if (!any(keep)) return(NULL)
  stats::setNames(vals[keep], keys[keep])
}

# A value as one line to read (a default; a list's value)
.tpl_show <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.character(x) && length(x) == 1L && is.null(names(x))) return(x)
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
}

# What an input gives, as the argument's value (NULL: none -- the default)
.tpl_value <- function(v, type, prm = NULL) {
  if (is.null(v)) return(NULL)
  switch(type,
    flag = isTRUE(v),
    number = {
      v <- suppressWarnings(as.numeric(v))
      if (!length(v) || is.na(v)) NULL else v
    },
    vars = , chips = , choices = , levels = {
      v <- as.character(v)
      v <- v[nzchar(trimws(v))]
      if (!length(v)) NULL else if (identical(type, "levels") && !isTRUE(prm$multiple)) v[1L] else v
    },
    list = .tpl_list_parse(v, prm),
    {
      v <- trimws(paste(as.character(v), collapse = ""))
      if (nzchar(v)) v else NULL
    })
}

# The args with one argument set (NULL, or the default itself: taken away)
.tpl_set_arg <- function(args, prm, value) {
  args <- args %||% list()
  if (is.null(value) || (!isTRUE(prm$required) && identical(value, prm$default)) ||
      (!isTRUE(prm$required) && !is.null(prm$default) &&
       identical(unname(as.vector(value)), unname(as.vector(prm$default))))) {
    args[[prm$name]] <- NULL
  } else {
    args[[prm$name]] <- value
  }
  # (in the template's order)
  args
}

# The required arguments still empty
.tpl_missing <- function(name, args, study = NULL) {
  prm <- .tpl_params(name)
  req <- names(prm)[vapply(prm, function(x) isTRUE(x$required), NA)]
  # (a required argument the study gives -- its treatment for `by` -- is
  # not empty: tflspec fills it)
  given <- function(a) {
    v <- args[[a]] %||% if (!is.null(study)) .tpl_study_value(study, prm[[a]])
    length(v) > 0L && any(nzchar(as.character(v)))
  }
  req[!vapply(req, given, NA)]
}

# A template's parameters the card shows: not those it takes from the
# study standard (`hidden`; a KM type's data stage, its look)
.tpl_shown <- function(prm) Filter(function(x) !isTRUE(x$hidden), prm)

# Whether the report says the template's data in its own `data:` steps
# (the template hides its data stage: km_survival, km_cumulative)
.tpl_own_data <- function(prm) {
  any(vapply(prm, function(x) isTRUE(x$hidden) && identical(x$stage, "data"), NA))
}

# A report's `data:` steps as the card's fields say them: the dataset
# read, the analysis set's join (a semi join, as the report's population
# says it), the parameter, a filter.  Steps of another shape (a derive, a
# second read ...): `fields = FALSE`, kept as they are.
.tpl_steps_fields <- function(steps) {
  out <- list(fields = TRUE, dataset = "", join = FALSE, join_dataset = "", join_where = "",
              param = "", filter = "")
  steps <- steps %||% list()
  if (!length(steps)) return(out)
  kind <- vapply(steps, function(s) if (is.list(s)) as.character(s$step %||% "")[1L] else "", "")
  ok <- kind[1L] == "read" && all(kind[-1L] %in% c("join", "param", "filter")) &&
    !anyDuplicated(kind) && identical(kind, kind[order(match(kind, c("read", "join", "param", "filter")))])
  j <- steps[kind == "join"]
  if (length(j) && !identical(j[[1L]]$how, "semi")) ok <- FALSE
  if (!ok) return(utils::modifyList(out, list(fields = FALSE)))
  one <- function(k, f) if (any(kind == k)) as.character(steps[[which(kind == k)]][[f]] %||% "")[1L] else ""
  out$dataset <- one("read", "dataset")
  out$join <- any(kind == "join")
  out$join_dataset <- one("join", "dataset")
  out$join_where <- one("join", "where")
  out$param <- one("param", "value")
  out$filter <- one("filter", "expr")
  out
}

# The `data:` steps the card's fields write: the dataset read; the
# analysis set's subjects kept (a semi join on its dataset; `pop`: the
# population's row, its dataset and where); the parameter; the filter
# (NULL: no dataset, no steps)
.tpl_steps_from <- function(dataset, pop = NULL, param = "", filter = "") {
  has <- function(x) length(x) && !is.na(x[1L]) && nzchar(trimws(x[1L]))
  if (!has(dataset)) return(NULL)
  steps <- list(list(step = "read", dataset = toupper(trimws(dataset))))
  # (the analysis set's dataset itself read: the same semi join keeps its
  # subjects -- its condition never dropped)
  if (!is.null(pop) && has(pop$dataset)) {
    steps[[length(steps) + 1L]] <- c(list(step = "join", dataset = toupper(pop$dataset)),
                                     if (has(pop$where)) list(where = trimws(pop$where)),
                                     list(by = "USUBJID", how = "semi"))
  }
  if (has(param)) steps[[length(steps) + 1L]] <- list(step = "param", value = trimws(param))
  if (has(filter)) steps[[length(steps) + 1L]] <- list(step = "filter", expr = trimws(filter))
  steps
}

# The analysis-set flags of a subject-level dataset no population of the
# study says (*FL, its values Y / N): offered beside the study's
# populations, as a join on that flag
.tpl_flags_undefined <- function(adsl, pops = NULL) {
  if (!is.data.frame(adsl)) return(character())
  fl <- grep("FL$", names(adsl), value = TRUE)
  fl <- fl[vapply(fl, function(v) {
    x <- unique(stats::na.omit(as.character(adsl[[v]])))
    length(x) > 0L && all(x %in% c("Y", "N", ""))
  }, NA)]
  said <- paste(as.character(pops$where %||% character()), collapse = " ")
  fl[!vapply(fl, function(v) grepl(paste0("\\b", v, "\\b"), said), NA)]
}

# Whether the report's join is no longer its analysis set's (the
# population changed, or its definition): Apply writes it again
.tpl_join_stale <- function(f, pop, dataset = f$dataset) {
  # (no population: a join is the report's own -- a flag the study does not
  # define -- and stays)
  if (is.null(pop)) return(FALSE)
  want <- .tpl_steps_from(dataset, pop)
  wj <- Filter(function(s) identical(s$step, "join"), want %||% list())
  if (!f$join) return(length(wj) > 0L)
  if (!length(wj)) return(TRUE)
  !identical(toupper(f$join_dataset), toupper(wj[[1L]]$dataset)) ||
    !identical(f$join_where, wj[[1L]]$where %||% "")
}

# A report's file with its template's arguments set
.tpl_output <- function(p, output_id, args, name = NULL) {
  o <- p$spec$outputs[[output_id]]
  if (!is.null(name)) o$template$name <- name
  prm <- .tpl_params(o$template$name)
  args <- args[c(intersect(names(prm), names(args)), setdiff(names(args), names(prm)))]
  o$template$args <- args
  if (!is.null(args$population)) o$population <- as.character(args$population)
  o
}

# The study standard's value of a parameter that says where it is
# (tfl_param(study = "<verb>.<argument>")): that argument of the verb's
# item in the study's defaults (NULL: none)
.tpl_study_value <- function(study, prm) {
  at <- as.character(prm$study %||% "")[1L]
  if (is.na(at) || !grepl(".", at, fixed = TRUE)) return(NULL)
  fn <- sub("\\..*$", "", at)
  arg <- sub("^[^.]*\\.", "", at)
  # (the study's settings: study.yml `settings:`, the treatment ...)
  if (identical(fn, "settings")) return(study$study$settings[[arg]])
  for (it in study$defaults$content %||% list()) {
    if (identical(it$fn, fn) && !is.null(it$args[[arg]])) return(it$args[[arg]])
  }
  NULL
}

# ---- the page: a template report's steps -----------------------------------------
# A template report is made in two steps -- "1 Template (ARD and
# content)" (the content step, the ARD step hidden) and "2 Page and
# output" -- a code SPEC report in three; never by its kind.  The server
# says which at once when the report changes (rp-tpl-mode, a custom
# message: sent before the flush that draws the report), and the page
# shows "Loading ..." over the steps until that flush is done
# (rp-report-ready): the report before it, or the code SPEC's builder, is
# never seen in its place.  `.rp-tpl-only` / `.rp-code-only` are shown by
# the mode.
.tpl_step_labels <- c(content = "1 Template (data and content)", page = "2 Page and output")

.tpl_css <- "
body:not(.rp-tpl) .rp-tpl-only, body.rp-tpl .rp-code-only { display: none !important; }
.rp-report-loading { display: none; }
body.rp-adding::before { content: attr(data-loading); position: fixed; inset: 0; z-index: 1040;
  background: var(--bs-body-bg, #fff); color: #6c757d; padding: 6rem 2rem; font-size: .95rem; }

body.rp-switching .rp-report-loading { display: block; }
body.rp-switching #step ~ .tab-content, body.rp-switching #report_head, #report_head.rp-stale {
  visibility: hidden; }
.rp-tpl-default .form-control, .rp-tpl-default .selectize-input,
.rp-tpl-default .form-select, .rp-tpl-default .selectize-input .item {
  color: #8a8f98 !important; background-color: #f4f5f7; }
.rp-tpl-default .form-label { color: #6c757d; }
.rp-tpl-arg-from { font-size: .75rem; color: #6b7280; }
.rp-code pre.rp-flow { max-height: none !important; overflow-y: visible; min-height: 0; }
@keyframes rp-card-flash { 0%, 60% { outline-color: rgba(13,110,253,.85); }
  100% { outline-color: rgba(13,110,253,0); } }
.rp-card.rp-card-flash { outline: 3px solid rgba(13,110,253,0); outline-offset: 2px;
  animation: rp-card-flash 1.8s ease-out 1; }
.rp-add-menu .dropdown-menu { max-height: 60vh; overflow-y: auto; }
/* room below the last card: a card added at the end can come to the top */
.rp-builder-cards { padding-bottom: 45vh; }
/* the right pane (SPEC | Code | Result, ARD): it does not scroll itself --
   fitted to the window, sticky -- and in it only the tab shown scrolls (a
   code block, a table inside it grow to their height: one scroll bar) */
@media (min-width: 992px) {
  .rp-b-right { display: flex; flex-direction: column; overflow: hidden !important;
    height: calc(100vh - var(--rp-top, 0px) - 1rem); }
  .rp-split:not(.rp-lay-stack):not(.rp-lay-one) > .rp-tabs {
    height: calc(100vh - var(--rp-top, 0px) - 1rem); }
  .rp-b-right > .rp-tabs { flex: 1 1 auto; }
  .rp-b-right > .rp-tabs, .rp-split:not(.rp-lay-stack):not(.rp-lay-one) > .rp-tabs {
    display: flex; flex-direction: column; min-height: 0; overflow: hidden !important; }
  .rp-b-right > .rp-tabs > .tabbable,
  .rp-split:not(.rp-lay-stack):not(.rp-lay-one) > .rp-tabs > .tabbable {
    display: flex; flex-direction: column; flex: 1 1 auto; min-height: 0; }
  .rp-b-right > .rp-tabs > .tabbable > .tab-content,
  .rp-split:not(.rp-lay-stack):not(.rp-lay-one) > .rp-tabs > .tabbable > .tab-content {
    flex: 1 1 auto; min-height: 0; overflow: hidden; }
  .rp-b-right > .rp-tabs > .tabbable > .tab-content > .tab-pane.active,
  .rp-split:not(.rp-lay-stack):not(.rp-lay-one) > .rp-tabs > .tabbable > .tab-content > .tab-pane.active {
    height: 100%; overflow-y: auto; }
  .rp-b-right .tab-pane pre, .rp-split > .rp-tabs .tab-pane pre { max-height: none; }
  .rp-b-right .tab-pane .dataTables_scrollBody,
  .rp-split > .rp-tabs .tab-pane .dataTables_scrollBody { max-height: none !important; }
}
/* narrow, or the panes one above the other: the page alone scrolls */
@media (max-width: 991.98px) {
  .rp-tabs .tab-pane pre { max-height: none; }
}
.rp-split.rp-lay-stack > .rp-tabs .tab-pane pre, .rp-split.rp-lay-one > .rp-tabs .tab-pane pre {
  max-height: none; }
/* an argument's edit: only the right pane shows it is busy */
html.shiny-busy.rp-user-busy body.rp-tpl:not(.rp-switching)::after { visibility: hidden; }
.rp-b-right { position: relative; }
html.shiny-busy body.rp-tpl .rp-b-right::after { content: attr(data-busy); position: absolute;
  top: .25rem; right: .5rem; z-index: 5; font-size: .75rem; padding: 0 .4rem;
  border-radius: .25rem; background: #ffc107; color: #000; }
body.rp-tpl .recalculating { opacity: 1 !important; }
body.rp-tpl .rp-tabs .recalculating { opacity: .35 !important; }
.rp-add-menu .dropdown-item.disabled { color: #9ca3af; }
"

.tpl_js <- "
$(document).on('shiny:connected', function() {
  var timer = null;
  // the report head of another report than the one opened: hidden
  function headCheck() {
    var want = document.body.getAttribute('data-rp-id');
    var h = $('#report_head [data-rp-head]');
    $('#report_head').toggleClass('rp-stale', !!want && h.length > 0 && h.attr('data-rp-head') !== want);
  }
  $(document).on('shiny:value', function(e) { if (e.name === 'report_head') setTimeout(headCheck, 0); });
  document.body.setAttribute('data-loading', $('.rp-report-loading').first().text() || 'Loading ...');
  Shiny.addCustomMessageHandler('rp-tpl-mode', function(x) {
    if (x.id) document.body.setAttribute('data-rp-id', x.id);
    headCheck();
    $('body').toggleClass('rp-tpl', !!x.tpl);
    // (the ARD step: a template report's is in its step 1)
    var ard = $('#step a[data-value=\"ard\"]');
    ard.parent().toggleClass('d-none', !!x.tpl);
    if (x.tpl && ard.hasClass('active')) {
      $('#step a[data-value=\"content\"]').tab('show');
      Shiny.setInputValue('step', 'content');
    }
    if (x.names) {
      $('.rp-step-name').each(function() {
        var k = $(this).attr('data-step');
        if (x.names[k]) $(this).text(x.names[k]);
      });
    }
    // (the right pane's ARD tab: a template report's)
    var at = $('#table_right a[data-value=\"ard\"]');
    at.parent().toggleClass('d-none', !x.tpl);
    if (!x.tpl && at.hasClass('active')) $('#table_right a[data-value=\"result\"]').tab('show');
    if (x.loading) {
      $('body').addClass('rp-switching');
      clearTimeout(timer);
      // (never left on: at most 30 s)
      timer = setTimeout(function() { $('body').removeClass('rp-switching rp-adding'); }, 30000);
    }
  });
  Shiny.addCustomMessageHandler('rp-tpl-card-show', function(x) {
    setTimeout(function() {
      var c = $('#tpl_card, #tpl_card_lf, #tpl_card_fig').filter(function() {
        return this.offsetParent !== null && $(this).children().length; }).first();
      if (c.length) c[0].scrollIntoView({block: 'start', behavior: 'smooth'});
    }, 150);
  });
  // the new report's dialog: its body scrolls, its OK always in view
  $(document).on('show.bs.modal', function(e) {
    if ($(e.target).find('#modal_id').length) $(e.target).find('.modal-dialog').addClass('modal-dialog-scrollable');
  });
  // OK of a new template report: its steps and Loading ... at the click
  $(document).on('click', '#add_ok', function() {
    if ($('input[name=\"modal_how\"]:checked').val() !== 'template') return;
    if ($(this).prop('disabled')) return;
    $('body').addClass('rp-tpl rp-switching rp-adding');
    clearTimeout(timer);
    timer = setTimeout(function() { $('body').removeClass('rp-switching rp-adding'); }, 30000);
  });
  Shiny.addCustomMessageHandler('rp-report-ready', function(x) {
    clearTimeout(timer);
    setTimeout(function() { $('body').removeClass('rp-switching rp-adding'); }, 50);
  });
  // a card just added: scrolled to and lit for a moment -- once the page
  // is idle (the re-drawn form and its parts in place); the menu's toggle
  // does not take the focus (and the scroll) back
  var wantCard = 0;
  $(document).on('click', '.rp-add-menu .dropdown-item', function() {
    wantCard = Date.now();
    var tg = $(this).closest('.rp-add-menu').find('.dropdown-toggle');
    setTimeout(function() { tg.blur(); }, 0);
  });
  function goCard() {
    setTimeout(function() {
      var c = $('.rp-card.rp-card-new').first();
      if (!c.length) return;
      var first = !c.hasClass('rp-card-flashed');
      var r = c[0].getBoundingClientRect();
      var bar = parseFloat(getComputedStyle(document.documentElement).getPropertyValue('--rp-top')) || 0;
      if (!first && r.top >= bar && r.top < window.innerHeight - 60) return;
      if (first) c.addClass('rp-card-flashed rp-card-flash rp-card-open');
      // (its head in view, below the page's sticky bar -- whichever box
      // scrolls: scroll-margin)
      var bar = parseFloat(getComputedStyle(document.documentElement).getPropertyValue('--rp-top')) || 0;
      c[0].style.scrollMarginTop = (bar + 12) + 'px';
      // (at once: the page's smooth scrolling would be cut short by the
      // drawing still going on)
      window.scrollTo({top: Math.max(0, window.scrollY + c[0].getBoundingClientRect().top - bar - 12),
                       behavior: 'instant'});
      if (first) setTimeout(function() { c.removeClass('rp-card-flash'); }, 1900);
    }, 120);
  }
  $(document).on('shiny:idle', function() {
    if (wantCard && Date.now() - wantCard < 6000) goCard(); else wantCard = 0;
  });
});
"

# Where an argument's value comes from -- the one place the card asks (so
# it can later read tflspec's own record, attr(<resolved output>,
# "template")$study): list(from = "own" | "study" | "default", study =
# the study standard's value, NULL none)
.tpl_arg_origin <- function(study, prm, args, filled = NULL) {
  src <- as.character(prm$study %||% NA_character_)[1L]
  if (prm$name %in% names(args)) {
    return(list(from = "own", study = .tpl_study_value(study, prm), source = src))
  }
  # (tflspec >= 0.0.24.9185 says which it filled from the study:
  # attr(<resolved output>, "template")$study, their values in $args)
  if (!is.null(filled) && prm$name %in% filled$study) {
    return(list(from = "study", study = filled$args[[prm$name]], source = src))
  }
  sv <- .tpl_study_value(study, prm)
  list(from = if (!is.null(sv)) "study" else "default", study = sv, source = src)
}

# What tflspec filled in from the study for a template report: the
# template attribute of its resolved output (NULL: none, or an older
# tflspec)
.tpl_filled <- function(p, output_id) {
  o <- p$spec$outputs[[output_id]]
  if (is.null(o)) return(NULL)
  r <- tryCatch(tflspec::tfl_resolve_output(o, p$spec$study), error = function(e) NULL)
  a <- attr(r, "template")
  if (is.null(a$study)) NULL else a
}

# A new report written straight into the spec: its line in spec/toc.yml
# (in the list's natural order, .natural_slot()) and its own file, the
# view's rows made from them alone -- not add_output() and the bridge over
# every report (#440: some 15 s for a study of 17 reports).  `output` a
# tfl_output; `label`, `section`, `population` the toc line's.
.spec_add_output <- function(p, output, label = NA_character_, section = NA_character_,
                             population = NA_character_) {
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  attr(p, "changed") <- NULL
  id <- output$id
  if (id %in% p$outputs$output_id) stop("Report '", id, "' is already on the list.", call. = FALSE)
  toc <- p$spec$study$toc %||% list()
  lines <- toc$outputs %||% list()
  ids <- vapply(lines, function(e) as.character(e$id %||% ""), "")
  entry <- list(id = id, kind = output$kind)
  if (length(label) && !is.na(label) && nzchar(label)) entry$label <- label
  if (length(section) && !is.na(section) && nzchar(section)) entry$section <- section
  if (length(population) && !is.na(population) && nzchar(population)) entry$population <- population
  k <- .natural_slot(ids, id)
  lines <- append(lines, list(entry), after = k - 1L)
  toc$outputs <- lines
  p$spec$study$toc <- toc
  rel <- file.path(study_layout()[["spec"]], "toc.yml")
  f <- tempfile("tflplanner-toc", fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tflspec::tfl_write_study_part(p$spec$study, "toc", f)
  p$spec$text[[rel]] <- readLines(f, encoding = "UTF-8", warn = FALSE)
  p <- .spec_set_output(p, id, output)
  # (the list in the toc's order, in the view and in the base alike)
  ord <- function(o) {
    o <- o[order(match(o$output_id, append(ids, id, after = k - 1L), nomatch = length(ids) + 2L),
                 seq_len(nrow(o))), , drop = FALSE]
    rownames(o) <- NULL
    o
  }
  p$outputs <- ord(p$outputs)
  if (!is.null(p$spec$base)) p$spec$base$outputs <- ord(p$spec$base$outputs)
  p
}

# A report's analysis set in its spec/toc.yml line too (the TOC and the
# header's <OUTPUT_POPULATION> read it): NA, none -- the line's taken out
.spec_set_toc_population <- function(p, output_id, population = NA_character_) {
  toc <- p$spec$study$toc
  lines <- toc$outputs %||% list()
  k <- match(output_id, vapply(lines, function(e) as.character(e$id %||% ""), ""))
  if (is.na(k)) return(p)
  has <- length(population) && !is.na(population[1L]) && nzchar(population[1L])
  if (identical(lines[[k]]$population, if (has) population[1L])) return(p)
  lines[[k]]$population <- if (has) population[1L]
  toc$outputs <- lines
  p$spec$study$toc <- toc
  rel <- file.path(study_layout()[["spec"]], "toc.yml")
  f <- tempfile("tflplanner-toc", fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tflspec::tfl_write_study_part(p$spec$study, "toc", f)
  p$spec$text[[rel]] <- readLines(f, encoding = "UTF-8", warn = FALSE)
  i <- match(output_id, p$outputs$output_id)
  if (!is.na(i) && !is.null(p$outputs$population)) p$outputs$population[i] <- if (has) population[1L] else NA
  p
}

# Timings of a template report's opening, for a check (#440):
# options(tflplanner.tpl_trace = TRUE) prints each step's time since the
# report was asked for; options(tflplanner.tpl_slow = <seconds>) waits that
# long in each drawing step (a slow machine, to see that no other screen
# is drawn meanwhile)
.tpl_clock <- new.env()
.tpl_trace <- function(step, start = FALSE) {
  if (start) .tpl_clock$t0 <- Sys.time()
  slow <- getOption("tflplanner.tpl_slow", 0)
  if (is.numeric(slow) && slow > 0 && !start) Sys.sleep(slow)
  if (!isTRUE(getOption("tflplanner.tpl_trace"))) return(invisible())
  t0 <- .tpl_clock$t0 %||% Sys.time()
  message(sprintf("[tpl] %6.2f s  %s", as.numeric(difftime(Sys.time(), t0, units = "secs")), step))
}

# The verbs of the lines after a template report's call, as its program
# has them (tflspec::tfl_content_code(): the study standard's items, and any
# the template adds after its call), in their order -- the cards shown
# with the report's own (one card a line, #440)
.tpl_code_fns <- function(p, output_id) {
  sp <- tryCatch(.code_spec(p), error = function(e) NULL)
  o <- sp$outputs[[output_id]]
  if (is.null(o) || is.null(.tpl_spec_name(o))) return(character())
  code <- tryCatch(tflspec::tfl_content_code(tflspec::tfl_resolve_output(o, sp$study)),
                   error = function(e) character())
  at <- grep("tpl_[A-Za-z0-9_]+\\(", code)
  if (!length(at)) return(character())
  rest <- code[seq_along(code) > at[1L]]
  m <- regmatches(rest, regexpr("^\\s*(plan_[A-Za-z0-9_]+)\\(", rest))
  sub("\\($", "", trimws(m))
}
.tpl_spec_name <- function(o) {
  t <- o$template
  if (is.list(t) && !is.null(t$name)) t$name else NULL
}

# A template's call as its program has it: tpl_demog() (a template with no
# call of its own -- a figure's -- tfl_template(<name>))
.tpl_call_name <- function(name) {
  x <- tryCatch(tflspec::tfl_templates(), error = function(e) NULL)
  call <- if (!is.null(x) && "call" %in% names(x)) x$call[match(name, x$name)] else NA
  if (length(call) && !is.na(call) && nzchar(call)) paste0(call, "()") else sprintf("tfl_template(%s)", name)
}

# A template report made a code one (the template card's "Convert to
# code"): the template's items, with the study's defaults and the report's
# own over them (tflspec::tfl_template_to_spec()), written into the report;
# the planner.  One way: the template is no longer the report's.
.tpl_to_code <- function(p, id) {
  sp <- .code_spec(p)
  o <- sp$outputs[[id]]
  if (is.null(o) || is.null(o$template)) stop(sprintf("%s is not a template report.", id), call. = FALSE)
  b <- tflspec::tfl_template_to_spec(o, sp$study)
  .spec_set_output(p, id, b)
}
