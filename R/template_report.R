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
  summary = "Continuous variables by a column key",
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
  for (a in names(p)) {
    if (!nzchar(p[[a]]$doc %||% "") && !is.na(.tpl_doc_fill[a])) p[[a]]$doc <- .tpl_doc_fill[[a]]
  }
  p
}

# A report's template, as its file says it (NULL: none)
.tpl_of <- function(p, output_id) {
  o <- p$spec$outputs[[output_id]]
  if (is.null(o) || is.null(o$template$name)) return(NULL)
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
.tpl_missing <- function(name, args) {
  prm <- .tpl_params(name)
  req <- names(prm)[vapply(prm, function(x) isTRUE(x$required), NA)]
  req[!vapply(req, function(a) length(args[[a]]) > 0L && any(nzchar(as.character(args[[a]]))), NA)]
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

# a meaning for the parameters whose template says none (their doc empty)
.tpl_doc_fill <- c(
  hierarchy = "the hierarchy's variables (SOC, PT)",
  where = "the records' condition",
  data_label = "the analysis data's label",
  analysis_id = "the analysis' id",
  analysis_label = "the analysis' label",
  overall = "the any-event row's label")

# The study standard's value of a parameter that says where it is
# (tfl_param(study = "<verb>.<argument>")): that argument of the verb's
# item in the study's defaults (NULL: none)
.tpl_study_value <- function(study, prm) {
  at <- as.character(prm$study %||% "")[1L]
  if (is.na(at) || !grepl(".", at, fixed = TRUE)) return(NULL)
  fn <- sub("\\..*$", "", at)
  arg <- sub("^[^.]*\\.", "", at)
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
.tpl_step_labels <- c(content = "1 Template (ARD and content)", page = "2 Page and output")

.tpl_css <- "
body:not(.rp-tpl) .rp-tpl-only, body.rp-tpl .rp-code-only { display: none !important; }
.rp-report-loading { display: none; }
body.rp-switching .rp-report-loading { display: block; }
body.rp-switching #step ~ .tab-content { visibility: hidden; }
.rp-tpl-default .form-control, .rp-tpl-default .selectize-input,
.rp-tpl-default .form-select, .rp-tpl-default .selectize-input .item {
  color: #8a8f98 !important; background-color: #f4f5f7; }
.rp-tpl-default .form-label { color: #6c757d; }
.rp-tpl-arg-from { font-size: .75rem; color: #6b7280; }
@keyframes rp-card-flash { 0% { box-shadow: 0 0 0 3px rgba(13,110,253,.55); }
  100% { box-shadow: 0 0 0 3px rgba(13,110,253,0); } }
.rp-card.rp-card-flash { animation: rp-card-flash 1.6s ease-out 1; }
.rp-add-menu .dropdown-menu { max-height: 60vh; overflow-y: auto; }
.rp-add-menu .dropdown-item.disabled { color: #9ca3af; }
"

.tpl_js <- "
$(document).on('shiny:connected', function() {
  var timer = null;
  Shiny.addCustomMessageHandler('rp-tpl-mode', function(x) {
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
      timer = setTimeout(function() { $('body').removeClass('rp-switching'); }, 30000);
    }
  });
  Shiny.addCustomMessageHandler('rp-report-ready', function(x) {
    clearTimeout(timer);
    setTimeout(function() { $('body').removeClass('rp-switching'); }, 50);
  });
  // a card just added: scrolled to and lit for a moment
  $(document).on('shiny:value', function() {
    setTimeout(function() {
      var c = $('.rp-card.rp-card-new').not('.rp-card-flashed').first();
      if (!c.length) return;
      c.addClass('rp-card-flashed rp-card-flash rp-card-open');
      c[0].scrollIntoView({block: 'center', behavior: 'smooth'});
      setTimeout(function() { c.removeClass('rp-card-flash'); }, 1700);
    }, 80);
  });
});
"

# Where an argument's value comes from -- the one place the card asks (so
# it can later read tflspec's own record, attr(<resolved output>,
# "template")$study): list(from = "own" | "study" | "default", study =
# the study standard's value, NULL none)
.tpl_arg_origin <- function(study, prm, args) {
  sv <- .tpl_study_value(study, prm)
  from <- if (prm$name %in% names(args)) "own" else if (!is.null(sv)) "study" else "default"
  list(from = from, study = sv)
}
