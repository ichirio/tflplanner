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
  vs_change = "A vital sign's change",
  ae_disc_soc_pt = "Adverse events leading to discontinuation by SOC and PT",
  ae_eair = "Exposure-adjusted incidence rates by SOC and PT",
  ae_ser_soc_pt = "Serious adverse events by SOC and PT",
  lab_abnormal = "Laboratory abnormalities",
  param_change = "Parameters' value and change",
  pk_conc = "PK concentrations by time point",
  stat_block = "Statistics from the study's ARD, a block a row",
  forest_hr = "Hazard ratio by subgroup (forest plot)",
  km = "Kaplan-Meier curves by group",
  km_survival = "Kaplan-Meier survival curves with a risk table",
  km_cumulative = "Cumulative incidence (1 - KM)",
  mean_ci = "Mean by visit with its CI",
  mean_se = "Mean by visit with its SE",
  waterfall = "Waterfall plot",
  ancova = "Analysis of covariance (ANCOVA) of a change")

# The templates of a kind of report: name and kind
.tpl_list <- function(kind) {
  x <- tryCatch(tflspec::tfl_templates(), error = function(e) NULL)
  x <- if (is.null(x) || !nrow(x)) data.frame(name = character(), kind = character()) else
    x[, c("name", "kind"), drop = FALSE]
  # (the series' templates first: "<series>/<template>")
  s <- .tpl_series_templates()
  x <- rbind(s[, c("name", "kind"), drop = FALSE], x)
  x <- x[x$kind %in% kind, , drop = FALSE]
  rownames(x) <- NULL
  x
}

# The templates of tflspec's series (tflspec#819/#824: tfl_series_list(), each
# series' templates/<t>.yml): name "<series>/<t>", kind (its `kind:`, else
# a `figure:` a figure, a listing's style a listing, else a table) and its
# label.  None
# for a tflspec with no series.
.tpl_series_templates <- function(lister = .tpl_series_lister()) {
  none <- data.frame(name = character(), kind = character(), label = character())
  s <- if (is.function(lister)) tryCatch(lister(), error = function(e) NULL)
  if (!is.data.frame(s) || !nrow(s)) return(none)
  # (a series found in two folders: the first, as tflspec reads it)
  s <- s[!duplicated(s$name), , drop = FALSE]
  out <- lapply(seq_len(nrow(s)), function(i) {
    files <- sort(list.files(file.path(s$path[[i]], "templates"), "\\.yml$", full.names = TRUE))
    if (!length(files)) return(NULL)
    rows <- lapply(files, function(fl) {
      top <- readLines(fl, n = 80L, warn = FALSE, encoding = "UTF-8")
      lab <- sub("^label:\\s*", "", grep("^label:", top, value = TRUE)[1L])
      # (the template's own `kind:` first -- km_cards is a figure with no
      # `figure:` key; else a `figure:` a figure, a listing's style a
      # listing, else a table)
      kind <- trimws(sub("^kind:\\s*", "", grep("^kind:", top, value = TRUE)[1L]))
      if (is.na(kind) || !kind %in% c("table", "listing", "figure")) {
        kind <- if (any(grepl("^figure:", top))) "figure" else
          if (any(grepl("^style:.*listing", top))) "listing" else "table"
      }
      data.frame(name = paste0(s$name[[i]], "/", sub("\\.yml$", "", basename(fl))), kind = kind,
                 label = if (is.na(lab)) "" else trimws(lab))
    })
    do.call(rbind, rows)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) none else out
}

# a name as a part of an input's id (letters, digits and _)
.tpl_id_part <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

# tflspec's tfl_series_list(), NULL for a tflspec that has none
.tpl_series_lister <- function() {
  if (requireNamespace("tflspec", quietly = TRUE)) {
    get0("tfl_series_list", envir = asNamespace("tflspec"), inherits = FALSE)
  }
}

# A template's one line for the list: the app's own (.tpl_desc), else a
# series template's label; "" for none
.tpl_desc_of <- function(name, series = .tpl_series_templates()) {
  if (!is.na(.tpl_desc[name])) return(.tpl_desc[[name]])
  lab <- series$label[match(name, series$name)]
  if (length(lab) && !is.na(lab)) lab else ""
}

# A template's parameters, named, in its order (none: a template unknown).
# A series template ("<series>/<template>") whose file tfl_arg_defs() reads:
# its params from those defs (.tpl_params_from_defs()); else tflspec's
# registered template's.
.tpl_params <- function(name) {
  defs <- .tpl_defs(name)
  if (!is.null(defs)) return(.tpl_params_from_defs(defs))
  # TODO(P1a-2): drop when series templates exist
  p <- tryCatch(tflspec::tfl_template_params(name), error = function(e) list())
  if (length(p) && is.null(names(p))) {
    names(p) <- vapply(p, function(x) x$name, "")
  }
  p
}

# A series template's argument definitions ("<series>/<template>": a
# name with a series), NULL for a template of no series or no file yet.
# Read from the open study's copy when it has one (a study reads only its
# copies, canon 2026-11-19; a template copied under a name of its own has
# no other), else from the series folders (a template not added yet).
.tpl_defs <- function(name) {
  if (!is.character(name) || length(name) != 1L || !grepl("^[^/]+/[^/]+$", name)) return(NULL)
  dir <- .tpl_study_dir()
  d <- if (!is.null(dir) && file.exists(.tpl_copy_file(dir, name))) {
    .arg_defs(template = name, study_dir = dir)
  } else {
    .arg_defs(template = name)
  }
  if (is.data.frame(d) && nrow(d)) d
}

# The open study's folder, which the template cards read the study's
# copies from: set by the app (`dir`), NULL when none is open
.tpl_ctx <- new.env(parent = emptyenv())
.tpl_study_dir <- function(dir) {
  if (missing(dir)) return(.tpl_ctx$dir)
  .tpl_ctx$dir <- if (length(dir) && !is.na(dir[1L]) && dir.exists(dir[1L])) dir[1L]
  invisible(.tpl_ctx$dir)
}

# A study's copy of a template "<series>/<name>"
# (<study>/spec/series/<series>/templates/<name>.yml)
.tpl_copy_file <- function(dir, name) {
  file.path(.series_copy_root(dir), sub("/.*$", "", name), "templates",
            paste0(sub("^[^/]*/", "", name), ".yml"))
}

# A series template copied into the study before a report uses it
# (tflspec::tfl_copy_templates(); a copy the study has is left as it is):
# the message saying so, NULL when it was there already or the template
# is of no series.  tflspec's error as it is.
.tpl_copy_into_study <- function(dir, name, t = identity) {
  if (is.null(dir) || !is.character(name) || !grepl("^[^/]+/[^/]+$", name)) return(NULL)
  d <- tflspec::tfl_copy_templates(dir, name)
  new <- d[d$status == "copied", , drop = FALSE]
  if (!nrow(new)) return(NULL)
  sprintf(t("Copied the template %s into this study (%s): the study reads its own copy from now on; the series screen lists it."),
          name, file.path(study_layout()[["spec"]], "series", sub("/.*$", "", name)))
}

# A parameter marked "key" (tflspec's tier: a default that stays on top of
# the card); a tflspec with no such tier, or a parameter with no tier: no
.tpl_is_key <- function(x) identical(x$tier, "key")

# The small "key" marker beside a key argument's label (tip: why it is on top);
# `t` the translator
.tpl_key_tag <- function(x, t) if (.tpl_is_key(x)) shiny::span(
  class = "badge rounded-pill rp-tpl-badge rp-tpl-badge-key",
  title = t("A setting often changed in this report; shown on top even at its default."),
  t("Main"))

# A required parameter (tier "required" or required = TRUE) that has no default
.tpl_required_empty <- function(x) {
  (isTRUE(x$required) || identical(x$tier, "required")) && is.null(x$default)
}

# The card's arguments of a section in two groups: `up`, kept on top -- the
# required, the report's own (in `args`) and the key ones, whatever their
# value -- and `low`, the others, folded away (their order kept).  `prm`: the
# section's parameters, named.
.tpl_card_groups <- function(prm, args) {
  nm <- names(prm)
  on_top <- vapply(nm, function(a) isTRUE(prm[[a]]$required) || a %in% names(args) ||
                     .tpl_is_key(prm[[a]]), NA)
  list(up = nm[on_top], low = nm[!on_top])
}

# A report's template, as its file says it (NULL: none; a figure
# designer's `template: km_simple`, a name, is no template SPEC)
.tpl_of <- function(p, output_id) {
  o <- p$spec$outputs[[output_id]]
  if (is.null(o) || !is.list(o$template) || is.null(o$template$name)) return(NULL)
  list(name = as.character(o$template$name), args = .tpl_args_of(o$template))
}

# The places a report may write its template's arguments, as tflspec reads
# them (#751 item 1).  A series template (`<series>/<name>`, tflspec's
# tfl_template_resolve() -> .tr_given()): the flat `args:`, the step
# sections `data:`, `analysis:`, `content:`, `page:` (a map, not one of the
# template's own parameters), and any other key directly under `template:`
# -- all read as one map in the file's order, a later one winning.  A
# built-in template (tflspec's .tpl_spec_args()): `args:` and the two parts
# `ard:` and `content:` (a name in two places stops there; here the first
# is kept).
.tpl_arg_steps <- c("data", "analysis", "content", "page")
.tpl_builtin_parts <- c("ard", "content")
.tpl_is_series <- function(name) grepl("/", as.character(name)[1L], fixed = TRUE)
.tpl_secs_for <- function(name) if (.tpl_is_series(name)) .tpl_arg_steps else .tpl_builtin_parts
.tpl_is_map <- function(v) is.list(v) && !is.null(names(v))

# A report's template arguments as one map (`t`: its `template:`)
.tpl_args_of <- function(t) {
  name <- as.character(t$name)[1L]
  own <- tryCatch(names(.tpl_params(name)), error = function(e) character())
  if (!.tpl_is_series(name)) {
    args <- as.list(t$args %||% list())
    for (k in .tpl_builtin_parts) {
      v <- t[[k]]
      if (!.tpl_is_map(v)) next
      for (a in setdiff(names(v), names(args))) args[a] <- list(v[[a]])
    }
    return(args)
  }
  out <- list()
  for (k in setdiff(names(t), "name")) {
    v <- t[[k]]
    if (k %in% c("args", .tpl_arg_steps) && !k %in% own && .tpl_is_map(v)) {
      for (a in names(v)) out[a] <- list(v[[a]])
    } else {
      out[k] <- list(v)
    }
  }
  out
}

# The step sections a report's `template:` has (maps, not one of the
# template's own parameters), in the file's order
.tpl_step_secs <- function(t) {
  name <- as.character(t$name)[1L]
  own <- tryCatch(names(.tpl_params(name)), error = function(e) character())
  ks <- setdiff(intersect(names(t), .tpl_secs_for(name)), own)
  ks[vapply(ks, function(k) .tpl_is_map(t[[k]]), NA)]
}

# The arguments a series template's report writes directly under
# `template:` (no section), in the file's order
.tpl_bare_args <- function(t) {
  if (!.tpl_is_series(t$name)) return(character())
  setdiff(names(t), c("name", "args", .tpl_step_secs(t)))
}

# A report's `template:` with its arguments written in the form the file
# already uses: flat stays flat (`args:`); by step stays by step, an
# argument in the section (or the flat `args:`, or directly under
# `template:`) it was in, a new one in the section of its `step:`
# (tfl_arg_defs(template =)), else flat.  `by_step`: write a report with no
# arguments yet by step (a new report, the canon's form).  An emptied
# section is dropped (tflspec reads `analysis: {}` as an argument).
.tpl_write_args <- function(t, args, by_step = FALSE) {
  secs <- .tpl_step_secs(t)
  bare_old <- .tpl_bare_args(t)
  if (!length(secs) && !length(bare_old) && !(by_step && !length(t$args))) {
    t$args <- args
    return(t)
  }
  name <- as.character(t$name)[1L]
  old <- lapply(stats::setNames(secs, secs), function(k) t[[k]])
  flat_old <- names(t$args %||% list())
  out <- old
  out[] <- list(list())
  flat <- list()
  bare <- list()
  for (a in names(args)) {
    k <- if (a %in% bare_old) "" else if (a %in% flat_old) NA_character_ else {
      w <- secs[vapply(secs, function(k) a %in% names(old[[k]]), NA)]
      if (length(w)) w[[length(w)]] else .tpl_arg_step(name, a, secs)
    }
    if (identical(k, "")) {
      bare[a] <- list(args[[a]])
    } else if (is.na(k)) {
      flat[a] <- list(args[[a]])
    } else {
      out[[k]][a] <- list(args[[a]])
    }
  }
  # (the file's order kept, a new argument after it)
  keep_order <- function(x, was) x[c(intersect(was, names(x)), setdiff(names(x), was))]
  for (a in setdiff(bare_old, names(bare))) t[[a]] <- NULL
  for (a in names(bare)) t[a] <- list(bare[[a]])
  for (k in secs) {
    out[[k]] <- keep_order(out[[k]], names(old[[k]]))
    t[[k]] <- NULL
  }
  t$args <- if (length(flat)) keep_order(flat, flat_old)
  for (k in names(out)) if (length(out[[k]])) t[[k]] <- out[[k]]
  t
}

# The section a new argument of a report by step goes to: its `step:` as
# the template's form names it (a built-in template's analysis step is its
# `ard:` part); NA (flat) when the template does not say, or the step is no
# section of that form
.tpl_arg_step <- function(name, arg, secs = character()) {
  st <- as.character(.arg_def(.tpl_defs(name), arg)$step %||% NA_character_)[1L]
  if (is.na(st)) return(NA_character_)
  if (.tpl_is_series(name)) {
    if (identical(st, "ard")) st <- "analysis"
  } else if (identical(st, "analysis")) {
    st <- "ard"
  }
  if (st %in% .tpl_secs_for(name)) st else NA_character_
}

# The study's datasets, by dataset (upper case), read once a study folder:
# through spec/data.yml's paths and readers (`planner`'s datasets, as the
# programs read them -- an .xpt too, #518), else the .rds files under data/
.tpl_cols_cache <- new.env()
.tpl_data_cols <- function(root, planner = NULL) {
  if (is.null(root)) return(list())
  ds <- if (!is.null(planner)) toupper(stats::na.omit(planner$ard$datasets$dataset))
  key <- paste(c(normalizePath(root, "/", mustWork = FALSE), sort(ds)), collapse = "|")
  if (!is.null(.tpl_cols_cache[[key]])) return(.tpl_cols_cache[[key]])
  out <- if (length(ds)) {
    .study_data(list(planner = planner, path = root), ds)
  } else {
    files <- list.files(file.path(root, "data"), "\\.(rds|RDS)$", recursive = TRUE, full.names = TRUE)
    o <- list()
    for (f in files) {
      d <- tryCatch(readRDS(f), error = function(e) NULL)
      if (is.data.frame(d)) o[[toupper(sub("\\.[^.]*$", "", basename(f)))]] <- d
    }
    o
  }
  .tpl_cols_cache[[key]] <- out
  out
}

# The columns a parameter chooses from: the dataset's the report reads
# (its `data` argument or default), else every dataset's
.tpl_columns <- function(data, dataset = NULL) {
  .tpl_dataset_columns(data, dataset) %||% unique(unlist(lapply(data, names)))
}

# A dataset's columns, by its name or a template's data id (adsl_sub: its
# dataset, ADSL -- as .tpl_vars_dataset() reads it); NULL when the study
# has no such dataset
.tpl_dataset_columns <- function(data, dataset) {
  ds <- toupper(as.character(dataset %||% "")[1L])
  if (is.na(ds) || !nzchar(ds)) return(NULL)
  for (d in unique(c(ds, sub("_.*$", "", ds)))) {
    if (!is.null(data[[d]])) return(names(data[[d]]))
  }
  NULL
}

# The dataset a series template's parameter takes its columns from: its
# choices say `vars(adsl_sub)`, the data row the template reads -- named by
# its dataset (adsl_sub: ADSL).  NULL when it says none of the datasets there are.
.tpl_vars_dataset <- function(prm, data) {
  for (w in tryCatch(.choice_words(prm$sources), error = function(e) character())) {
    q <- .choice_word_parse(w)
    if (!identical(q$name, "vars") || !length(q$args)) next
    ds <- toupper(sub("_.*$", "", as.character(q$args[[1L]])))
    if (ds %in% names(data)) return(ds)
  }
  NULL
}

.tpl_is_varlike <- function(x) {
  is.character(x) && length(x) > 0L && is.null(names(x)) &&
    all(grepl("^[A-Z][A-Z0-9_]*$", x))
}

# The input a parameter gets: population, data, var (one column), vars
# (columns, in order), levels (a variable's values), flag, number, text,
# chips (several texts), choice (one of tfl_param(choices = )), choices
# (several of them), pick / picks (from a def's source words), list (a small table of lines, "a | b"), columns (a
# listing's: R/tpl_columns_ui.R).  By the type
# tflspec declares (tfl_param(type = )); guessed from the default and the
# doc only when it declares none.
.tpl_type <- function(prm, columns = character()) {
  n <- prm$name
  if (identical(n, "population")) return("population")
  if (identical(n, "data")) return("data")
  several <- isTRUE(prm$multiple)
  ty <- prm$type
  if (identical(ty, "table") && !is.null(.tpl_table_spec(prm))) return("table")
  if (.tpl_is_columns(prm)) return("columns")
  if (length(ty) == 1L && nzchar(ty)) {
    return(switch(ty,
      vars = if (several) "vars" else "var",
      levels = "levels",
      flag = "flag",
      number = "number",
      r = "r",
      text = if (several) "chips" else "text",
      choice = if (several) "choices" else "choice",
      pick = if (several) "picks" else "pick",
      list = "list",
      lines = "lines",
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

# An R parameter's value as the text its field shows (a tfl_r's text, a
# string as it is, a logical or number as R writes it); "" for none
.tpl_r_text <- function(x) {
  if (is.null(x) || !length(x)) return("")
  if (inherits(x, "tfl_r") || is.character(x)) return(paste(as.character(unclass(x)), collapse = "\n"))
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
}

# The R field of a template card (the one the builder's R conditions use:
# monospace, the dataset's columns offered as it is typed); `id` its input id
.tpl_r_field <- function(id, label, value, cols = character(), placeholder = NULL) {
  dl <- paste0(id, "__dl")
  w <- shiny::textInput(id, label, value = .tpl_r_text(value), width = "100%", updateOn = "blur",
                        placeholder = placeholder)
  w <- htmltools::tagQuery(w)$find("input")$addClass("rp-mono rp-condition")$
    addAttrs(spellcheck = "false", list = if (length(cols)) dl)$allTags()
  shiny::tagList(w, if (length(cols)) shiny::tags$datalist(
    id = dl, lapply(unique(as.character(cols)), function(x) shiny::tags$option(value = x))))
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
  if (is.null(x) || !length(x) || .is_unset(x)) return("")
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
    # (a whole number as one: a column's width 15, not 15.0)
    if (grepl("^-?[0-9]+$", v)) return(as.integer(v))
    if (grepl("^-?[0-9]+(\\.[0-9]+)?$", v)) return(as.numeric(v))
    if (several && grepl(",", v, fixed = TRUE)) return(trimws(strsplit(v, ",", fixed = TRUE)[[1L]]))
    v
  }
  if (.tpl_list_records(prm)) {
    f <- .tpl_list_fields(prm)
    recs <- lapply(cells, function(cs) {
      r <- list()
      for (i in seq_along(f)) {
        if (i > length(cs)) break
        v <- val(cs[[i]], several = TRUE)
        if (!is.null(v)) r[[f[[i]]]] <- v
      }
      r
    })
    # (a line of empty cells is no record)
    recs <- recs[lengths(recs) > 0L]
    return(if (length(recs)) recs)
  }
  keys <- vapply(cells, function(cs) cs[[1L]], "")
  vals <- vapply(cells, function(cs) if (length(cs) > 1L) paste(cs[-1L], collapse = " | ") else "", "")
  # (a name with no value is not an entry: an empty cell is "not given",
  # never "" -- the rule of an empty field)
  keep <- nzchar(keys) & nzchar(vals)
  if (!any(keep)) return(NULL)
  # (a map whose value is several, fields `values`: a line's comma-separated
  # values as a vector -- RACE | ASIAN, WHITE)
  if (identical(.tpl_list_fields(prm)[2L], "values")) {
    return(stats::setNames(lapply(vals[keep], function(v) trimws(strsplit(v, ",", fixed = TRUE)[[1L]])), keys[keep]))
  }
  # (a rule a side, fields `edge`, `line`: TRUE / FALSE as a flag, a width as a number)
  if (identical(.tpl_list_fields(prm)[2L], "line")) {
    return(stats::setNames(lapply(vals[keep], val), keys[keep]))
  }
  stats::setNames(vals[keep], keys[keep])
}

# A sequence of plain values (a "lines" field, .tpl_param_type(): list<string>,
# list<number>, list<integer>) as the text of its box, a value a line; "" for
# none.  Only the values are shown: a map written where a sequence belongs
# (a map of "" the add-report dialog once wrote) shows its non-empty values,
# so its echo reads back as the sequence they make (none: not given).  An
# entry of several lines is shown as its lines (a field of this kind holds
# none: a heading's lines are its entries).
.tpl_lines_text <- function(x) {
  if (is.null(x) || !length(x) || .is_unset(x)) return("")
  v <- as.character(unlist(x, use.names = FALSE))
  paste(v[!is.na(v) & nzchar(trimws(v))], collapse = "\n")
}

# ... and the box read back: a value a line (blank lines dropped; NULL for
# none).  Lines that all start "- " are YAML (.tpl_yaml_read()).  A number
# type's lines are numbers when every one is (.tpl_whole()), else they stay
# as typed; a string's keep their leading spaces (an indented heading line).
.tpl_lines_parse <- function(text, prm = NULL) {
  txt <- paste(as.character(text %||% ""), collapse = "\n")
  lines <- sub("[ \t\r]+$", "", strsplit(txt, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(NULL)
  if (all(grepl("^\\s*- ", lines))) {
    v <- .tpl_yaml_read(paste(lines, collapse = "\n"))
    if (is.list(v) && is.null(names(v)) &&
          all(vapply(v, function(e) is.atomic(e) && length(e) == 1L, NA))) v <- unlist(v)
    return(v)
  }
  ty <- as.character(prm$def$type %||% "")[1L]
  if (grepl("^list<(number|integer)>$", ty)) {
    n <- suppressWarnings(as.numeric(trimws(lines)))
    if (!anyNA(n)) return(.tpl_whole(n, prm))
  }
  lines
}

# A value as one line to read (a default; a list's value): a plain string as
# itself, anything else in YAML flow form (`[a, b]`, `{k: v}`, `!r code` for
# raw R) -- what .tpl_text_parse() reads back
.tpl_show <- function(x) {
  if (is.null(x)) return("NULL")
  if (.is_unset(x)) return(.none_word)
  if (is.character(x) && length(x) == 1L && is.null(names(x)) && !inherits(x, "tfl_r")) return(x)
  .tpl_yaml_flow(x)
}

# The YAML handlers the card reads text with (`!r` is raw R: a tfl_r)
# (a sequence holding an `!r` or a map stays a list, as in tflspec's reader, so a
# guard's R keeps its class; one of plain scalars of one kind is a vector)
.tpl_yaml_handlers <- function() {
  seq <- function(x) {
    if (!length(x)) return(list())
    if (any(vapply(x, function(e) inherits(e, "tfl_r") || is.list(e), NA))) return(x)
    if (all(vapply(x, function(e) is.atomic(e) && length(e) == 1L, NA)) &&
          length(unique(vapply(x, function(e) class(e)[1L], ""))) == 1L) return(unlist(x))
    x
  }
  list(r = function(x) tflspec::tfl_r(as.character(x)), seq = seq)
}

# A value in YAML flow form (one line).  A string is quoted only when its
# plain form would not read back as itself.
.tpl_yaml_flow <- function(x) {
  back <- function(txt) tryCatch(yaml::yaml.load(paste0("[", txt, "]"), handlers = .tpl_yaml_handlers())[[1L]],
                                 error = function(e) structure(list(), failed = TRUE))
  quote_it <- function(s) {
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    s <- gsub("\"", "\\\"", s, fixed = TRUE)
    s <- gsub("\n", "\n", s, fixed = TRUE)
    s <- gsub("\t", "\t", s, fixed = TRUE)
    paste0("\"", s, "\"")
  }
  str <- function(s) {
    if (is.na(s)) return("null")
    if (!nzchar(s)) return("\"\"")
    if (identical(back(s), s)) s else quote_it(s)
  }
  scalar <- function(v) {
    if (inherits(v, "tfl_r")) {
      s <- as.character(unclass(v))[1L]
      plain <- paste0("!r ", s)
      if (identical(back(plain), v)) plain else paste0("!r ", quote_it(s))
    } else if (is.null(v)) "null"
    else if (is.logical(v)) if (is.na(v)) "null" else if (v) "true" else "false"
    else if (is.numeric(v)) {
      if (is.na(v)) ".nan" else if (is.infinite(v)) (if (v > 0) ".inf" else "-.inf") else
        format(v, scientific = FALSE, drop0trailing = TRUE, trim = TRUE, digits = 15)
    } else str(as.character(v))
  }
  key <- function(k) if (nzchar(k)) str(k) else "\"\""
  go <- function(v) {
    if (inherits(v, "tfl_r") || is.null(v)) return(scalar(v))
    if (is.list(v)) {
      nm <- names(v)
      items <- vapply(v, go, "")
      if (!is.null(nm) && all(nzchar(nm))) {
        return(paste0("{", paste(paste0(vapply(nm, key, ""), ": ", items), collapse = ", "), "}"))
      }
      return(paste0("[", paste(items, collapse = ", "), "]"))
    }
    if (length(v) == 1L && is.null(names(v))) return(scalar(v))
    nm <- names(v)
    items <- vapply(seq_along(v), function(i) scalar(v[[i]]), "")
    if (!is.null(nm) && all(nzchar(nm))) {
      return(paste0("{", paste(paste0(vapply(nm, key, ""), ": ", items), collapse = ", "), "}"))
    }
    paste0("[", paste(items, collapse = ", "), "]")
  }
  go(x)
}

# A list value in YAML block form (several lines), for a list the rows box
# cannot show: a map's key on a line, its value after it, or indented below;
# a sequence's entry a `- ` line.  A leaf is written as .tpl_yaml_flow() does
# (`!r code` for raw R; numbers as typed digits), so .tpl_yaml_read() reads it back.
.tpl_yaml_block <- function(x) {
  leaf <- function(v) inherits(v, "tfl_r") || is.null(v) || (!is.list(v) && length(v) <= 1L) ||
    (is.list(v) && !length(v))
  # (raw R plain, as a person writes it in a block, when that reads back as itself)
  one <- function(e) {
    if (inherits(e, "tfl_r")) {
      s <- as.character(unclass(e))[1L]
      plain <- paste0("!r ", s)
      ok <- !grepl("[\r\n]", s) && identical(tryCatch(yaml::yaml.load(paste0("- ", plain), handlers = .tpl_yaml_handlers())[[1L]],
                                                       error = function(err) NULL), e)
      if (ok) return(plain)
    }
    .tpl_yaml_flow(e)
  }
  go <- function(v, pad) {
    if (is.list(v) || (is.atomic(v) && length(v) > 1L && is.null(names(v)))) {
      items <- as.list(v)
      nm <- names(items)
      if (!is.null(nm) && all(nzchar(nm))) {
        return(unlist(lapply(seq_along(items), function(i) {
          k <- .tpl_yaml_flow(nm[[i]])
          if (leaf(items[[i]]) || !is.null(names(items[[i]])) && is.atomic(items[[i]])) {
            paste0(pad, k, ": ", one(items[[i]]))
          } else c(paste0(pad, k, ":"), go(items[[i]], paste0(pad, "  ")))
        }), use.names = FALSE))
      }
      return(unlist(lapply(items, function(e) paste0(pad, "- ", one(e))), use.names = FALSE))
    }
    paste0(pad, one(v))
  }
  if (is.null(x) || (is.list(x) && !length(x))) return("")
  paste(go(x, ""), collapse = "\n")
}

# ... and the YAML text of a list read back (NULL: empty).  An error (not YAML,
# or not a map / sequence) is raised for the caller to say.
.tpl_yaml_read <- function(text) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  if (!nzchar(trimws(text))) return(NULL)
  r <- yaml::yaml.load(text, handlers = .tpl_yaml_handlers())
  if (is.null(r) || (is.list(r) && !length(r))) return(NULL)
  # a sequence of one entry (`- label`, `[label]`) is read as one value by
  # yaml: it is a list as typed, not a scalar
  lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1L]])
  first <- lines[nzchar(lines) & !startsWith(lines, "#")][1L]
  if (grepl("^(-( |$)|[[])", first)) return(r)
  if (!is.list(r) && is.null(names(r)) && length(r) == 1L) {
    stop("a map (`name: value` lines) or a list (`- value` lines) is expected", call. = FALSE)
  }
  r
}

# Text in the card's box read as a value.  One starting with `[` or `!r ` is
# YAML (`!r` raw R, as .tpl_show() writes it): an error (not YAML) is raised
# for the caller to say.  One starting with `{` is a map only when it parses
# to a named list with no empty value; otherwise (`{n} ({p:.1f%})`, `{n}`: a
# format) it is the string as typed, with no error.  Any other text is a string.
.tpl_text_parse <- function(v) {
  if (!is.character(v) || length(v) != 1L) return(v)
  if (grepl("^([[]|!r )", v)) return(yaml::yaml.load(v, handlers = .tpl_yaml_handlers()))
  if (!startsWith(v, "{")) return(v)
  r <- tryCatch(yaml::yaml.load(v, handlers = .tpl_yaml_handlers()), error = function(e) NULL)
  if (is.list(r) && length(r) && !is.null(names(r)) && all(nzchar(names(r))) &&
        !any(vapply(r, is.null, NA))) r else v
}

# A number as typed: a whole one an integer where the argument is a count
# -- its default an integer, or its name says so (max_rows, n_*, *_digits)
# -- so the SPEC YAML says 25, not 25.0
.tpl_whole <- function(v, prm) {
  count <- is.integer(prm$default) || grepl("(^n_|_n$|rows$|lines$|digits$)", prm$name %||% "")
  if (count && all(is.finite(v)) && all(v == round(v)) && all(abs(v) < .Machine$integer.max)) as.integer(v) else v
}

# An edit of a list shown as YAML text: `same` (the text it was drawn with, no
# edit), `bad` (not YAML, or not a map / list: the value stays; `msg` says why)
# or `edit` (`value`, the list read back).  `shown`: the value drawn.
.tpl_yaml_edit <- function(text, shown) {
  if (identical(trimws(paste(as.character(text), collapse = "\n")), trimws(.tpl_yaml_block(shown)))) {
    return(list(kind = "same"))
  }
  r <- tryCatch(list(value = .tpl_yaml_read(text)), error = function(e) conditionMessage(e))
  if (is.character(r)) return(list(kind = "bad", msg = r))
  list(kind = "edit", value = r$value)
}

# What an input gives, as the argument's value (NULL: none -- the default)
.tpl_value <- function(v, type, prm = NULL) {
  if (is.null(v)) return(NULL)
  # ("(none)" on an argument that may be left out: `!unset`; among other
  # items of a many-valued field it is just not one of them)
  # (in any language: "(none)", "\uff08\u306a\u3057\uff09")
  nn <- is.character(v) && !isTRUE(prm$required) && any(trimws(v) %in% .none_texts())
  if (nn) {
    if (length(v) == 1L) return(tflspec::tfl_unset())
    v <- v[!trimws(v) %in% .none_texts()]
  }
  switch(type,
    flag = isTRUE(v),
    number = {
      v <- suppressWarnings(as.numeric(v))
      if (!length(v) || is.na(v)) NULL else .tpl_whole(v, prm)
    },
    vars = , chips = , choices = , picks = , levels = {
      v <- as.character(v)
      v <- v[nzchar(trimws(v))]
      if (!length(v)) NULL else if (identical(type, "levels") && !isTRUE(prm$multiple)) v[1L] else v
    },
    list = , columns = .tpl_list_parse(v, prm),
    lines = .tpl_lines_parse(v, prm),
    table = if (.tbl_is_state(v)) .tbl_read(v),
    # (R as typed, never YAML: TRUE stays the text TRUE, `x: y` stays that text)
    r = {
      v <- trimws(paste(as.character(v), collapse = "\n"))
      if (nzchar(v)) tflspec::tfl_r(v)
    },
    {
      v <- trimws(paste(as.character(v), collapse = "\n"))
      # (YAML flow or `!r`: what .tpl_show() writes; a text that does not parse is kept as typed --
      # the card checks it first with .tpl_text_parse())
      if (nzchar(v)) tryCatch(.tpl_text_parse(v), error = function(e) v) else NULL
    })
}

# What a field drawn with `shown` (tpl_widget()) sends before anyone touches
# it, read back as .tpl_value() reads it: the value an untouched field
# gives -- not always `shown` itself (a FALSE text says "false", a list's
# vectors come back as its lines say them, NULL a flag is FALSE)
.tpl_untouched <- function(shown, type, prm = NULL) {
  raw <- tryCatch(switch(type,
    flag = isTRUE(shown),
    number = .num_or_null(shown),
    text = if (is.null(shown)) "" else .tpl_show(shown),
    r = .tpl_r_text(shown),
    # (a list the rows cannot show is drawn as its YAML box)
    list = .tpl_list_text(shown, prm) %||% .tpl_yaml_block(shown),
    columns = .tpl_list_text(shown, prm),
    lines = .tpl_lines_text(shown),
    table = .tpl_table_state(shown, prm),
    vars = , chips = , choices = , picks = as.character(shown %||% character()),
    levels = if (isTRUE(prm$multiple)) as.character(shown %||% character()) else
      as.character(shown %||% "")[1L],
    as.character(shown %||% "")[1L]), error = function(e) NULL)
  if (is.null(raw)) return(NULL)
  tryCatch(.tpl_field_arg(raw, type, prm, shown, echo = FALSE), error = function(e) NULL)
}

# A template argument's field read back the way tpl_widget() drew it -- the
# add-report dialog and the card both read through here: a list the
# "name | text" rows cannot show is its YAML box (.tpl_yaml_read()), any other
# field its own reader (.tpl_value()).  The rule of an empty field:
#   NULL          "not given" (the argument left out, the template's
#                 default): an empty or blank field, a list whose entries
#                 are all empty, and (`echo`) a field that says what it sent
#                 untouched when drawn with `shown` (.tpl_untouched());
#   tfl_unset()   "(none)";
#   the value     else, a list's empty entries ("", NA, none) dropped.
# `shown`: the value the field was drawn with.  An error: a YAML box that
# does not read (the caller says so).  No Shiny.
.tpl_field_arg <- function(raw, type, prm = NULL, shown = NULL, echo = TRUE) {
  if (is.null(raw)) return(NULL)
  if (identical(type, "list") && is.null(.tpl_list_text(shown, prm))) {
    txt <- paste(as.character(raw), collapse = "\n")
    if (!isTRUE(prm$required) && trimws(txt) %in% .none_texts()) return(tflspec::tfl_unset())
    v <- .tpl_yaml_read(txt)
  } else {
    v <- .tpl_value(raw, type, prm)
  }
  if (.is_unset(v)) return(v)
  if (type %in% c("list", "lines", "columns")) v <- .tpl_drop_empty(v)
  if (is.null(v)) return(NULL)
  if (echo && !is.null(shown) && identical(v, .tpl_untouched(shown, type, prm))) return(NULL)
  v
}

# A list value's empty entries dropped (none, NA, "" or a blank text, an
# empty vector or list); NULL when none is left.  R code and other tagged
# values are kept as they are.
.tpl_drop_empty <- function(v) {
  if (is.null(v) || is.object(v) || !(is.list(v) || is.atomic(v))) return(v)
  empty <- vapply(as.list(v), function(e) {
    is.null(e) || (!is.object(e) && (is.atomic(e) || is.list(e)) &&
                     (!length(e) || (is.atomic(e) && length(e) == 1L &&
                                       (is.na(e) || !nzchar(trimws(as.character(e)))))))
  }, NA)
  v <- v[!empty]
  if (length(v)) v
}

# The arguments the add-report dialog writes for template `prm_all`
# (.tpl_params()), from what its fields send: `raws` by argument name (a
# pick's "Add all suggested" already expanded; `population` the report's
# population field where the template hides it, .tpl_asks_report_pop();
# returned as attribute "population"), `nones` by name a
# switch's <id>__none input, `study` the study SPEC (its values prefill the
# fields).  An optional argument is written only when its field says other
# than what it sent untouched (.tpl_field_arg()); a required one empty
# gives no args and attribute "missing" (its name).  No Shiny.
.tpl_dialog_args <- function(prm_all, raws, study = NULL, nones = list()) {
  args <- list()
  # (the report's population, for a template that hides it: not an argument;
  # required for one whose set is the report's alone, demog)
  pop <- NULL
  if (.tpl_asks_report_pop(prm_all)) {
    pop <- .tpl_value(raws[["population"]], "population", prm_all$population)
    if (inherits(pop, "tfl_unset")) pop <- NULL
    # (required: a hidden one that is the report's alone (demog), or a shown
    # one the template needs (forest_hr); not a KM type's, which may be left out)
    if (is.null(pop) && isTRUE(prm_all[["population"]]$required) && !.tpl_own_data(prm_all)) {
      return(structure(list(), missing = "population"))
    }
  }
  for (x in .tpl_dialog_shown(prm_all)) {
    raw <- raws[[x$name]]
    type <- .tpl_type(x)
    start <- .tpl_study_value(study, x) %||% x$default
    if (!isTRUE(x$required)) {
      # (an optional one: written only when changed from what its field
      # sent untouched -- not compared with the start value itself: a
      # FALSE shown as "false", a list's vectors as its lines, are no edit)
      if (is.null(raw)) next
      # (a switch the "(none)" button turned off: its own input, <id>__none)
      none_in <- nones[[x$name]]
      if (identical(type, "flag") && isTRUE(none_in)) raw <- .none_word
      # (a YAML box that does not read: not written)
      v <- tryCatch(.tpl_field_arg(raw, type, x, start), error = function(e) NULL)
      if (.tpl_flag_off_drawn(type, v, start, none_in)) next
      # (an unset only equals an unset; other values: as the field sent them untouched)
      same <- .same_unset(v, start)
      if (is.null(v) || isTRUE(same)) next
      if (is.na(same) && (identical(v, .tpl_untouched(start, type, x)) ||
                            identical(as.character(unlist(v)), as.character(unlist(start))))) next
      args[[x$name]] <- v
      next
    }
    v <- tryCatch(.tpl_field_arg(raw, type, x, start, echo = FALSE), error = function(e) NULL)
    if (is.null(v)) return(structure(list(), missing = x$name))
    # (the study's own value, as prefilled: left to the study)
    sv <- .tpl_study_value(study, x)
    if (!is.null(sv) && identical(as.character(unlist(v)), as.character(unlist(sv)))) next
    args[[x$name]] <- v
  }
  if (!is.null(pop)) {
    attr(args, "population") <- pop
    if (.tpl_pop_is_arg(prm_all)) args$population <- pop
  }
  args
}

# A table parameter (a def's input "table": a map<K,V> or a rows<...>,
# R/arg_defs_table.R): its table's columns (NULL: its type is no table, or
# it has no columns to show); `ctx` what the card knows (.tpl_table_ctx())
.tpl_table_spec <- function(prm, ctx = NULL, value = NULL, lang = "en") {
  d <- prm$def
  if (!is.list(d) || is.null(d$type)) return(NULL)
  .tbl_spec(d$type, def = d, ctx = ctx, value = value, lang = lang)
}

# The state an untouched table field holds (NULL: no table)
.tpl_table_state <- function(shown, prm) {
  spec <- .tpl_table_spec(prm, value = shown)
  if (!is.null(spec)) .tbl_state(spec, shown)
}

# A table's value as one text to compare (a data frame and its records, read
# back from the field, say the same): each row its non-blank cells by name,
# the rows in order
.tpl_table_key <- function(x, prm) {
  spec <- .tpl_table_spec(prm, value = x)
  if (is.null(spec)) return("")
  rows <- lapply(.tbl_rows(spec, x), function(r) {
    names(r) <- spec$cols
    r <- r[nzchar(r)]
    if (identical(spec$shape, "rows")) r[order(names(r))] else r
  })
  rows <- Filter(length, rows)
  as.character(jsonlite::toJSON(lapply(rows, as.list), auto_unbox = TRUE))
}

# What a template card knows for a table's choices (the ctx of
# .choice_values()): the columns of a data row (vars(<id>): the dataset its
# `from` reads, .tpl_data_rows(); else a dataset of that name, else the one
# its id begins with -- adsl_sub: ADSL; else the report's dataset), a
# variable's values, the card's arguments and the template's params.
# `rows`: the template's data rows and the report's own (.tpl_data_rows());
# their ids are the report's data (report_data), NULL: the report's dataset
.tpl_table_ctx <- function(data, dataset = NULL, args = list(), params = list(),
                           populations = NULL, rows = NULL) {
  ds <- function(x) {
    x <- toupper(as.character(unlist(x)))
    unique(x[!is.na(x) & nzchar(x)])
  }
  has_rows <- is.data.frame(rows) && nrow(rows) > 0L
  list(
    # (the source words of a pick: the study's data, the report's -- the ids
    # of its data rows (tflspec's arg defs: a data row's id), else its `data`
    # argument or dataset -- and the study's analysis sets)
    study_data = ds(names(data)),
    report_data = if (has_rows) unique(rows$id) else ds(c(args$data, if (is.null(args$data)) dataset)),
    populations = populations,
    # (vars(adsl_sub): the data row's dataset's columns -- not every
    # dataset's: its `from`, else the dataset its id names or begins with,
    # else the report's dataset)
    vars = function(what = NULL) {
      w <- as.character(what %||% "")[1L]
      k <- if (has_rows && !is.na(w)) match(w, rows$id) else NA_integer_
      from <- if (!is.na(k)) rows$from[[k]]
      .tpl_dataset_columns(data, from) %||% .tpl_dataset_columns(data, w) %||%
        .tpl_columns(data, dataset)
    },
    levels = function(what = NULL) .tpl_levels(data, as.character(what %||% NA_character_)[1L], dataset),
    args = args,
    params = names(params))
}

# A series template's file (<series>/templates/<template>.yml), NULL for
# none (a template of tflspec's registry, a series not found)
.tpl_series_file <- function(name, lister = .tpl_series_lister()) {
  if (!is.character(name) || length(name) != 1L || !grepl("^[^/]+/[^/]+$", name)) return(NULL)
  s <- if (is.function(lister)) tryCatch(lister(), error = function(e) NULL)
  if (!is.data.frame(s) || !nrow(s)) return(NULL)
  parts <- strsplit(name, "/", fixed = TRUE)[[1L]]
  path <- s$path[match(parts[[1L]], s$name)]
  if (!length(path) || is.na(path)) return(NULL)
  f <- file.path(path, "templates", paste0(parts[[2L]], ".yml"))
  if (file.exists(f)) f
}

# A template report's data rows: the template's `data:` rows and the
# report's own (its `data:` rows, a list of maps with an id), each its id and
# the dataset it reads (`from`; a "{param}" is the report's argument or the
# param's default, NA when neither says).  A data frame (id, from), no rows
# for a template with none.
.tpl_data_rows <- function(name, args = list(), params = list(), own = NULL,
                           file = .tpl_series_file(name)) {
  keep <- function(x) x
  tpl <- if (!is.null(file)) {
    y <- tryCatch(yaml::read_yaml(file, handlers = list(r = keep, series = keep, study = keep,
                                                        fixed = keep, unset = function(x) NULL)),
                  error = function(e) NULL)
    if (is.list(y$data) && is.null(names(y$data))) y$data
  }
  mine <- if (is.list(own) && length(own) && is.null(names(own))) own
  rows <- Filter(function(r) is.list(r) && length(r$id) == 1L && !is.na(r$id), c(tpl, mine))
  fill <- function(x) {
    x <- as.character(unlist(x))
    if (length(x) != 1L || is.na(x)) return(NA_character_)
    m <- regmatches(x, regexec("^[{]([A-Za-z][A-Za-z0-9_.]*)[}]$", x))[[1L]]
    if (length(m) == 2L) {
      v <- as.character(unlist(args[[m[[2L]]]] %||% params[[m[[2L]]]]$default))
      return(if (length(v) && !is.na(v[[1L]]) && nzchar(v[[1L]])) v[[1L]] else NA_character_)
    }
    x
  }
  out <- data.frame(id = vapply(rows, function(r) as.character(r$id), ""),
                    from = vapply(rows, function(r) fill(r$from), ""),
                    stringsAsFactors = FALSE)
  out[!duplicated(out$id), , drop = FALSE]
}

# The value a `!series K` default takes (canon 2026-11-19: the series'
# values.K in the study's copy of the series, else in its source) -- NULL
# for a param whose default is no series value, a template of no series,
# or a tflspec that cannot say.  `study`: the study spec (rv$p$spec$study,
# its folder in $dir)
.tpl_series_value <- function(study, name, prm) {
  key <- as.character(prm$series %||% "")[1L]
  if (is.na(key) || !nzchar(key) || !is.character(name) || !grepl("/", name, fixed = TRUE)) return(NULL)
  s <- sub("/.*$", "", name)
  dir <- if (is.list(study) && is.character(study$dir)) study$dir
  v <- tryCatch({
    x <- if (.series_copied(dir, s)) {
      tflspec::tfl_series_read(s, dir = .series_copy_root(dir))
    } else {
      tflspec::tfl_series_read(s)
    }
    x$values
  }, error = function(e) NULL)
  if (is.list(v)) v[[key]]
}

# The args with one argument set (NULL, or the default itself: taken away)
.tpl_set_arg <- function(args, prm, value) {
  args <- args %||% list()
  if (is.null(value) || (!isTRUE(prm$required) && identical(value, prm$default)) ||
      (!isTRUE(prm$required) && !is.null(prm$default) && !.is_unset(value) && !.is_unset(prm$default) &&
       identical(unname(as.vector(value)), unname(as.vector(prm$default))))) {
    args[[prm$name]] <- NULL
  } else {
    args[[prm$name]] <- value
  }
  # (in the template's order)
  args
}

# The text arguments that are a header over the columns: the soft warning on
# an edit (the value is kept).  tflspec reads {col} {col1} ... by plain text
# replacement (rtfreporter .plan_header_fill), so a typed header without one
# gives every column the same text.
.tpl_header_checked <- "post_header"

# Which warning a header text earns: the English text (for t()), or NULL.
# The braces are read in order -- a "}" with no "{" before it, or a "{" left
# open, does not match; "{{col}}" matches (and holds a {col}).
.tpl_header_problem <- function(value, name = "post_header") {
  if (!name %in% .tpl_header_checked) return(NULL)
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) return(NULL)
  ch <- strsplit(value, "", fixed = TRUE)[[1L]]
  depth <- 0L
  for (x in ch[ch %in% c("{", "}")]) {
    depth <- depth + if (identical(x, "{")) 1L else -1L
    if (depth < 0L) break
  }
  if (depth != 0L) return("The header's braces don't match: it prints as typed.")
  if (!grepl("[{]col[0-9]*[}]", value)) return("The header has no {col}: each category's column gets the same text.")
  NULL
}

# The required arguments still empty: those the card shows (a hidden one is
# not the report's argument: demog's `population` is its `population:` field)
.tpl_missing <- function(name, args, study = NULL) {
  prm <- .tpl_shown(.tpl_params(name))
  req <- names(prm)[vapply(prm, function(x) isTRUE(x$required), NA)]
  # (a required argument the study gives -- its treatment for `by` -- is
  # not empty: tflspec fills it)
  given <- function(a) {
    v <- args[[a]] %||% if (!is.null(study)) .tpl_study_value(study, prm[[a]])
    length(v) > 0L && any(nzchar(as.character(v)))
  }
  req[!vapply(req, given, NA)]
}

# A template report's required values still empty, as its card says them:
# .tpl_missing()'s, and "population" first when the template takes the
# report's population (demog) and the report has none (character(): none,
# or not a template report).  With none of those, the required arguments
# tflspec finds empty (.tpl_rule_missing(): demog's `continuous` or
# `categorical`), with that error as the attribute `rule`.
.tpl_missing_report <- function(p, output_id) {
  tp <- .tpl_of(p, output_id)
  if (is.null(tp)) return(character())
  miss <- .tpl_missing(tp$name, tp$args, p$spec$study)
  if (.tpl_pop_of_report(.tpl_params(tp$name))) {
    pid <- as.character(p$spec$outputs[[output_id]]$common$population %||% "")[1L]
    if (is.na(pid) || !nzchar(pid)) miss <- c("population", miss)
  }
  if (!length(miss)) {
    rule <- .tpl_rule_missing(p, output_id)
    if (!is.null(rule)) miss <- structure(rule$args, rule = rule)
  }
  miss
}

# The required arguments tflspec finds empty when it resolves a template
# report: a rule the card's own marks do not hold (demog: `continuous` or
# `categorical`).  Read from tflspec's condition
# (tflspec_template_args_missing: its template, arguments and whether one
# of them is enough), never from its message.  NULL: none, or another
# error (said where the report is run).  The last answer is kept per
# report: the preview asks at each edit.
.tpl_rule_cache <- new.env(parent = emptyenv())
.tpl_rule_missing <- function(p, output_id) {
  o <- p$spec$outputs[[output_id]]
  if (is.null(o)) return(NULL)
  key <- list(o, p$spec$study)
  hit <- .tpl_rule_cache[[output_id]]
  if (!is.null(hit) && identical(hit$key, key)) return(hit$value)
  # (only its error is read: what else it says is the run's to say)
  value <- tryCatch({
    suppressWarnings(suppressMessages(tflspec::tfl_resolve_output(o, p$spec$study)))
    NULL
  }, tflspec_template_args_missing = .tpl_args_missing_of, error = function(e) NULL)
  assign(output_id, list(key = key, value = value), envir = .tpl_rule_cache)
  value
}

# tflspec's required-argument error as data
.tpl_args_missing_of <- function(e) {
  list(template = as.character(e$template)[1L], args = as.character(e$args),
       any = isTRUE(e$any))
}

# An argument's heading, as its card shows it: "meaning (name)" (the
# meaning from .tpl_arg_labels, or .tpl_doc_labels by its doc; the doc
# itself when it has neither; the name alone when it has no doc)
.tpl_arg_heading <- function(prm, t = identity) {
  doc <- prm$doc %||% ""
  # (the report's population a template hides (demog): the tip the other
  # templates' population has, not tflspec's note that it is no parameter)
  if (identical(prm$name, "population") && isTRUE(prm$hidden)) doc <- "the analysis set's id (SAF)"
  short <- if (doc %in% names(.tpl_doc_labels)) .tpl_doc_labels[[doc]] else .tpl_arg_labels[prm$name]
  head <- if (!is.na(short)) t(short) else if (nzchar(doc)) t(doc) else ""
  if (nzchar(head)) sprintf(t("%s (%s)"), head, prm$name) else prm$name
}

# An argument's help as its (i) shows it: in the Japanese UI the Japanese
# tflspec gives (`help: {en, ja}`), else the doc through the app's texts;
# "" for none
.tpl_doc_shown <- function(prm, t = identity, lang = "en") {
  if (identical(lang, "ja") && length(prm$doc_ja) && nzchar(prm$doc_ja[1L])) return(prm$doc_ja[1L])
  doc <- prm$doc %||% ""
  if (nzchar(doc)) t(doc) else ""
}

# The required arguments to fill in, as the app says it (#752): each by its
# card's heading, joined by "or" when one of them is enough, and the step
# it is filled in -- "Fill in Continuous variables (continuous) or
# Categorical variables (categorical) in 2 analysis."
.tpl_args_missing_text <- function(template, args, any = FALSE, t = identity) {
  prm <- .tpl_params(template)
  heads <- vapply(args, function(a) .tpl_arg_heading(prm[[a]] %||% list(name = a), t), "")
  secs <- vapply(args, function(a) {
    if (identical(a, "population") && .tpl_pop_of_report(prm)) "data" else .tpl_arg_section(template, a)
  }, "")
  join <- if (isTRUE(any)) t("%s or %s") else t("%s and %s")
  what <- Reduce(function(x, y) sprintf(join, x, y), unname(heads))
  steps <- paste(unique(vapply(secs, function(s) t(.step_labels[[s]]), "")), collapse = ", ")
  sprintf(t("Fill in %s in %s."), what, steps)
}

# What a template report's empty required arguments say where its preview
# waits for them: tflspec's rule as .tpl_args_missing_text() says it, else
# the names
.tpl_missing_say <- function(miss, t = identity) {
  rule <- attr(miss, "rule")
  if (!is.null(rule)) return(.tpl_args_missing_text(rule$template, rule$args, rule$any, t))
  sprintf(t("Required arguments are empty: %s. The preview waits for them."),
          paste(miss, collapse = ", "))
}

# The note a required-argument error is shown in: one, replaced by the next
# (a save and an Update preview say it once); NULL for any other error
.tpl_args_note <- "tpl_args_missing"
.error_note_id <- function(e) {
  if (inherits(e, "tflspec_template_args_missing")) .tpl_args_note
}

# An error's text on screen: tflspec's required-argument error as
# .tpl_args_missing_text() says it, any other as .error_view() does
.error_text <- function(e, t = identity) {
  if (inherits(e, "tflspec_template_args_missing")) {
    r <- .tpl_args_missing_of(e)
    return(.tpl_args_missing_text(r$template, r$args, r$any, t))
  }
  .error_view(conditionMessage(e), t)
}

# The editor step a template's argument is drawn in (#514): "data" (the
# analysis data: which records), "ard" (what is computed), "content" (how it
# is shown) or "page" (the page's titles).  A tflplanner-side lookup, not a
# part of the template: the table inst/extdata/template_arg_sections.csv
# (the shipped templates' arguments), else the argument's name (the
# company's templates, variants, a template not in the table), else "ard".
# Where an argument is drawn only; it is saved as it was, in the report's
# `template: {name, args}`.
.tpl_sections <- c("data", "ard", "content", "page")
.tpl_section_table <- local({
  tab <- NULL
  function() {
    if (is.null(tab)) {
      f <- system.file("extdata", "template_arg_sections.csv", package = "tflplanner")
      tab <<- if (nzchar(f)) {
        utils::read.csv(f, stringsAsFactors = FALSE, na.strings = character())
      } else data.frame(template = character(), argument = character(), section = character())
    }
    tab
  }
})
.tpl_arg_section <- function(template, argument) {
  # (a series template's def: its step)
  st <- .arg_def(.tpl_defs(template), argument)$step
  if (length(st) == 1L && !is.na(st) && st %in% names(.tpl_step_section)) {
    return(.tpl_step_section[[st]])
  }
  # TODO(P1a-2): drop when series templates exist (and the table
  # inst/extdata/template_arg_sections.csv with it)
  tab <- .tpl_section_table()
  i <- which(tab$template == template & tab$argument == argument)
  if (length(i)) return(tab$section[i[1L]])
  if (argument %in% c("population", "subset", "where", "data", "data_id", "param", "paramcd")) return("data")
  if (argument %in% c("title", "page_by", "page_heading")) return("page")
  if (argument %in% c("labels", "heading", "order", "sort", "formats") ||
        grepl("_(label|order)$", argument)) return("content")
  "ard"
}

# A template's parameters the card shows: not those it takes from the
# study standard (`hidden`; a KM type's data stage, its look)
.tpl_shown <- function(prm) Filter(function(x) !isTRUE(x$hidden), prm)

# Whether the report says the template's data in its own `data:` steps
# (the template hides its data stage: km_survival, km_cumulative)
.tpl_own_data <- function(prm) {
  any(vapply(prm, function(x) isTRUE(x$hidden) && identical(x$step, "data"), NA))
}

# Whether the template's analysis set is the report's `population:` field
# alone: its `population` hidden, required, and not part of the report's own
# data steps (demog, stf_t02_demog).  The add-report dialog asks for it, and
# writes it as the report's population, not as an argument.
.tpl_pop_of_report <- function(prm) {
  p <- prm[["population"]]
  !is.null(p) && isTRUE(p$hidden) && isTRUE(p$required) && !.tpl_own_data(prm)
}

# Whether the add-report dialog asks the report's analysis set for a
# template: every one does (#868, decided by coordinator 11-26 night): the
# one control "This report's population", the study's populations only,
# whatever the template says of its `population` -- hidden (demog, a KM
# type), shown (forest_hr, tflspec_hierarchy) or not at all (tflspec_v1's
# figures).  A `population` argument is never drawn as a field of its own
# (.tpl_dialog_shown()); where the set is written: .tpl_pop_is_arg()
.tpl_asks_report_pop <- function(prm) TRUE

# The template's parameters the add-report dialog draws as fields: the
# shown ones but `population`, which is the report's set control
.tpl_dialog_shown <- function(prm) {
  s <- .tpl_shown(prm)
  s[!vapply(s, function(x) identical(x$name, "population"), NA)]
}

# The `population` parameter the set control stands for; a template with
# none (tflspec_v1's figures) gets a bare one: optional, no default
.tpl_pop_param <- function(prm) {
  x <- prm[["population"]]
  if (is.null(x)) list(name = "population", required = FALSE) else x
}

# Whether the template takes its set as a shown argument (forest_hr,
# tflspec_hierarchy): the dialog's set control then also writes that
# argument, as the field of it did; a hidden one, or none, is the report's
# set alone
.tpl_pop_is_arg <- function(prm) {
  p <- prm[["population"]]
  !is.null(p) && !isTRUE(p$hidden)
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
  # (the report's set written where its file has it -- a KM type's data map
  # kept by a switch to the layout `km`, else `common:` -- never a second
  # value beside it, .o_set_population(); #868)
  if (!is.null(args$population)) o <- .o_set_population(o, as.character(args$population))
  # (a template whose analysis set is the report's population (demog) holds
  # it in `common:`, not in its arguments)
  if (.tpl_pop_of_report(prm)) args$population <- NULL
  o$template <- .tpl_write_args(o$template, args)
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
# A template report is made in the four steps of a code SPEC report (data,
# analysis, content, page: .step_labels); each step's card has the
# arguments whose `step:` is that step, and the template's own card is
# above the steps.  The server says at once whether the report is a
# template report when it changes (rp-tpl-mode, a custom message: sent
# before the flush that draws the report), and the page shows "Loading ..."
# over the steps until that flush is done (rp-report-ready): the report
# before it, or the code SPEC's builder, is never seen in its place.
# `.rp-tpl-only` / `.rp-code-only` are shown by the mode.
# (a step's word in its card's heading: the step's name without its number)
.tpl_step_word <- c(data = "data", ard = "analysis", content = "content", page = "page")

.tpl_css <- "
body:not(.rp-tpl) .rp-tpl-only, body.rp-tpl .rp-code-only { display: none !important; }
.rp-report-loading { display: none; }
body.rp-adding::before { content: attr(data-loading); position: fixed; inset: 0; z-index: 1040;
  background: var(--bs-body-bg, #fff); color: #6c757d; padding: 6rem 2rem; font-size: .95rem; }

body.rp-switching .rp-report-loading { display: block; }
body.rp-switching #step ~ .tab-content, body.rp-switching #report_head, #report_head.rp-stale,
body.rp-switching #tpl_head {
  visibility: hidden; }
/* an argument at its default reads like any other: its tag says so, no fading;
   only a choice left empty ('(default)', '(not chosen)': no value) stays light */
.selectize-input .item[data-value=''] {
  color: #8a8f98; }
.rp-tpl-arg-from { font-size: .75rem; color: #6b7280; }
.rp-tpl-badge { font-size: .7rem; font-weight: 500; vertical-align: middle; }
.rp-tpl-badge-own { background-color: #0d6efd; color: #fff; }
.rp-tpl-badge-study { background-color: #198754; color: #fff; }
.rp-tpl-badge-fallback { background-color: #ffc107; color: #000; }
.rp-tpl-badge-default { background-color: transparent; color: #495057; border: 1px solid #adb5bd; }
.rp-tpl-badge-key { background-color: #e9ecef; color: #495057; border: none; }
.rp-code pre.rp-flow { max-height: none !important; overflow-y: visible; min-height: 0; }
@keyframes rp-card-flash { 0%, 60% { outline-color: rgba(13,110,253,.85); }
  100% { outline-color: rgba(13,110,253,0); } }
.rp-card.rp-card-flash { outline: 3px solid rgba(13,110,253,0); outline-offset: 2px;
  animation: rp-card-flash 1.8s ease-out 1; }
.rp-add-menu .dropdown-menu { max-height: 60vh; overflow-y: auto; }
/* the analysis step of a template report: its card only (the code SPEC's
   definition, its own R program and Update preview are a code report's) */
body.rp-tpl #step_ard > :not(.rp-tpl-only) { display: none !important; }
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
  // (a card the server says is as drawn: left as it is, with the focus in it --
  // a template's, or the analysis form after its own edit was applied)
  $(document).on('shiny:value', function(e) {
    if (/^(tpl_card|ard_stat_ui$)/.test(e.name) && e.value && /rp-keep/.test(e.value.html || '')) e.preventDefault();
  });
  document.body.setAttribute('data-loading', $('.rp-report-loading').first().text() || 'Loading ...');
  // the two ARD tabs of a template report: greyed out and not selectable
  // when it makes no ARD (a click, the keys, do nothing); one selected gives
  // way to the report's Result, so no pane is left empty
  function rpArdTabs(on, tip) {
    $('body').toggleClass('rp-ard', !!on);
    // (a table's, a figure's and a listing's tabs: the one of the report shown is the one that counts)
    var tabs = $('#table_right, #fig_right, #lf_right');
    tabs.find('a[data-value^=\"ard\"]').each(function() {
      $(this).toggleClass('disabled', !on).attr('aria-disabled', on ? null : 'true')
        .attr('tabindex', on ? null : '-1');
      $(this).parent().attr('title', on ? null : (tip || ''));
    });
    if (!on) tabs.each(function() {
      if ($(this).find('a.active[data-value^=\"ard\"]').length) {
        $(this).find('a[data-value=\"result\"]').tab('show');
      }
    });
  }
  Shiny.addCustomMessageHandler('rp-ard-tabs', function(x) { rpArdTabs(x.on, x.tip); });
  Shiny.addCustomMessageHandler('rp-tpl-mode', function(x) {
    if (x.id) document.body.setAttribute('data-rp-id', x.id);
    headCheck();
    $('body').toggleClass('rp-tpl', !!x.tpl);
    // (the right pane's ARD tabs: a template report's; rp-ard-tabs says
    // whether it makes an ARD once the report is known)
    if (!x.tpl) rpArdTabs(false);
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

# One row per shown argument of a template, in its order, from tflspec's
# own account of where each value comes from (tfl_tpl_args(): type, layer,
# study_key, effective, fallback ...).  `study` the study's spec,
# `population` the report's.  NULL when tflspec cannot say (an older
# tflspec, a study spec it cannot read): the card then says it as before
# (.tpl_arg_origin()).
.tpl_args_rows <- function(name, study, population = NULL) {
  pop <- as.character(population %||% "")[1L]
  if (is.na(pop) || !nzchar(pop)) pop <- NULL
  r <- tryCatch(tflspec::tfl_tpl_args(name, study = study, population = pop),
                error = function(e) NULL)
  if (is.data.frame(r) && nrow(r) && all(c("param", "layer") %in% names(r))) r
}

# The badge of one argument: list(kind = "own" | "study" | "fallback" |
# "default", key = the study's key, for "study"), or NULL (no row: the old
# text).  The report's own value first (tfl_tpl_args() has no report
# arguments); then the layer tflspec names: "study: <key>" the study
# standard, "report" the report's analysis set (output.population, kept as
# "study" with no key), "fallback" the default for when the study standard
# is undefined, "default" with a study key a study reference with no value
# (kind "none": no badge, the field says "(none)"), the rest (template,
# default with no key, variant:<name>) the template's own.
.tpl_badge_of <- function(a, args, rows) {
  if (is.null(rows)) return(NULL)
  r <- rows[rows$param == a, , drop = FALSE]
  if (!nrow(r)) return(NULL)
  ly <- as.character(r$layer[1L])
  if (a %in% names(args)) {
    # (the report's value over a study's: the study's key and value kept for the note)
    if (startsWith(ly, "study:"))
      return(list(kind = "own", key = as.character(r$study_key[1L]), study = r$effective[[1L]]))
    return(list(kind = "own", key = NA_character_))
  }
  if (startsWith(ly, "study:")) return(list(kind = "study", key = as.character(r$study_key[1L])))
  if (identical(ly, "report")) return(list(kind = "report", key = NA_character_))
  if (identical(ly, "fallback")) return(list(kind = "fallback", key = as.character(r$study_key[1L])))
  if (identical(ly, "default")) {
    sk <- if ("study_key" %in% names(r)) as.character(r$study_key[1L]) else NA_character_
    if (length(sk) && !is.na(sk) && nzchar(sk)) return(list(kind = "none", key = sk))
  }
  list(kind = "default", key = NA_character_)
}

# The study key of a field that is a study reference with no value (kind
# "none"), else NULL: its placeholder says "(none)" and its (i) names the key
.tpl_none_key <- function(a, args, rows) {
  b <- .tpl_badge_of(a, args, rows)
  if (!is.null(b) && identical(b$kind, "none")) b$key
}

# The (i) text of a field: the doc, and for a study reference with no value
# (`key`) the sentence that says where it would come from
.tpl_info_text <- function(doc, key, t) {
  if (is.null(key) || is.na(key) || !nzchar(key)) return(doc)
  paste(c(if (nzchar(doc)) t(doc), sprintf(t("Uses the study standard's %s when it is defined."), key)),
        collapse = " ")
}

# The badge itself, a small pill after the label; `t` the translator,
# `kind` the report's kind ("table": "This table", else "This report")
.tpl_badge_tag <- function(b, t, kind = "table") {
  if (identical(b$kind, "none")) return(NULL)
  txt <- switch(b$kind,
    own = t(if (identical(kind, "table")) "This table" else "This report"),
    study = sprintf(t("Study standard (%s)"), b$key),
    report = t("This report's analysis set"),
    fallback = if (!length(b$key) || is.na(b$key[1L])) t("Default when the study standard is undefined") else
      sprintf(t("Default when the study standard (%s) is undefined"), b$key),
    t("Template default"))
  cls <- c(own = "own", study = "study", report = "study", fallback = "fallback")[b$kind]
  shiny::span(class = paste0("badge rounded-pill rp-tpl-badge rp-tpl-badge-",
                             if (is.na(cls)) "default" else cls), txt)
}

# The value a report's card draws for an argument (`prm`, its parameter;
# `args`, the report's): its own, else the study's as the card says it
# (.tpl_arg_origin(): what tflspec filled from the study first), else the
# template's default.  A field's value coming back as drawn is no edit: a
# value of the study standard is never written into the report's args by
# the drawing alone (#751 item 2: it would stop following the standard).
.tpl_drawn_value <- function(p, output_id, args, prm) {
  if (prm$name %in% names(args)) return(args[[prm$name]])
  sv <- .tpl_study_value(p$spec$study, prm)
  if (is.null(sv)) {
    og <- .tpl_arg_origin(p$spec$study, prm, args, .tpl_filled(p, output_id))
    if (identical(og$from, "study")) sv <- og$study
  }
  sv %||% prm$default
}

# A template report as tflspec resolves it (tfl_resolve_output(); NULL when
# it cannot be).  The last report's kept while it, the study and the
# study's copies of the templates are the same (a card's fields each ask it
# as they come back: .tpl_drawn_value()).
.tpl_resolved_cache <- new.env(parent = emptyenv())
.tpl_resolved <- function(p, output_id) {
  o <- p$spec$outputs[[output_id]]
  if (is.null(o)) return(NULL)
  k <- .tpl_resolved_cache
  # (the study's copies of the templates read too: a copy again is a change)
  dir <- p$spec$study$dir
  cp <- if (is.character(dir) && length(dir) == 1L) {
    list.files(.series_copy_root(dir), recursive = TRUE, full.names = TRUE)
  }
  key <- list(o, p$spec$study, cp, file.mtime(cp))
  if (isTRUE(k$has) && identical(k$key, key)) return(k$out)
  out <- tryCatch(tflspec::tfl_resolve_output(o, p$spec$study), error = function(e) NULL)
  k$key <- key
  k$out <- out
  k$has <- TRUE
  out
}

# What tflspec filled in from the study for a template report: the
# template attribute of its resolved output (NULL: none, or an older
# tflspec)
.tpl_filled <- function(p, output_id) {
  a <- attr(.tpl_resolved(p, output_id), "template")
  if (is.null(a$study)) NULL else a
}

# A template report's analysis set, for its data step (#751 item 6): the
# report's own (`common: population`) and the analysis data the template
# makes with the population each has as tflspec resolves it (a template
# row's own, else the report's) -- a data frame id, from, population; NULL
# rows when it cannot be resolved
.tpl_pop_view <- function(p, output_id) {
  pid <- as.character(p$spec$outputs[[output_id]]$common$population %||% NA_character_)[1L]
  ad <- .tpl_resolved(p, output_id)$ard$analysis_data
  rows <- if (length(ad)) data.frame(
    id = vapply(ad, function(x) as.character(x$id %||% NA_character_)[1L], ""),
    from = vapply(ad, function(x) as.character(x$from %||% NA_character_)[1L], ""),
    population = vapply(ad, function(x) as.character(x$population %||% pid)[1L], ""),
    stringsAsFactors = FALSE)
  list(population = pid, rows = rows)
}

# .tpl_pop_view() drawn in the data step: the report's analysis set and the
# analysis data the template makes, each with its analysis set
.tpl_pop_view_ui <- function(pv, t = identity) {
  pop <- if (is.na(pv$population) || !nzchar(pv$population)) t("(none)") else pv$population
  shiny::div(
    class = "small mb-2",
    shiny::div(class = "mb-1",
               with_tip(shiny::span(shiny::strong(t("Analysis set (population)")), ": ", shiny::code(pop)),
                        t("The report's analysis set (common: population), set on the report list. The template's analysis data read it unless their own row names one."))),
    if (NROW(pv$rows)) shiny::tags$table(
      class = "table table-sm small mb-0",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(t("Analysis data (id)")), shiny::tags$th(t("Dataset (from)")),
        shiny::tags$th(t("Analysis set (population)")))),
      shiny::tags$tbody(lapply(seq_len(nrow(pv$rows)), function(i) shiny::tags$tr(
        shiny::tags$td(shiny::code(pv$rows$id[i])), shiny::tags$td(pv$rows$from[i]),
        shiny::tags$td(if (is.na(pv$rows$population[i])) t("(none)") else pv$rows$population[i]))))))
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

# The verbs of the study standard's items a template report has, and of any
# the template adds after its call, as its program has them, in their order
# -- the cards shown with the report's own (one card an item, #440).  A
# template with `extends` (demog) has the study's items inside its call
# (`study = list(plan_x = ...)`, tflspec #780), the others as lines after it:
# the resolved output's template record (`$rest`, items not the report's own)
# says the first, the program's lines after the call the second.
.tpl_code_fns <- function(p, output_id) {
  sp <- tryCatch(.code_spec(p), error = function(e) NULL)
  o <- sp$outputs[[output_id]]
  if (is.null(o) || is.null(.tpl_spec_name(o))) return(character())
  res <- tryCatch(tflspec::tfl_resolve_output(o, sp$study), error = function(e) NULL)
  code <- tryCatch(tflspec::tfl_content_code(res), error = function(e) character())
  in_call <- vapply(attr(res, "template")$rest %||% list(), function(it) {
    if (is.list(it) && !identical(it$from, "own") && is.character(it$fn) && length(it$fn) == 1L) it$fn else NA_character_
  }, "")
  at <- grep("(tpl_[A-Za-z0-9_]+|tfl_tpl)\\(", code)
  after <- character()
  if (length(at)) {
    rest <- code[seq_along(code) > at[1L]]
    m <- regmatches(rest, regexpr("^\\s*(plan_[A-Za-z0-9_]+)\\(", rest))
    after <- sub("\\($", "", trimws(m))
  }
  # (an item said both ways counts once; a verb the lines repeat stays twice)
  in_call <- in_call[!is.na(in_call)]
  fns <- c(in_call[!in_call %in% after], after)
  # (the study's items inside the call: only the plan verbs, as the lines are)
  fns[grepl("^plan_", fns)]
}
.tpl_spec_name <- function(o) {
  t <- o$template
  if (is.list(t) && !is.null(t$name)) t$name else NULL
}

# The function that makes a template's report, as one name whatever the
# template (#751 item 7; the template's own name is in the card beside it):
# a built-in template's call in its program (tfl_tpl()); a series template
# (`<series>/<name>`), whose program is the code tflspec resolves it to,
# tfl_template_resolve()
.tpl_call_name <- function(name) {
  if (.tpl_is_series(name)) return("tfl_template_resolve()")
  x <- tryCatch(tflspec::tfl_templates(), error = function(e) NULL)
  call <- if (!is.null(x) && "call" %in% names(x)) x$call[match(name, x$name)] else NA
  paste0(if (length(call) && !is.na(call) && nzchar(call)) call else "tfl_tpl", "()")
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

# A value a field says that is not its current one but is one a write of it
# replaced within `window` seconds: a field drawn from an older card saying
# its old value back, not an edit.  `past`: data frame of v (the values) and
# t (their times, seconds); `val`, `now` likewise.  (A write's say-back came
# 2-6 s after it when measured; 15 s is the margin; the tests, which run on
# machines that take longer, widen it with option tflplanner.tpl_stale_window.)
.tpl_stale_say <- function(val, past, now,
                           window = getOption("tflplanner.tpl_stale_window",
                                              15)) {
  if (is.null(past) || !nrow(past)) return(FALSE)
  any(past$v == val & now - past$t <= window)
}
