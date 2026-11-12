# The four-steps report screen: what the screen reads (provisional).
#
# TEMPORARY ADAPTER.  The screen of a report in its target shape -- a
# `common:` block, then the four steps data / ard / content / page -- is built
# before the tflspec readers for `common:`, `data:` and `page:` exist (DP-2,
# PP-2).  This file reads those keys straight from the report's YAML file and
# resolves the page bands by hand.  It is to be REPLACED by what tflspec's
# readers give (the resolved record: each value with its route, the page
# configurations from tfl_styles(), the template's params with their `step`).
# Nothing else in tflplanner calls it; the screen is off unless
# options(tflplanner.foursteps = TRUE) or the environment variable
# TFLPLANNER_FOURSTEPS is set.

.fs_on <- function() {
  isTRUE(getOption("tflplanner.foursteps")) || nzchar(Sys.getenv("TFLPLANNER_FOURSTEPS"))
}

# the study folder the screen starts on: the option, then the variable
.fs_study_default <- function() {
  x <- getOption("tflplanner.foursteps.study") %||% Sys.getenv("TFLPLANNER_FOURSTEPS_STUDY")
  if (length(x) == 1L && !is.na(x) && nzchar(x)) x else ""
}

# a report file's YAML: `!r` code and `!unset` kept as classed values
.fs_yaml <- function(path) {
  yaml::read_yaml(path, handlers = list(
    r = function(x) structure(list(code = as.character(x)), class = "fs_r"),
    unset = function(x) structure(list(), class = "fs_unset")))
}

.fs_is_unset <- function(x) inherits(x, "fs_unset")

# a value as plain data (no classed objects), for display
.fs_plain <- function(v) {
  if (inherits(v, "fs_r")) return(paste0("!r ", v$code))
  if (inherits(v, "fs_unset")) return("!unset")
  if (is.list(v)) return(lapply(v, .fs_plain))
  v
}

# a value as one line of text
.fs_show <- function(v) {
  if (is.null(v)) return("")
  if (inherits(v, "fs_r")) return(v$code)
  if (inherits(v, "fs_unset")) return("")
  if (is.atomic(v)) return(paste(as.character(v), collapse = ", "))
  v <- .fs_plain(v)
  if (is.null(names(v)) && all(vapply(v, function(e) is.atomic(e) && length(e) == 1L, NA))) {
    return(paste(unlist(v), collapse = ", "))
  }
  x <- trimws(yaml::as.yaml(v))
  gsub("\n+", "; ", x)
}

# ---------------------------------------------------------------- the study

# the study's files the screen reads: study.yml, spec/data.yml (populations),
# spec/defaults.yml (styles:); a missing one is empty
.fs_study <- function(root) {
  rd <- function(f) {
    f <- file.path(root, f)
    if (file.exists(f)) tryCatch(.fs_yaml(f), error = function(e) list()) else list()
  }
  d <- rd(file.path("spec", "data.yml"))
  pops <- d$populations %||% list()
  list(root = root,
       settings = rd("study.yml")$settings %||% list(),
       tokens = rd("study.yml")$tokens %||% list(),
       populations = pops,
       defaults = rd(file.path("spec", "defaults.yml")))
}

.fs_population <- function(study, id) {
  for (p in study$populations) if (identical(as.character(p$id), as.character(id))) return(p)
  NULL
}

# the study's report files, id -> path (spec/outputs/*.yml)
.fs_reports <- function(root) {
  d <- file.path(root, "spec", "outputs")
  f <- if (dir.exists(d)) sort(list.files(d, "[.]ya?ml$", full.names = TRUE)) else character()
  stats::setNames(f, sub("[.]ya?ml$", "", basename(f)))
}

# ----------------------------------------------- the page configurations
# (provisional: the built-ins tfl_standard and tfl_small are written here;
# tflspec ships them later -- PAGE-PART 2.1b)

.fs_header_lines <- function() {
  list(list(left = "{COMPANY}"),
       list(left = "Protocol: {STUDY_ID}", right = "Page {PAGE} of {TOTAL_PAGES}"),
       list(center = "{OUTPUT_LABEL}"),
       list(center = "{OUTPUT_TITLE}"),
       list(center = "<{OUTPUT_POPULATION}>"))
}

.fs_builtin_configs <- function() {
  mk <- function(name, margin, size, rows) {
    list(name = name, source = "built-in",
         settings = list(rtf_page = list(orientation = "landscape", margin_left_in = margin,
                                         margin_right_in = margin),
                         rtf_style = list(font_size_half_points = size)),
         max_rows = rows,
         bands = list(header = list(first = .fs_header_lines(), last = list()),
                      footer = list(first = list(), last = list(list(left = "{PROGRAM}", right = "{DATETIME}"))),
                      footnotes = list(first = list(), last = list())))
  }
  list(tfl_standard = mk("tfl_standard", 1, 18L, 30L),
       tfl_small = mk("tfl_small", 0.5, 14L, 42L))
}

# a band of a configuration: plain lines are its first block, or a map
# {first:, last:}
.fs_cfg_band <- function(x) {
  if (is.null(x)) return(list(first = list(), last = list()))
  if (!is.null(names(x)) && all(names(x) %in% c("first", "last"))) {
    return(list(first = unname(x$first %||% list()), last = unname(x$last %||% list())))
  }
  list(first = unname(x), last = list())
}

# the configurations of a study: the built-ins, then the study's own
# (defaults.yml `styles:`), a study's set of a built-in name replacing it
.fs_configs <- function(study) {
  out <- .fs_builtin_configs()
  for (nm in names(study$defaults$styles)) {
    s <- study$defaults$styles[[nm]]
    pg <- s$page %||% list()
    rows <- NULL
    for (it in s$content) if (!is.null(it$plan_paginate_rows)) rows <- it$plan_paginate_rows$max_rows
    out[[nm]] <- list(
      name = nm, source = "study",
      settings = pg[intersect(names(pg), c("rtf_page", "rtf_style", "rtf_document", "rtf_tables"))],
      max_rows = rows,
      bands = list(header = .fs_cfg_band(pg$header), footer = .fs_cfg_band(pg$footer),
                   footnotes = .fs_cfg_band(pg$footnotes)))
  }
  out
}

# ------------------------------------------- what a template says (provisional)
# The template's own values, by step: written here for the two templates of the
# test study.  tflspec's template_params (with `step`) replaces this.

.fs_templates <- function() {
  list(
    demog = list(
      label = "Demographics",
      style = NULL,
      data = list(dataset = list(value = "ADSL"),
                  subset = list(value = NULL),
                  denominator = list(value = "adsl_sub", fixed = TRUE)),
      ard = list(),
      content = list(split = list(value = "group_safe", fixed = TRUE),
                     max_rows = list(config = TRUE),
                     page_by = list(value = NULL), page_heading = list(value = NULL)),
      page = list(titles = NULL, header = NULL, footer = NULL,
                  footnotes = c("Percentages are based on the number of subjects in the analysis set."))),
    ae_soc_pt = list(
      label = "Adverse events by SOC and PT",
      style = "tfl_standard",
      data = list(dataset = list(value = "ADAE"),
                  subset = list(value = NULL),
                  where = list(value = "TRTEMFL == \"Y\""),
                  denominator = list(value = "adsl_sub")),
      ard = list(hierarchy = list(value = c("AEBODSYS", "AEDECOD")),
                 total = list(value = FALSE)),
      content = list(split = list(value = "group_force"),
                     max_rows = list(config = TRUE),
                     page_by = list(value = NULL), page_heading = list(value = NULL)),
      page = list(titles = NULL, header = NULL, footer = NULL,
                  footnotes = c("Subjects are counted once within each SOC and PT.",
                                "MedDRA version {MEDDRA_VERSION}."))))
}

# the English label of a parameter, "meaning (name)"; a name with no label
# stands as it is
.fs_labels <- c(
  dataset = "Dataset (dataset)", subset = "Subjects kept (subset)",
  where = "Records kept (where)", denominator = "Denominator dataset (denominator)",
  hierarchy = "Hierarchy (hierarchy)", continuous = "Continuous variables (continuous)",
  categorical = "Categorical variables (categorical)", total = "Total column (total)",
  min_pct = "Smallest percent shown (min_pct)",
  split = "Page breaks (split)", max_rows = "Rows a page (max_rows)",
  page_by = "Variable that starts a page (page_by)",
  page_heading = "Heading of each page (page_heading)",
  labels = "Row labels (labels)")

.fs_pagebreak <- c("split", "max_rows", "page_by", "page_heading")
.fs_bands <- c("titles", "header", "footer", "footnotes")
.fs_band_labels <- c(titles = "Titles", header = "Header", footer = "Footer", footnotes = "Footnotes")

# --------------------------------------------------------- the page bands

# lines (a vector of text, a list of lines, a map by number) as numbered
# lines: list of list(num, text, raw)
.fs_line_text <- function(x) {
  if (is.null(x) || (is.list(x) && !length(x))) return("")
  if (is.list(x) && any(c("left", "center", "right") %in% names(x))) {
    p <- c(left = x$left, center = x$center, right = x$right)
    return(paste(vapply(names(p), function(n) paste0(n, ": ", p[[n]]), ""), collapse = "  |  "))
  }
  paste(as.character(unlist(x)), collapse = " ")
}

# a report's value of a band as a body: kind "off" (!unset), "none" (NULL),
# "list" (the full list) or "map" (by number); lines are list(num, line)
.fs_body <- function(x) {
  if (.fs_is_unset(x)) return(list(kind = "off", lines = list()))
  if (is.null(x)) return(list(kind = "none", lines = list()))
  named <- !is.null(names(x)) && !any(c("left", "center", "right") %in% names(x))
  if (is.atomic(x) && length(x) > 0L) x <- as.list(x)
  if (!is.list(x) || !length(x)) return(list(kind = "none", lines = list()))
  if (named) {
    return(list(kind = "map", lines = lapply(seq_along(x), function(i)
      list(num = as.integer(names(x)[i]), line = x[[i]]))))
  }
  if (!is.null(names(x))) x <- list(x)
  list(kind = "list", lines = lapply(seq_along(x), function(i) list(num = i, line = x[[i]])))
}

# One band, resolved: the configuration's first lines, the body (the
# template's default lines changed by the report's, ordered by number), the
# configuration's last lines.  Each item: src (config / template / own),
# num (body lines), text.
.fs_resolve_band <- function(cfg_band, tdefault, rep) {
  bd <- .fs_body(rep)
  if (identical(bd$kind, "off")) return(list(off = TRUE, items = list()))
  body <- list()
  for (l in .fs_body(tdefault)$lines) body[[as.character(l$num)]] <- list(src = "template", num = l$num, line = l$line)
  if (identical(bd$kind, "list")) body <- list()
  for (l in bd$lines) {
    k <- as.character(l$num)
    if (.fs_is_unset(l$line)) {
      # a template line taken out: kept in its place, marked removed
      body[[k]] <- if (is.null(body[[k]])) NULL else
        list(src = "removed", num = l$num, line = body[[k]]$line)
    } else body[[k]] <- list(src = "own", num = l$num, line = l$line)
  }
  body <- body[order(vapply(body, function(b) b$num, 0L))]
  cf <- function(ls) lapply(ls, function(l) list(src = "config", num = NA_integer_, line = l))
  items <- c(cf(cfg_band$first), unname(body), cf(cfg_band$last))
  items <- lapply(items, function(i) { i$text <- .fs_line_text(i$line); i })
  list(off = FALSE, items = items, own = any(vapply(body, function(b) b$src %in% c("own", "removed"), NA)))
}

# ----------------------------------------------------------- the model
# One report, ready to draw (see R/foursteps_ui.R).  A row: name, label,
# value (text), origin (own / default / fixed / config / study / none), note.

.fs_row <- function(name, value, origin, config = NULL, extra = list()) {
  c(list(name = name, value = value, origin = origin, config = config), extra)
}

# a step's rows of a template report: what the report writes in `template:
# <step>:`, then the template's own values the report does not write
.fs_template_rows <- function(tpl, step, given, cfg) {
  given <- given %||% list()
  rows <- list()
  for (n in names(given)) {
    rows[[n]] <- .fs_row(n, .fs_show(given[[n]]), "own")
  }
  for (n in names(tpl[[step]])) {
    if (n %in% names(given)) next
    d <- tpl[[step]][[n]]
    if (isTRUE(d$config)) {
      rows[[n]] <- .fs_row(n, if (is.null(cfg$max_rows)) "" else as.character(cfg$max_rows), "config", cfg$name)
    } else {
      rows[[n]] <- .fs_row(n, .fs_show(d$value), if (isTRUE(d$fixed)) "fixed" else "default")
    }
  }
  unname(rows)
}

# the datasets a template report builds, from its data-step values
.fs_template_datasets <- function(rows, common, study) {
  v <- function(n, d = "") {
    for (r in rows) if (identical(r$name, n)) return(r$value)
    d
  }
  pop <- .fs_population(study, common$population)
  out <- list(list(id = "adsl_sub", role = "analysis set",
                   text = paste0(common$population %||% "", if (!is.null(pop$label)) paste0(" (", pop$label, ")") else ""),
                   subset = v("subset")))
  ds <- v("dataset")
  if (nzchar(ds) && !identical(toupper(ds), "ADSL")) {
    out[[2L]] <- list(id = paste0(tolower(ds), "_sub"), role = "records",
                      text = paste0(ds, " of the subjects of adsl_sub"), subset = v("where"))
  }
  den <- v("denominator", "adsl_sub")
  if (grepl("_den$", den)) {
    out[[length(out) + 1L]] <- list(id = den, role = "denominator",
                                    text = paste0("the subjects tested: one row a subject of ", sub("_den$", "_sub", den)), subset = "")
  }
  out
}

# the definitions of a code SPEC report's data:, with the implicit adsl_sub
# first when the report does not write it
.fs_code_datasets <- function(data, common, study) {
  defs <- if (is.list(data) && is.null(names(data))) data else list()
  ids <- vapply(defs, function(d) as.character(d$id %||% ""), "")
  out <- list()
  if (!"adsl_sub" %in% ids) {
    pop <- .fs_population(study, common$population)
    out[[1L]] <- list(id = "adsl_sub", role = "analysis set", implicit = TRUE,
                      text = paste0("made from the common block: ", common$population %||% "",
                                    if (!is.null(pop$label)) paste0(" (", pop$label, ")") else ""),
                      fields = list())
  }
  for (d in defs) {
    id <- as.character(d$id %||% "")
    role <- if (grepl("_den$", id)) "denominator" else if (is.null(d$subjects)) "analysis set" else "records"
    out[[length(out) + 1L]] <- list(id = id, role = role, implicit = FALSE, text = "",
                                    fields = d[setdiff(names(d), "id")])
  }
  out
}

# The report as the screen shows it.  `study` is .fs_study(); `path` the
# report's YAML file.
.fs_model <- function(path, study) {
  y <- .fs_yaml(path)
  cfgs <- .fs_configs(study)
  tname <- y$template$name
  is_tpl <- !is.null(tname)
  tpl <- if (is_tpl) .fs_templates()[[tname]] %||% list() else list()
  common <- y$common %||% list()
  pop <- .fs_population(study, common$population)
  # the page configuration: the report's, the template's, the study's, tfl_standard
  sty <- as.character(y$style %||% "")[1L]
  chosen_by <- "own"
  if (!nzchar(sty)) { sty <- as.character(tpl$style %||% ""); chosen_by <- "template" }
  if (!nzchar(sty)) { sty <- as.character(study$settings$style %||% ""); chosen_by <- "study" }
  if (!nzchar(sty)) { sty <- "tfl_standard"; chosen_by <- "built-in" }
  cfg <- cfgs[[sty]] %||% cfgs$tfl_standard
  m <- list(
    id = as.character(y$id %||% tools::file_path_sans_ext(basename(path))),
    kind = as.character(y$kind %||% "table"), label = as.character(y$label %||% ""),
    template = if (is_tpl) tname else NULL,
    common = list(
      population = list(value = as.character(common$population %||% ""), origin = if (is.null(common$population)) "none" else "own",
                        options = vapply(study$populations, function(p) as.character(p$id), ""),
                        labels = vapply(study$populations, function(p) as.character(p$label %||% ""), "")),
      treatment = list(value = as.character(common$treatment %||% ""),
                       origin = if (is.null(common$treatment)) "none" else "own"),
      population_label = list(
        value = as.character(common$population_label %||% pop$label %||% ""),
        origin = if (!is.null(common$population_label)) "own" else if (!is.null(pop$label)) "study" else "none")),
    configs = lapply(cfgs, function(c) list(name = c$name, source = c$source)),
    style = list(name = sty, chosen_by = chosen_by),
    steps = list())
  tp <- y$template %||% list()
  if (is_tpl) {
    drows <- .fs_template_rows(tpl, "data", tp$data, cfg)
    m$steps$data <- list(rows = drows, datasets = .fs_template_datasets(drows, common, study))
    m$steps$ard <- list(rows = .fs_template_rows(tpl, "ard", tp$ard, cfg))
    crows <- .fs_template_rows(tpl, "content", tp$content, cfg)
    m$steps$content <- list(
      pagebreak = Filter(function(r) r$name %in% .fs_pagebreak, crows),
      rows = Filter(function(r) !r$name %in% .fs_pagebreak, crows))
  } else {
    m$steps$data <- list(defs = .fs_code_datasets(y$data, common, study))
    m$steps$ard <- list(analyses = unname(y$ard$analyses %||% list()))
    m$steps$content <- list(verbs = unname(y$content %||% list()))
  }
  m$steps$data$ids <- vapply(
    if (is_tpl) m$steps$data$datasets else m$steps$data$defs, function(d) d$id, "")
  m$steps$data$id_roles <- vapply(
    if (is_tpl) m$steps$data$datasets else m$steps$data$defs, function(d) d$role, "")
  # step 4: the bands, the settings, the tokens
  rep_page <- y$page %||% list()
  tpage <- tp$page %||% list()
  bands <- list()
  for (b in .fs_bands) {
    cb <- cfg$bands[[b]] %||% list(first = list(), last = list())
    rep <- if (is_tpl) tpage[[b]] else rep_page[[b]]
    bands[[b]] <- .fs_resolve_band(cb, if (is_tpl) tpl$page[[b]] else NULL, rep)
  }
  settings <- list()
  for (k in union(names(cfg$settings), setdiff(names(rep_page), c(.fs_bands, "tokens")))) {
    for (a in union(names(cfg$settings[[k]]), names(rep_page[[k]]))) {
      own <- !is.null(rep_page[[k]][[a]])
      settings[[length(settings) + 1L]] <- list(
        name = paste0(k, ": ", a), value = .fs_show(if (own) rep_page[[k]][[a]] else cfg$settings[[k]][[a]]),
        origin = if (own) "own" else "config", config = cfg$name)
    }
  }
  tokens <- lapply(names(rep_page$tokens), function(n)
    list(name = n, value = .fs_show(rep_page$tokens[[n]]), origin = "own"))
  stok <- lapply(setdiff(names(study$tokens), names(rep_page$tokens)), function(n)
    list(name = n, value = .fs_show(study$tokens[[n]]), origin = "study"))
  m$steps$page <- list(bands = bands, settings = settings, tokens = c(tokens, stok), config = cfg$name)
  m
}
