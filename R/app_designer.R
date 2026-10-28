# The Plot Designer: a figure designed piece by piece, drawn as it is
# designed.
#
# A figure's design (tflspec's figure design) is four lists of pieces: the
# data steps from ADaM, the statistics, the figure-wide settings and the
# layers.  A template fills them at once (KM with the number at risk, mean
# over time, waterfall ...).  The data steps are the figure's step 1 ("1
# Data", with the numbers from an ARD below them); the figure settings and
# the layers its step 2 ("2 Figure").  Each as a table's step 2, input at
# the left, the result at the right:
#   left    the pieces, as a stack: add, move up or down, remove; a piece
#           with a problem is marked;
#   middle  the piece chosen, on a form: its fields, the default shown, the
#           data's datasets, variables and PARAMCDs to pick from;
#   right   (step 2) the figure as its program saves it (the PNG, at its
#           size), redrawn on each change, and the checks;
#   below   the code the design makes and the design itself (YAML).
#
# What the pieces and their fields are comes from tflspec's
# tfl_fig_parts(); the design is the figure's (fig_design()), saved with
# the study.

.pd_sections <- c(data = "Data steps", plot = "Figure settings",
                  layers = "Layers")

# the data steps that make an object of their own (a KM fit, summary
# statistics ...): the steps after one go on in its pipe (#293)
.pd_named_steps <- c(survfit = "fit", summary = "sm", summary_by = "sg",
                     rate = "rt", count = "ct", subset = "sub", `function` = "res")

# the functions a figure's "Any function" step suggests (any pkg::fn can be
# typed)
.pd_fn_suggest <- c("survival::survfit", "ggsurvfit::survfit2", "survival::coxph",
                    "stats::quantile", "stats::lm", "stats::glm")

# A function's arguments in short, for its "Arguments (R)" field: their
# names (a required one starred), not the data (the step's From)
.pd_fn_args_hint <- function(fn) {
  if (is.null(fn) || !nzchar(fn %||% "")) return(NULL)
  a <- tryCatch(tflspec::tfl_ard_args(fn), error = function(e) NULL)
  if (is.null(a) || !nrow(a)) return(NULL)
  a <- a[!a$kind %in% c("data", "statistics", "denominator") & a$arg != "...", , drop = FALSE]
  if (!nrow(a)) return(NULL)
  paste(paste0(a$arg, ifelse(a$required, "*", "")), collapse = ", ")
}

.designer_ui <- function(t) {
  shiny::conditionalPanel(
    "output.report_kind == 'figure'",
    shiny::tags$style(shiny::HTML("
      .pd-item { cursor: pointer; padding: .25rem .5rem; font-size: .85rem; }
      .pd-item.active { background: #e7f1ff; border-left: 3px solid #0d6efd; }
      .pd-item .pd-sum { color: #6c757d; font-size: .75rem; }
      .pd-item .pd-bad { color: #dc3545; font-weight: bold; }
      .pd-tools button { padding: 0 .3rem; font-size: .75rem; }
      .pd-sec { font-size: .8rem; font-weight: 600; margin: .5rem 0 .2rem; }
      .pd-code textarea { font-family: monospace; font-size: .8rem; }
      .pd-overlay { background: #f8f9fa; border: 1px solid #dee2e6; border-radius: .3rem;
        padding: .4rem .6rem; font-size: .8rem; margin-bottom: .5rem; max-height: 14rem; overflow: auto; }
      .pd-overlay .badge { font-size: .65rem; }
      .pd-overlay button { padding: 0 .4rem; font-size: .7rem; }
      .pd-overlay-head { display: flex; justify-content: space-between; align-items: center; }
      .pd-piece-code pre { font-size: .75rem; max-height: calc(100vh - 16rem); min-height: 4rem; margin-bottom: 0; resize: vertical; }
      .pd-auto .form-group, .pd-auto .checkbox { margin: 0; }
      .pd-auto label { font-size: .8rem; font-weight: normal; margin: 0; }")),
    shiny::uiOutput("pd_note"),
    # one way to make a figure: the designer (its first step a template or
    # empty); the plot written by hand, user code, apart below
    shiny::uiOutput("pd_body"))
}

# A figure's step 1 (#293): the data steps of its design, their form
.designer_data_ui <- function() {
  shiny::conditionalPanel(
    "output.report_kind == 'figure'",
    shiny::uiOutput("pd_data_body"))
}

# the sections of a design each step shows (the code lists go with the data)
.pd_step_secs <- list(data = c("data", "codelists"), figure = c("plot", "layers"))
.pd_in_step <- function(s, step) isTRUE(s$sec %in% .pd_step_secs[[step]])

# A select input with some of its options disabled (selectize keeps them
# listed but not choosable)
.disable_options <- function(tag, values) {
  html <- as.character(tag)
  for (v in values) {
    html <- sub(paste0('<option value="', v, '"'),
                paste0('<option value="', v, '" disabled'), html, fixed = TRUE)
  }
  # (the selectize script and style go with it)
  htmltools::attachDependencies(htmltools::HTML(html),
                                htmltools::findDependencies(tag))
}

# Can a study with these datasets draw a template?  Its `data` is written
# as tflspec's catalog writes it: "ADTR + ADRS" (both), "ADLB / ADVS + ADSL"
# (ADLB or ADVS, and ADSL); "any" and "(your estimates)" need nothing.
.fig_data_ok <- function(data, have) {
  if (is.null(data) || is.na(data) || !nzchar(data)) return(TRUE)
  have <- toupper(have)
  parts <- trimws(strsplit(data, "+", fixed = TRUE)[[1L]])
  all(vapply(parts, function(p) {
    alt <- toupper(trimws(strsplit(p, "/", fixed = TRUE)[[1L]]))
    !all(grepl("^[A-Z][A-Z0-9]*$", alt)) || any(alt %in% have)
  }, NA))
}

# the dataset a template reads first, of those the study has
.fig_first_data <- function(data, have) {
  if (is.null(data) || is.na(data)) return(NA_character_)
  alt <- toupper(trimws(strsplit(trimws(strsplit(data, "+", fixed = TRUE)[[1L]])[1L],
                                 "/", fixed = TRUE)[[1L]]))
  alt <- alt[alt %in% toupper(have)]
  if (length(alt)) alt[1L] else NA_character_
}

# The template choices: under their category (tflspec >= 0.0.24.9003; the
# kind before), each with the data it reads; those the study's data cannot
# draw stay listed but cannot be chosen, and say why.
.fig_template_choices <- function(templates, have, words) {
  cat <- if ("category" %in% names(templates)) templates$category else templates$kind
  dat <- if ("data" %in% names(templates)) templates$data else rep(NA_character_, nrow(templates))
  ok <- vapply(dat, .fig_data_ok, NA, have = have, USE.NAMES = FALSE)
  lab <- paste0(words$label(templates$label),
                ifelse(is.na(dat), "", paste0("  \u00b7 ", dat)),
                ifelse(ok, "", paste0("  \u2014 ", words$missing)))
  ch <- split(stats::setNames(templates$template, lab),
              factor(cat, levels = unique(cat)))
  ch <- lapply(ch, as.list)
  ch[[words$other]] <- stats::setNames(list(""), words$empty)
  # the first choice: of those it can draw, the one that uses most of the
  # study's data besides ADSL (an AE figure for a study of ADSL and ADAE)
  used <- vapply(dat, function(d) {
    if (is.na(d)) return(0L)
    ds <- toupper(trimws(unlist(strsplit(d, "[+/]"))))
    length(intersect(setdiff(ds, "ADSL"), toupper(have)))
  }, 0L, USE.NAMES = FALSE)
  best <- if (any(ok)) which(ok)[which.max(used[ok])] else NA
  list(choices = ch, off = templates$template[!ok],
       first = if (is.na(best)) "" else templates$template[best])
}

# the value an input gives, as a field: NULL = not set (the default)
.pd_value <- function(kind, v) {
  if (is.null(v) || !length(v)) return(NULL)
  if (kind == "logical") return(isTRUE(v))
  if (kind == "number") {
    v <- suppressWarnings(as.numeric(v))
    return(if (length(v) != 1L || is.na(v)) NULL else v)
  }
  v <- as.character(v)
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(NULL)
  if (kind %in% c("variables", "param", "ard_stats")) return(paste(v, collapse = ", "))
  v[1L]
}

# the pieces that read the figure's ARD (offered when it has one, #293)
.pd_ard_pieces <- c("ard_stats", "ard_number")

# a line on what a piece is set to
.pd_summary <- function(p) {
  # a piece of the figure's ARD: its address (the analysis, variable,
  # statistic, group)
  if (isTRUE(p$step %in% .pd_ard_pieces) || isTRUE(p$layer %in% .pd_ard_pieces)) {
    g <- if (is.list(p$group)) paste(names(p$group), "=", unlist(p$group)) else p$group
    x <- paste(c(p$analysis_id, p$variable, p$stat %||% p$stats, g), collapse = " ")
    return(if (nchar(x) > 40) paste0(substr(x, 1, 38), "..") else x)
  }
  if (isTRUE(p$step %in% c("ard", "function"))) {
    x <- paste(c(p$analysis_id %||% p$name, sub("^.*::", "", p$fn %||% "")), collapse = " ")
    return(if (nchar(x) > 40) paste0(substr(x, 1, 38), "..") else x)
  }
  keys <- c("dataset", "value", "variable", "expr", "vars", "time", "by",
            "x", "y", "yintercept", "xintercept", "geom", "type", "unit")
  v <- unlist(lapply(keys, function(k) {
    x <- p[[k]]
    if (is.null(x) || !length(x) || identical(x, "")) return(NULL)
    x <- paste(x, collapse = ", ")
    if (nchar(x) > 28) x <- paste0(substr(x, 1, 26), "..")
    if (k %in% c("dataset", "value", "variable", "expr", "geom", "type")) x
    else paste0(k, "=", x)
  }))
  paste(utils::head(v, 3), collapse = "  ")
}

# One object the data steps made, in short: a data frame's size, columns
# and first rows; anything else, the first lines it prints
.pd_object_ui <- function(name, o, t = identity) {
  if (identical(o$class, "data frame")) {
    h <- o$head
    cells <- lapply(h, function(v) {
      v <- format(v, digits = 4L)
      ifelse(nchar(v) > 24L, paste0(substr(v, 1L, 22L), ".."), v)
    })
    return(shiny::div(
      class = "mb-2 small",
      shiny::strong(shiny::code(name)), " ",
      shiny::span(class = "text-muted", sprintf(t("data frame, %d rows x %d columns"),
                                                o$rows, length(o$columns))),
      shiny::tags$details(
        shiny::tags$summary(class = "text-muted", t("its first rows")),
        shiny::div(
          style = "overflow-x: auto",
          shiny::tags$table(
            class = "table table-sm table-bordered mb-0 small",
            shiny::tags$thead(shiny::tags$tr(lapply(o$columns, shiny::tags$th))),
            shiny::tags$tbody(lapply(seq_len(nrow(h)), function(i)
              shiny::tags$tr(lapply(cells, function(v) shiny::tags$td(v[i]))))))))))
  }
  shiny::div(
    class = "mb-2 small",
    shiny::strong(shiny::code(name)), " ", shiny::span(class = "text-muted", o$class),
    shiny::tags$pre(class = "small mb-0", paste(o$text, collapse = "\n")))
}

.pd_act <- function(op, sec, i) {
  sprintf("event.stopPropagation(); Shiny.setInputValue('pd_act', {op: '%s', sec: '%s', i: %d, n: Math.random()}, {priority: 'event'})",
          op, sec, i)
}
