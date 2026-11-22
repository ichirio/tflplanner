# The Template series card (Study tab): what it draws.  A series (tflspec's
# template series: series.yml's `values:` and its templates) is read by
# tflspec from its source (the company's folders, tflspec's own).  A study
# reads only its own copy of the series and of each template its reports use
# (<study>/spec/series/<series>/: series.yml, templates/<name>.yml and
# tflspec's copied.yml; canon 2026-11-19): the study's values are its copy's
# `values:`, and each copied file's state against its source is listed.
# The server is R/srv_series.R.

# whether this tflspec has template series (an older one has none)
.series_available <- function() {
  exists("tfl_series_list", envir = asNamespace("tflspec"), inherits = FALSE)
}

# one of tflspec's series functions (looked up when called: an older tflspec
# has none, and tests replace them)
.series_fn <- function(name) {
  get(name, envir = asNamespace("tflspec"), inherits = FALSE)
}

# Where a site's series are: tflplanner's home's standards/series, told to
# tflspec when the home is not R_user_dir() (tflspec looks there itself).
# An option set by the user is kept; one this function set before follows
# the home.
.series_opt <- new.env(parent = emptyenv())
.series_dir_option <- function(home = tflplanner_home()) {
  user <- normalizePath(tools::R_user_dir("tflplanner", "data"), "/", mustWork = FALSE)
  now <- getOption("tflspec.series_dir")
  mine <- !is.null(now) && identical(now, .series_opt$set)
  if (!is.null(now) && !mine) return(invisible(now))
  want <- if (identical(normalizePath(home, "/", mustWork = FALSE), user)) NULL else
    file.path(home, "standards", "series")
  options(tflspec.series_dir = want)
  .series_opt$set <- want
  invisible(want)
}

# A value as YAML on one line ({a: 1, b: x}, [1, 2], 'a, b'), its tags as
# written (tflspec reads them as markers: !series K, !study K, !fixed v,
# !unset)
.series_flow <- function(v) {
  if (inherits(v, "tfl_series_ref")) return(paste("!series", v$key))
  if (inherits(v, "tfl_study_ref")) return(paste("!study", v$key))
  if (inherits(v, "tfl_fixed_ref")) return(paste("!fixed", .series_flow(v$value)))
  if (inherits(v, "tfl_unset")) return("!unset")
  if (is.null(v)) return("null")
  nm <- names(v)
  if (is.list(v) || length(v) != 1L || !is.null(nm)) {
    inner <- vapply(seq_along(v), function(i) .series_flow(v[[i]]), "")
    if (!is.null(nm) && length(v)) {
      return(paste0("{", paste0(vapply(nm, .series_scalar, ""), ": ", inner,
                                collapse = ", "), "}"))
    }
    if (!is.null(nm)) return("{}")
    return(paste0("[", paste(inner, collapse = ", "), "]"))
  }
  .series_scalar(v)
}
# what "Change for this study" starts from (read back by
# .series_yaml_load()): the value without its tag; a reference to another
# value (!series, !study) or !unset starts empty
.series_edit_text <- function(v) {
  if (inherits(v, "tfl_fixed_ref")) return(.series_flow(v$value))
  if (inherits(v, c("tfl_series_ref", "tfl_study_ref", "tfl_unset"))) return("")
  .series_flow(v)
}
.series_scalar <- function(x) {
  if (is.na(x)) return("null")
  if (is.character(x)) {
    plain <- nzchar(x) && !grepl("[][{},:#&*!|>'\"]|^[-?%@`]|^ | $", x) &&
      identical(.series_yaml_load(x), x)
    return(if (plain) x else paste0("'", gsub("'", "''", x, fixed = TRUE), "'"))
  }
  sub("\n$", "", yaml::as.yaml(x, handlers = list(logical = yaml::verbatim_logical)))
}

# YAML as typed for a study's value: only true / false are logical (y, n,
# yes, no stay text: N is formats' count, not FALSE)
.series_yaml_load <- function(x) {
  keep <- function(v) if (v %in% c("true", "True", "TRUE", "false", "False", "FALSE")) {
    tolower(v) == "true"
  } else {
    v
  }
  yaml::yaml.load(x, handlers = list(`bool#yes` = keep, `bool#no` = keep))
}

# a study's value's input: as many rows as its text needs (about 40
# characters a row)
.series_rows <- function(txt) {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  max(1L, sum(pmax(1L, ceiling(nchar(lines) / 40))))
}

# where tflspec found a series, in words
.series_source_label <- function(source, t = identity) {
  lab <- c(option = t("Set by an option (tflspec.series_dir)"),
           home = t("tflplanner's home (standards/series)"),
           package = t("In tflspec (package)"))
  out <- unname(lab[source])
  ifelse(is.na(out), source, out)
}

# The series list as a table (DT's data)
.series_list_table <- function(d, t = identity) {
  if (is.null(d) || !nrow(d)) {
    d <- data.frame(name = character(), label = character(), released = character(),
                    source = character(), path = character())
  }
  out <- data.frame(name = d$name, label = d$label, released = as.character(d$released),
                    source = .series_source_label(d$source, t), path = d$path,
                    stringsAsFactors = FALSE)
  names(out) <- c(t("Series (name)"), t("Label (label)"), t("Released (released)"),
                  t("Found in (source)"), t("Folder (path)"))
  out
}

# The chosen series' values beside the study's: one row a key, the study's
# value an input while it is changed here.  `draft`: the study's own values
# as edited (key -> YAML text: its copy's values that differ from the
# source's); a key of the study's copy the source does not have is shown too
# (never dropped).  `descriptions`: series.yml's descriptions: (a key -> its
# meaning in English), shown in the app's language when tflplanner has the
# text's translation, as written when not; a key without one has an empty
# cell (a series, or a tflspec, without them: all empty).
.series_values_ui <- function(values, draft, saved, t = identity, descriptions = NULL) {
  keys <- union(names(values), names(draft))
  if (!length(keys)) {
    return(shiny::p(class = "small text-muted", t("This series has no values.")))
  }
  rows <- lapply(seq_along(keys), function(i) {
    k <- keys[i]
    has <- k %in% names(values)
    v <- if (has) values[[k]] else NULL
    mine <- k %in% names(draft)
    txt <- if (mine) draft[[k]] else ""
    doc <- if (k %in% names(descriptions)) as.character(descriptions[[k]])[1L] else ""
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(k)),
      shiny::tags$td(class = "text-muted", if (!is.na(doc) && nzchar(doc)) t(doc)),
      shiny::tags$td(if (has) shiny::tags$code(.series_flow(v)) else
        shiny::span(class = "text-muted", t("(not in this series)"))),
      shiny::tags$td(
        if (mine) {
          shiny::textAreaInput(paste0("series_edit_", i), NULL, value = txt, width = "100%",
                               rows = .series_rows(txt),
                               resize = "vertical")
        } else if (k %in% names(saved)) {
          shiny::span(class = "text-muted", t("(removed; save to apply)"))
        } else {
          shiny::span(class = "text-muted", "-")
        }),
      shiny::tags$td(
        if (mine) .series_act("series_back", k, t("Back to the series value")) else
          if (has) .series_act("series_change", k, t("Change for this study"))))
  })
  shiny::tags$table(
    class = "table table-sm small align-middle mb-2",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(t("Key")), shiny::tags$th(t("Description")),
      shiny::tags$th(t("Series value (values:)")),
      shiny::tags$th(t("This study's value (its copy's values:)")),
      shiny::tags$th(""))),
    shiny::tags$tbody(rows))
}

# a link-like button that says its key to the server (`id`: the input)
.series_act <- function(id, key, label, class = "btn btn-sm btn-link p-0") {
  shiny::tags$button(
    type = "button", class = class, `data-key` = key,
    onclick = sprintf("Shiny.setInputValue('%s', {key: this.dataset.key, n: Date.now()}, {priority: 'event'});", id),
    label)
}

# A copied file's state (tflspec's tfl_template_status() `status`), in words
.series_status_label <- function(status, t = identity) {
  lab <- c(same = t("Same as the source"),
           source_changed = t("The source has changed"),
           study_changed = t("Changed in this study"),
           both_changed = t("Changed in both the source and this study"),
           source_missing = t("The source is missing"))
  out <- unname(lab[status])
  ifelse(is.na(out), status, out)
}

# a status the study's own edits of a copy are in (a copy again loses them)
.series_own_edits <- function(status) status %in% c("study_changed", "both_changed")

# The study's copies (tfl_template_status()) as a table: a row a copied file
# -- the template ("<series>/<name>"; series.yml for the series' own file),
# where it was copied from, when, and its state in words.  A template whose
# copy is not the source's (its source still there) offers its differences
# and a copy again from the source.
.series_copies_ui <- function(d, t = identity) {
  if (is.null(d) || !NROW(d)) {
    return(shiny::p(class = "small text-muted",
                    t("This study has no copies yet: adding a report from a template copies it into the study (tfl_copy_templates).")))
  }
  rows <- lapply(seq_len(nrow(d)), function(i) {
    tpl <- d$template[i]
    id <- if (is.na(tpl)) NA_character_ else paste0(d$series[i], "/", tpl)
    st <- d$status[i]
    acts <- !is.na(id) && !st %in% c("same", "source_missing")
    shiny::tags$tr(
      class = if (!identical(st, "same")) "table-warning",
      shiny::tags$td(shiny::tags$code(if (is.na(id)) paste0(d$series[i], "/series.yml") else id)),
      shiny::tags$td(shiny::tags$code(class = "small", as.character(d$from[i]))),
      shiny::tags$td(as.character(d$copied_at[i])),
      shiny::tags$td(.series_status_label(st, t)),
      shiny::tags$td(class = "text-nowrap", if (acts) shiny::tagList(
        .series_act("series_diff", id, t("Show the differences")),
        " ",
        .series_act("series_recopy", id, t("Copy again from the source")))))
  })
  shiny::tags$table(
    class = "table table-sm small align-middle mb-1",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(t("Template (template)")), shiny::tags$th(t("Copied from (from)")),
      shiny::tags$th(t("Copied at (copied_at)")), shiny::tags$th(t("Status (status)")),
      shiny::tags$th(""))),
    shiny::tags$tbody(rows))
}

# tfl_template_diff()'s lines (" " both, "-" the copy only, "+" the source
# only; its two header lines dropped) as a block, the changed lines marked
.series_diff_ui <- function(lines, t = identity) {
  body <- lines[!lines %in% c("--- copy", "+++ source")]
  mark <- substr(body, 1L, 1L)
  if (!length(body) || all(mark == " ")) {
    return(shiny::p(class = "small text-muted", t("No differences.")))
  }
  cls <- ifelse(mark == "-", "rp-diff-copy", ifelse(mark == "+", "rp-diff-source", ""))
  shiny::tagList(
    shiny::p(class = "small mb-1",
             shiny::span(class = "rp-diff-copy", t("- this study's copy only")), "  ",
             shiny::span(class = "rp-diff-source", t("+ the source only"))),
    shiny::tags$style(shiny::HTML(paste(
      ".rp-diff-copy{background:#fde2e1;color:#8a1c14}",
      ".rp-diff-source{background:#dcf5e3;color:#14602f}",
      ".rp-diff pre{margin:0}"))),
    shiny::div(class = "rp-code rp-diff",
               style = "max-height:60vh;overflow:auto",
               shiny::tags$pre(class = "small mb-0", lapply(seq_along(body), function(i) {
                 shiny::div(class = cls[i], body[i])
               }))))
}
