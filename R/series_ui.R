# The Template series card (Study tab): what it draws.  A series (tflspec's
# template series: series.yml's `values:` and its templates) is read by
# tflspec; the study's own values for it are study.yml's
# `series_settings: {<series>: {...}}`; spec/series.lock pins what the study
# was made with.  The server is R/srv_series.R.

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
# value an input while it is changed here.  `draft`: the study's block as
# edited (key -> YAML text); a key of the block the series does not have is
# shown too (never dropped).
.series_values_ui <- function(values, draft, saved, t = identity) {
  keys <- union(names(values), names(draft))
  if (!length(keys)) {
    return(shiny::p(class = "small text-muted", t("This series has no values.")))
  }
  act <- function(id, key, label) {
    shiny::tags$button(
      type = "button", class = "btn btn-sm btn-link p-0", `data-key` = key,
      onclick = sprintf("Shiny.setInputValue('%s', {key: this.dataset.key, n: Date.now()}, {priority: 'event'});", id),
      label)
  }
  rows <- lapply(seq_along(keys), function(i) {
    k <- keys[i]
    has <- k %in% names(values)
    v <- if (has) values[[k]] else NULL
    mine <- k %in% names(draft)
    txt <- if (mine) draft[[k]] else ""
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(k)),
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
        if (mine) act("series_back", k, t("Back to the series value")) else
          if (has) act("series_change", k, t("Change for this study"))))
  })
  shiny::tags$table(
    class = "table table-sm small align-middle mb-2",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(t("Key")), shiny::tags$th(t("Series value (values:)")),
      shiny::tags$th(t("This study's value (series_settings:)")),
      shiny::tags$th(""))),
    shiny::tags$tbody(rows))
}

# spec/series.lock as a table: a row a series (its file's md5 and its
# templates'), and the file as written below it
.series_lock_ui <- function(lock, text, notes = character(), t = identity) {
  out <- list()
  if (length(notes)) {
    out <- c(out, list(shiny::div(
      class = "alert alert-warning small py-1 px-2 mb-2",
      shiny::tags$ul(class = "mb-0 ps-3", lapply(notes, shiny::tags$li)))))
  }
  if (is.null(text)) {
    return(shiny::tagList(c(out, list(shiny::p(
      class = "small text-muted", t("No spec/series.lock yet: nothing is pinned."))))))
  }
  rows <- lapply(names(lock), function(s) {
    x <- lock[[s]]
    tpl <- x$templates
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(s)),
      shiny::tags$td(shiny::tags$code(as.character(x$path %||% ""))),
      shiny::tags$td(shiny::tags$code(as.character(x$series %||% ""))),
      shiny::tags$td(if (length(tpl)) {
        lapply(names(tpl), function(n) shiny::div(
          shiny::tags$code(n), " ", shiny::span(class = "text-muted", as.character(tpl[[n]]))))
      }))
  })
  shiny::tagList(c(out, list(
    shiny::tags$table(
      class = "table table-sm small mb-1",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(t("Series (name)")), shiny::tags$th(t("Folder (path)")),
        shiny::tags$th(t("series.yml's md5 (series)")),
        shiny::tags$th(t("Templates' md5 (templates)")))),
      shiny::tags$tbody(rows)),
    shiny::tags$details(
      class = "small",
      shiny::tags$summary(t("spec/series.lock as written")),
      shiny::tags$pre(class = "small mb-0", paste(text, collapse = "\n"))))))
}
