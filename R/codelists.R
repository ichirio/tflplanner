# A report's code lists.  A code list is a report's (tflspec: every row of
# the codelists sheet names its report): the same treatment arm may print
# one way in one table and another way in the next.  What a company or a
# study uses again is copied into a report -- from the company standards
# (their codelists sheet) or from another report -- and edited there.  One
# editor (the grid of a report's rows, the variables it uses or all, the
# copy dialog, a file read in) serves step 1 and the places that call it.

# The variables a report uses, for its code lists: what its ARD analyses
# read (by, strata, variables), the columns its analysis data, datasets and
# populations derive, and what its table shows (tables rows / cols, the
# variables sheet).  A superset is fine: it only narrows the grid.
.codelist_vars <- function(x, output_id) {
  if (is.null(output_id)) return(character())
  bar <- function(v) unlist(lapply(v[!is.na(v)], .split_bar))
  a <- x$ard$analyses
  a <- a[!is.na(a$output_id) & a$output_id == output_id, , drop = FALSE]
  made <- function(d) if (!is.null(d$derive)) trimws(sub("=.*$", "", bar(d$derive)))
  ad <- .adata_rows(x, output_id)
  words <- function(v) {
    v <- v[!is.na(v)]
    unlist(regmatches(v, gregexpr("[A-Za-z][A-Za-z0-9_.]*", v)))
  }
  tb <- sheet_rows(x, "tables", "")
  tb <- tb[is.na(tb$output_id) | tb$output_id == output_id, , drop = FALSE]
  vs <- sheet_rows(x, "variables", "")
  vs <- vs[is.na(vs$output_id) | vs$output_id == output_id, , drop = FALSE]
  v <- unique(c(bar(a$by), bar(a$strata), bar(a$variables),
                made(ad), made(x$ard$datasets), made(x$ard$populations),
                words(tb$rows), words(tb$cols), vs$variable))
  v[!is.na(v) & nzchar(v)]
}

# The variables a report's ARD reads (tflspec makes factors of their code
# lists: a value no record has is counted 0); the rest only print
.codelist_ard_vars <- function(x, output_id) {
  a <- x$ard$analyses
  a <- a[!is.na(a$output_id) & a$output_id == output_id, , drop = FALSE]
  bar <- function(v) unlist(lapply(v[!is.na(v)], .split_bar))
  unique(c(bar(a$by), bar(a$strata), bar(a$variables)))
}

#' A report's code lists: copy one in
#'
#' A code list is a report's: every row of the `codelists` sheet names its
#' report.  `import_codelist()` copies another report's code lists into
#' one (all, or those of some variables); `standard_codelists()` gives the
#' company standards' (their `codelists` sheet: one row a value, `set`
#' naming each list), for [set_codelist()].  The rows copied are the
#' report's own: nothing ties them to where they came from.
#'
#' @param x A `tflplanner`.
#' @param from_output The report copied from.
#' @param output_id The report copied into.
#' @param variables Only these variables' code lists; `NULL` for all.
#' @param sets Only these sets of the standards; `NULL` for all.
#' @param home tflplanner's home.
#' @return `import_codelist()`: the `tflplanner`; `standard_codelists()`: a
#'   data frame (`set`, `variable`, `value`, `label`, `order`, `note`).
#' @export
import_codelist <- function(x, from_output, output_id, variables = NULL) {
  src <- sheet_rows(x, "codelists", from_output)
  if (!is.null(variables)) {
    miss <- setdiff(variables, src$variable)
    if (length(miss)) {
      stop("Report '", from_output, "' has no code list of ",
           paste(sQuote(miss), collapse = ", "), ".", call. = FALSE)
    }
    src <- src[src$variable %in% variables, , drop = FALSE]
  }
  if (!nrow(src)) {
    stop("Report '", from_output, "' has no code lists to copy.", call. = FALSE)
  }
  set_codelist(x, output_id, src)
}

#' @rdname import_codelist
#' @export
standard_codelists <- function(sets = NULL, home = tflplanner_home()) {
  cl <- company_standards(home)$codelists
  if (is.null(cl) || !nrow(cl)) return(.builtin_standards()$codelists[0L, ])
  cl$set[is.na(cl$set)] <- cl$variable[is.na(cl$set)]
  if (!is.null(sets)) cl <- cl[cl$set %in% sets, , drop = FALSE]
  rownames(cl) <- NULL
  cl
}

# The company standards' code lists a new installation has: CDISC's usual
# ones (a company puts its own in the standards workbook)
.builtin_codelists <- function() {
  one <- function(var, values, labels, note = NA_character_) {
    data.frame(set = var, variable = var, value = values, label = labels,
               order = as.character(seq_along(values)), note = note,
               stringsAsFactors = FALSE)
  }
  rbind(
    one("SEX", c("F", "M", "U"), c("Female", "Male", "Unknown")),
    one("RACE", c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN",
                  "AMERICAN INDIAN OR ALASKA NATIVE",
                  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "MULTIPLE",
                  "NOT REPORTED", "UNKNOWN"),
        c("White", "Black or African American", "Asian",
          "American Indian or Alaska Native",
          "Native Hawaiian or Other Pacific Islander", "Multiple",
          "Not reported", "Unknown")),
    one("ETHNIC", c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO",
                    "NOT REPORTED", "UNKNOWN"),
        c("Hispanic or Latino", "Not Hispanic or Latino", "Not reported",
          "Unknown")),
    one("AESEV", c("MILD", "MODERATE", "SEVERE"), c("Mild", "Moderate", "Severe")),
    one("AESER", c("Y", "N"), c("Serious", "Not serious")),
    one("AEREL", c("NOT RELATED", "UNLIKELY RELATED", "POSSIBLY RELATED",
                   "PROBABLY RELATED", "RELATED"),
        c("Not related", "Unlikely related", "Possibly related",
          "Probably related", "Related"),
        "the sponsor's terms: edit to the company's"),
    one("AEOUT", c("RECOVERED/RESOLVED", "RECOVERING/RESOLVING",
                   "NOT RECOVERED/NOT RESOLVED",
                   "RECOVERED/RESOLVED WITH SEQUELAE", "FATAL", "UNKNOWN"),
        c("Recovered/resolved", "Recovering/resolving",
          "Not recovered/not resolved", "Recovered/resolved with sequelae",
          "Fatal", "Unknown")),
    one("EOSSTT", c("COMPLETED", "DISCONTINUED", "ONGOING"),
        c("Completed", "Discontinued", "Ongoing")))
}

# -- the editor ------------------------------------------------------------

# Its controls, the grid and the help; `prefix` names its inputs and
# outputs (one editor per place)
.codelist_editor_ui <- function(prefix, t) {
  p <- function(x) paste0(prefix, "_", x)
  shiny::tagList(
    shiny::div(
      class = "d-flex flex-wrap gap-3 align-items-center mb-1",
      .btn(p("copy"), t("Copy code lists..."),
           class = "btn-sm btn-outline-secondary py-0"),
      shiny::div(class = "small",
                 shiny::checkboxInput(p("all"), t("Every variable, not only those this report uses"),
                                      FALSE)),
      shiny::uiOutput(p("hidden"), inline = TRUE)),
    rhandsontable::rHandsontableOutput(p("hot")),
    shiny::tags$details(
      class = "small mt-2",
      shiny::tags$summary(t("Read a file into this report")),
      shiny::fileInput(
        p("file"), t("A code list (xlsx / csv: variable, value, label, order)"),
        accept = c(".xlsx", ".csv"), width = "100%")),
    shiny::tags$details(
      class = "rp-help mt-2",
      shiny::tags$summary(t("Column help")),
      DT::DTOutput(p("help"))))
}

# The editor's server: `report()` the report, `vars()` the variables to show
# (all with the box ticked).  The app's helpers come in as arguments.
.codelist_editor_server <- function(input, output, session, prefix, rv, report,
                                    vars, t, notify, guarded, bump, read_grid,
                                    grids_drawn, has_study) {
  p <- function(x) paste0(prefix, "_", x)
  key <- shiny::reactive(paste(prefix, report() %||% "", isTRUE(input[[p("all")]]),
                               rv$ver, sep = "|"))
  shown <- function(d) {
    if (isTRUE(input[[p("all")]])) return(rep(TRUE, nrow(d)))
    d$variable %in% vars() | is.na(d$variable)
  }
  output[[p("hot")]] <- rhandsontable::renderRHandsontable({
    shiny::req(has_study(), !is.null(report()))
    key()
    d <- sheet_rows(shiny::isolate(rv$p), "codelists", report())
    d <- d[shown(d), , drop = FALSE]
    d$output_id <- NULL
    grids_drawn()
    ch <- .std_choices("codelists")
    ch$variable <- unique(c(vars(), ch$variable))
    .grid(d, "codelists", key(), ch)
  })
  output[[p("hidden")]] <- shiny::renderUI({
    shiny::req(has_study(), !is.null(report()))
    rv$ver
    if (isTRUE(input[[p("all")]])) return(NULL)
    d <- sheet_rows(rv$p, "codelists", report())
    n <- length(unique(d$variable[!shown(d)]))
    if (!n) return(NULL)
    shiny::span(class = "small text-muted",
                sprintf(t("(%d more variables' code lists: tick the box to see them)"), n))
  })
  shiny::observeEvent(input[[p("hot")]], {
    h <- input[[p("hot")]]
    if (is.null(h$changes$changes) &&
        !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
    if (!identical(h$params$planner_key, key())) return()
    d <- read_grid(h)
    id <- report()
    if (is.null(d) || is.null(id)) return()
    old <- sheet_rows(rv$p, "codelists", id)
    keep <- old[!shown(old), , drop = FALSE]
    keep$output_id <- NULL
    d <- .drop_blank_rows(as.data.frame(d, stringsAsFactors = FALSE))
    guarded(rv$p <- set_sheet_rows(rv$p, "codelists", id,
                                   rbind(keep, d[names(keep)])))
  })
  output[[p("help")]] <- DT::renderDT(
    .help_table("codelists"), rownames = FALSE,
    options = list(dom = "t", paging = FALSE, ordering = FALSE))
  # a file's code list, into this report
  shiny::observeEvent(input[[p("file")]], {
    f <- input[[p("file")]]
    id <- report()
    if (is.null(f) || !has_study() || is.null(id)) return()
    ext <- tolower(tools::file_ext(f$name))
    path <- f$datapath
    if (!identical(tolower(tools::file_ext(path)), ext)) {
      file.copy(path, p2 <- paste0(path, ".", ext))
      path <- p2
    }
    rows <- guarded(read_codelist(path))
    if (is.null(rows)) return()
    p2 <- guarded(set_codelist(rv$p, id, rows))
    if (is.null(p2)) return()
    rv$p <- p2
    bump()
    notify(sprintf(t("%d values of %d variables read into %s's code lists."),
                   nrow(rows), length(unique(rows$variable)), id))
  })
  # copy: from the company standards or another report
  shiny::observeEvent(input[[p("copy")]], {
    id <- report()
    shiny::req(has_study(), id)
    cl <- sheet_rows(rv$p, "codelists", "")
    others <- setdiff(unique(cl$output_id[!is.na(cl$output_id)]), id)
    from <- c(if (nrow(standard_codelists())) stats::setNames(".std", t("The company standards")),
              stats::setNames(others, others))
    if (!length(from)) return(notify(t("No code lists to copy: the company standards have none, nor do the other reports."), "warning"))
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Copy code lists into %s"), id), easyClose = TRUE,
      shiny::selectInput(p("from"), t("From"), from, width = "100%"),
      shiny::uiOutput(p("which")),
      shiny::uiOutput(p("as")),
      shiny::p(class = "small text-muted",
               t("The rows copied are this report's own: change them here, and the source stays as it is. A value this report has already is replaced.")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn(p("copy_ok"), t("Copy"), class = "btn-primary"))))
  })
  # what can be copied: list(choices (named: what shows), the rows of each)
  source_rows <- function(f) {
    if (identical(f, ".std")) {
      s <- standard_codelists()
      return(split(s, factor(s$set, levels = unique(s$set))))
    }
    d <- sheet_rows(rv$p, "codelists", f)
    split(d, factor(d$variable, levels = unique(d$variable)))
  }
  output[[p("which")]] <- shiny::renderUI({
    f <- input[[p("from")]]
    shiny::req(f)
    src <- source_rows(f)
    if (!length(src)) return(NULL)
    lab <- vapply(names(src), function(k) {
      r <- src[[k]]
      v <- utils::head(r$value, 4L)
      more <- if (nrow(r) > 4L) ", ..." else ""
      head <- if (identical(k, r$variable[1L])) k else paste0(k, " (", r$variable[1L], ")")
      paste0(head, ": ", paste(v, collapse = ", "), more)
    }, "")
    use <- vapply(src, function(r) r$variable[1L] %in% vars(), NA)
    shiny::checkboxGroupInput(p("pick"), t("Its code lists"),
                              stats::setNames(names(src), lab),
                              selected = names(src)[use], width = "100%")
  })
  output[[p("as")]] <- shiny::renderUI({
    f <- input[[p("from")]]
    k <- input[[p("pick")]]
    shiny::req(f, length(k) == 1L)
    r <- source_rows(f)[[k]]
    shiny::req(r)
    shiny::textInput(p("as_var"), t("As the variable (its name in this report's data)"),
                     r$variable[1L], width = "100%")
  })
  shiny::observeEvent(input[[p("copy_ok")]], {
    id <- report()
    k <- input[[p("pick")]]
    if (!length(k)) return(notify(t("Choose at least one."), "warning"))
    rows <- do.call(rbind, lapply(source_rows(input[[p("from")]])[k],
                                  function(r) r[c("variable", "value", "label", "order")]))
    as <- trimws(input[[p("as_var")]] %||% "")
    if (length(k) == 1L && nzchar(as)) rows$variable <- as
    p2 <- guarded(set_codelist(rv$p, id, rows))
    if (is.null(p2)) return()
    rv$p <- p2
    shiny::removeModal()
    bump()
    notify(sprintf(t("%s's code lists: %s copied (save to keep them)."), id,
                   paste(unique(rows$variable), collapse = ", ")))
  })
  invisible(NULL)
}
