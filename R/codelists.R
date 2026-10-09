# A report's code lists.  A code list is a report's (tflspec: every row of
# the codelists sheet names its report): the same treatment arm may print
# one way in one table and another way in the next.  What a company or a
# study uses again is copied into a report -- from the company standards
# (their codelists sheet) or from another report -- and edited there.  One
# editor (the grid of a report's rows, the variables it uses or all, the
# copy dialog, a file read in) serves the places where a report's data is
# made: 1-1 (an analysis data's column definitions), a listing's data, a
# figure's data -- where the program puts them on the data.

# The variables a report uses, for its code lists: what its ARD analyses
# read (by, strata, variables), the columns its analysis data, datasets and
# populations derive, what its table shows (tables rows / cols, the
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
# `reopen`: the editor is in a dialog (.codelist_dialog()) -- the copy
# dialog, which replaces it, comes back to it.
.codelist_editor_server <- function(input, output, session, prefix, rv, report,
                                    vars, t, notify, guarded, bump, read_grid,
                                    grids_drawn, has_study, reopen = NULL) {
  p <- function(x) paste0(prefix, "_", x)
  back <- function() if (is.null(reopen)) shiny::removeModal() else reopen()
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
  open_copy <- function() {
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
      footer = shiny::tagList(.btn(p("copy_cancel"), t("Cancel"), class = "btn-outline-secondary"),
                              .btn(p("copy_ok"), t("Copy"), class = "btn-primary"))))
  }
  shiny::observeEvent(input[[p("copy")]], open_copy())
  shiny::observeEvent(input[[p("copy_cancel")]], back())
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
    bump()
    back()
    notify(sprintf(t("%s's code lists: %s copied (save to keep them)."), id,
                   paste(unique(rows$variable), collapse = ", ")))
  })
  invisible(list(copy = open_copy))
}

# The editor in a dialog, for the places that call it (1-1's column
# definitions, a listing's data, a figure's data):
# `open(vars, title)` shows the code lists of those variables (all with the
# box ticked); `done()` runs when it is closed (what reads the code lists
# draws again)
.codelist_dialog_server <- function(input, output, session, prefix, rv, report, t,
                                    notify, guarded, bump, read_grid, grids_drawn,
                                    has_study, done = function() NULL) {
  p <- function(x) paste0(prefix, "_", x)
  vars <- shiny::reactiveVal(character())
  title <- shiny::reactiveVal("")
  show <- function() {
    shiny::showModal(shiny::modalDialog(
      title = shiny::isolate(title()), size = "l", easyClose = FALSE,
      shiny::p(class = "small text-muted",
               t("This report's code lists: each variable's values, their order and what they become in the data (the label). The program puts them on the data where it is made (set_levels()): in the ARD, a listing's or a figure's data each listed column is a factor in the list's order, its values the labels. A value no record has is counted 0; a value the list does not have stops the program. A table's order is the list's unless the variables sheet says another (levels).")),
      .codelist_editor_ui(prefix, t),
      footer = .btn(p("done"), t("Close"), class = "btn-primary")))
  }
  ed <- .codelist_editor_server(input, output, session, prefix, rv, report = report,
                                vars = vars, t = t, notify = notify, guarded = guarded,
                                bump = bump, read_grid = read_grid,
                                grids_drawn = grids_drawn, has_study = has_study,
                                reopen = show)
  shiny::observeEvent(input[[p("done")]], {
    shiny::removeModal()
    done()
  })
  set <- function(v, heading) {
    vars(unique(v[!is.na(v) & nzchar(v)]))
    title(heading)
  }
  # open: the editor; copy: the copy dialog first (the editor after it)
  list(open = function(v, heading) {
    set(v, heading)
    show()
  }, copy = function(v, heading) {
    set(v, heading)
    ed$copy()
  })
}

# -- a study of the old format ---------------------------------------------

# A study made before analysis data and code lists were a report's: their
# rows without a report.  There is no migration (tflspec stops on them):
# the app says so once, in words, and step 1 says why it is empty.
.old_format <- function(p) {
  old <- function(d) {
    if (is.null(d) || !nrow(d)) return(FALSE)
    if (!"output_id" %in% names(d)) return(TRUE)
    rest <- d[setdiff(names(d), "output_id")]
    filled <- rowSums(!is.na(rest) & as.matrix(rest) != "") > 0
    any(filled & (is.na(d$output_id) | !nzchar(trimws(d$output_id))))
  }
  c(if (old(p$ard$analysis_data)) "analysis_data",
    if (old(p$sheets$codelists)) "codelists")
}

# The code list of `variable` (an earlier form of the variables' headings:
# its values the variables' names): the variables sheet's labels now (a
# label there wins), and out of the code lists -- the code lists say what
# the data's values become.  The planner keeps how many it moved
# (attribute "moved_headings"), for the app to say so once.
.move_heading_rows <- function(p) {
  cl <- p$sheets$codelists
  if (is.null(cl) || !nrow(cl)) return(p)
  k <- !is.na(cl$variable) & cl$variable == "variable"
  if (!any(k)) return(p)
  v <- p$sheets$variables
  for (i in which(k & !is.na(cl$label))) {
    hit <- which(v$variable %in% cl$value[i] & v$output_id %in% cl$output_id[i])
    if (!length(hit)) {
      v[nrow(v) + 1L, ] <- NA
      hit <- nrow(v)
      v$output_id[hit] <- cl$output_id[i]
      v$variable[hit] <- cl$value[i]
    }
    if (is.na(v$label[hit[1L]])) v$label[hit[1L]] <- cl$label[i]
  }
  p$sheets$variables <- v
  p$sheets$codelists <- cl[!k, , drop = FALSE]
  attr(p, "moved_headings") <- sum(k)
  p
}

.moved_headings_msg <- paste(
  "The code lists' rows of `variable` (the variables' headings) are the table's labels now",
  "(step 2, a variable's label): moved there.  Save the study to keep it.")

.old_format_msg <- paste(
  "This study is in an old format: its analysis data and code lists are the whole study's, not a report's.",
  "Make it again from the sample, or make a new study.")
.old_format_line <- "An old format: the analysis data have no report. Make the study again from the sample, or make a new study."

# A drag list's levels with the text their code list prints (faint), named
# by the value, which the list gives back; the values as they are when the
# code list says nothing of them
.levels_with_labels <- function(values, variable, codelists) {
  if (!length(values) || is.null(codelists) || !nrow(codelists)) return(values)
  cl <- codelists[!is.na(codelists$variable) & codelists$variable == variable, , drop = FALSE]
  lab <- cl$label[match(values, cl$value)]
  if (all(is.na(lab) | lab == values)) return(values)
  stats::setNames(lapply(seq_along(values), function(i) shiny::span(
    values[i],
    if (!is.na(lab[i]) && !identical(lab[i], values[i]))
      shiny::span(class = "text-muted small ms-1", lab[i]))), values)
}

# -- a report's code lists where its data is made (1-1, a listing's data,
# a figure's data) --------------------------------------------------------

# The code lists of `vars` (a report's rows of the codelists sheet), each a
# line of what it makes of the data: "F -> Female, M -> Male" (a value the
# list prints as it is, alone); list(variable, text), in the lists' order
.codelist_lines <- function(cl, vars = NULL) {
  if (is.null(cl) || !nrow(cl)) return(list())
  cl <- cl[!is.na(cl$variable) & !is.na(cl$value), , drop = FALSE]
  if (!is.null(vars)) cl <- cl[cl$variable %in% vars, , drop = FALSE]
  lapply(split(cl, factor(cl$variable, levels = unique(cl$variable))), function(r) {
    o <- suppressWarnings(as.numeric(r$order))
    r <- r[order(is.na(o), o, seq_len(nrow(r))), , drop = FALSE]
    lab <- ifelse(is.na(r$label) | r$label == r$value, "",
                  paste0(" \u2192 ", r$label))
    list(variable = r$variable[1L], text = paste0(r$value, lab, collapse = ", "))
  })
}

# The values of `data` its code lists do not have, by variable (the
# variables with a list that the data has; a blank is no value): a named
# list of character vectors, only those with some
.codelist_missing <- function(cl, data) {
  if (is.null(cl) || !nrow(cl) || is.null(data)) return(list())
  out <- list()
  for (v in intersect(unique(cl$variable[!is.na(cl$variable)]), names(data))) {
    x <- data[[v]]
    x <- unique(as.character(if (is.factor(x)) levels(droplevels(x)) else x))
    x <- x[!is.na(x) & nzchar(x)]
    miss <- setdiff(x, cl$value[cl$variable == v])
    if (length(miss)) out[[v]] <- sort(miss)
  }
  out
}

# The rows of `data` a condition (R, as the definition writes it) keeps:
# what the code lists meet, for their warning.  A condition that cannot be
# read on these rows (a column made later, R of its own): all the rows.
.rows_where <- function(data, where) {
  if (is.null(data) || is.null(where) || !length(where) || is.na(where) ||
      !nzchar(trimws(where))) return(data)
  k <- tryCatch(eval(parse(text = where, keep.source = FALSE)[[1L]], data, baseenv()),
                error = function(e) NULL)
  if (!is.logical(k) || length(k) != nrow(data)) return(data)
  data[k %in% TRUE, , drop = FALSE]
}

# `values` added to a report's code list of `variable`, after its others,
# each printing as itself: the planner
.codelist_add_values <- function(x, output_id, variable, values) {
  old <- sheet_rows(x, "codelists", output_id)
  o <- suppressWarnings(as.numeric(old$order[old$variable %in% variable]))
  start <- if (any(!is.na(o))) max(o, na.rm = TRUE) else
    sum(old$variable %in% variable)
  set_codelist(x, output_id, data.frame(
    variable = variable, value = values, label = values,
    order = as.character(start + seq_along(values)), stringsAsFactors = FALSE))
}

# The code lists' part of a place where a report's data is made: what the
# program makes of the columns' values (.codelist_lines()), one line per
# column with a list -- a click opens that column's values and labels in a
# grid right below, in place (one grid at a time) -- a column without one
# to start a list for, the values the data has that a list does not, each
# with a button that adds them, and the copy dialog.  `ids`: the inputs
# and outputs (pick, new, close, detail, hot, edit, copy, add); `open`: the
# column whose grid is open; `others`: the place's columns without a list;
# `tip`: what the part says of the place.
.codelist_part_ui <- function(lines, missing, t, ids, tip, open = NULL,
                              others = character()) {
  pick_js <- function(v) sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})",
                                 ids[["pick"]], encodeString(v, quote = "'"))
  shiny::tagList(
    shiny::div(class = "form-label mb-1", with_tip(
      shiny::span(t("Code lists"), shiny::span(class = "text-muted", " (codelists)")),
      paste(tip, t("A value with no arrow prints as it is."),
            t("Click a column to edit its values and labels.")))),
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mb-1",
      if (length(others)) shiny::div(
        class = "rp-cl-new", style = "min-width: 14rem;",
        shiny::selectizeInput(ids[["new"]], NULL, c("", others), width = "100%",
                              options = list(placeholder = t("A code list for another column...")))),
      .btn(ids[["copy"]], t("Copy..."), class = "btn-sm btn-outline-secondary py-0"),
      .btn(ids[["edit"]], t("All code lists..."), class = "btn-sm btn-link py-0")),
    if (!length(lines)) shiny::p(class = "small text-muted mb-1",
                                 t("None of these columns has a code list: their values are as the data has them.")) else
      shiny::tags$table(
        class = "table table-sm table-hover small mb-1 rp-cl-list",
        shiny::tags$tbody(lapply(lines, function(l) {
          on <- identical(l$variable, open)
          shiny::tags$tr(
            class = if (on) "table-active", style = "cursor: pointer;",
            title = t("Click to edit this code list"),
            onclick = pick_js(l$variable),
            shiny::tags$td(class = "fw-semibold text-nowrap",
                           shiny::span(class = "text-muted me-1", if (on) "\u25be" else "\u25b8"),
                           l$variable),
            shiny::tags$td(l$text))
        }))),
    lapply(names(missing), function(v) shiny::div(
      class = "alert alert-warning small py-1 px-2 mb-1 d-flex flex-wrap gap-2 align-items-center",
      shiny::span(sprintf(t("%s: the data has values its code list does not (the program would stop): %s"),
                          v, paste(missing[[v]], collapse = ", "))),
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-primary py-0",
        onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})",
                          ids[["add"]], encodeString(v, quote = "'")),
        t("Add them to the list")))))
}

# The open column's grid: its values, labels and order, written back to
# the report's code list of that column as they are edited
.codelist_detail_ui <- function(variable, t, ids) {
  shiny::div(
    class = "border rounded p-2 mb-2 bg-body-tertiary",
    shiny::div(
      class = "d-flex align-items-center gap-2",
      shiny::strong(class = "small", sprintf(t("Code list of %s"), variable)),
      shiny::div(class = "ms-auto",
                 .btn(ids[["close"]], t("Close"), class = "btn-sm btn-outline-secondary py-0"))),
    shiny::div(class = "small text-muted mb-1",
               t("value: as the data has it; label: what it prints as (blank: itself); order: its place")),
    rhandsontable::rHandsontableOutput(ids[["hot"]]))
}

# Its server: `vars()` the place's columns, `data()` its data (or NULL),
# `title()` the dialog's; `dialog` a .codelist_dialog_server() (copy, and
# every code list of the report).  Adding the values a list lacks adds them
# to this report's list, each printing as itself.  Draws the open column's
# grid (`ids[["detail"]]`, `ids[["hot"]]`).  Gives list(missing = the
# reactive of those values, open = the open column's reactiveVal).
.codelist_part_server <- function(input, output, rv, report, ids, vars, data, title,
                                  dialog, t, notify, guarded, bump, has_study,
                                  read_grid, grids_drawn) {
  missing <- shiny::reactive({
    shiny::req(has_study(), report())
    .codelist_missing(sheet_rows(rv$p, "codelists", report()), data())
  })
  open <- shiny::reactiveVal(NULL)
  # another report: nothing open
  shiny::observeEvent(report(), open(NULL), ignoreNULL = FALSE)
  # a click on the open one closes it
  shiny::observeEvent(input[[ids[["pick"]]]], {
    v <- input[[ids[["pick"]]]]
    open(if (identical(v, open())) NULL else v)
  })
  shiny::observeEvent(input[[ids[["new"]]]], {
    v <- input[[ids[["new"]]]]
    if (!.is_blank(v)) open(v)
  })
  shiny::observeEvent(input[[ids[["close"]]]], open(NULL))
  output[[ids[["detail"]]]] <- shiny::renderUI({
    v <- open()
    shiny::req(has_study(), report(), v)
    .codelist_detail_ui(v, t, ids)
  })
  key <- shiny::reactive(paste(ids[["hot"]], report() %||% "", open() %||% "",
                               rv$ver, sep = "|"))
  output[[ids[["hot"]]]] <- rhandsontable::renderRHandsontable({
    v <- open()
    shiny::req(has_study(), report(), v)
    key()
    grids_drawn()
    d <- sheet_rows(shiny::isolate(rv$p), "codelists", report())
    d <- d[d$variable %in% v, c("value", "label", "order"), drop = FALSE]
    .grid(d, "codelists", key(), list())
  })
  shiny::observeEvent(input[[ids[["hot"]]]], {
    h <- input[[ids[["hot"]]]]
    if (is.null(h$changes$changes) &&
        !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
    if (!identical(h$params$planner_key, key())) return()
    d <- read_grid(h)
    id <- report()
    v <- open()
    if (is.null(d) || is.null(id) || is.null(v)) return()
    d <- .drop_blank_rows(as.data.frame(d, stringsAsFactors = FALSE))
    old <- sheet_rows(rv$p, "codelists", id)
    keep <- old[!old$variable %in% v, , drop = FALSE]
    keep$output_id <- NULL
    mine <- data.frame(variable = rep(v, nrow(d)), stringsAsFactors = FALSE)
    for (cn in setdiff(names(keep), "variable")) {
      mine[[cn]] <- if (cn %in% names(d)) as.character(d[[cn]]) else
        rep(NA_character_, nrow(d))
    }
    # the column's rows where they were (a new list after the others)
    at <- which(old$variable %in% v)
    n_before <- if (length(at)) at[1L] - 1L else nrow(keep)
    rows <- rbind(keep[seq_len(n_before), , drop = FALSE], mine[names(keep)],
                  keep[setdiff(seq_len(nrow(keep)), seq_len(n_before)), , drop = FALSE])
    guarded(rv$p <- set_sheet_rows(rv$p, "codelists", id, rows))
  })
  shiny::observeEvent(input[[ids[["add"]]]], {
    v <- input[[ids[["add"]]]]
    miss <- missing()[[v]]
    shiny::req(length(miss), report())
    p2 <- guarded(.codelist_add_values(rv$p, report(), v, miss))
    if (is.null(p2)) return()
    rv$p <- p2
    bump()
    open(v)
    notify(sprintf(t("%s's code list of %s: %s added, each printing as itself (edit their text in the grid)."),
                   report(), v, paste(miss, collapse = ", ")))
  })
  shiny::observeEvent(input[[ids[["edit"]]]], {
    shiny::req(has_study(), report())
    dialog$open(vars(), title())
  })
  shiny::observeEvent(input[[ids[["copy"]]]], {
    shiny::req(has_study(), report())
    dialog$copy(vars(), title())
  })
  list(missing = missing, open = open)
}

# The ids of a place's code lists part, from its stem (`adata_cl`) and
# its dialog's prefix (`cl21`)
.codelist_part_ids <- function(stem, prefix, edit = paste0(prefix, "_open"),
                               copy = paste0(prefix, "_copy")) {
  c(edit = edit, copy = copy, add = paste0(stem, "_add"),
    pick = paste0(stem, "_pick"), new = paste0(stem, "_new"),
    close = paste0(stem, "_close"), detail = paste0(stem, "_detail"),
    hot = paste0(stem, "_hot"))
}

# The place's columns that have no code list yet (to start one)
.codelist_others <- function(cl, vars) {
  setdiff(vars[!is.na(vars) & nzchar(vars)], cl$variable)
}

# Under an analysis's own filter: it reads the data after the code lists
# are put on, so a listed column is compared with its labels -- said, with
# the report's listed columns and one example (SEX == "Female")
.where_labels_note <- function(cl, t) {
  cl <- cl[!is.na(cl$variable) & !is.na(cl$value), , drop = FALSE]
  if (!nrow(cl)) return(NULL)
  lab <- ifelse(is.na(cl$label), cl$value, cl$label)
  v <- unique(cl$variable)
  # the example: a value its list changes (SEX's F, as "Female")
  i <- which(lab != cl$value)[1L]
  if (is.na(i)) i <- 1L
  shiny::div(
    class = "form-text small mt-n1 mb-2",
    sprintf(t("A column with a code list (%s) is compared with its labels here, as the ARD has them: e.g. %s, not %s."),
            paste(v, collapse = ", "),
            sprintf("%s == %s", cl$variable[i], encodeString(lab[i], quote = "\"")),
            sprintf("%s == %s", cl$variable[i], encodeString(cl$value[i], quote = "\""))))
}
