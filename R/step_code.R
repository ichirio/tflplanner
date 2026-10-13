# A report's steps of its own code (the user-program design, 2026-10-12;
# C:/Yrepo/_archive/user-program-per-step-design-2026-10-12.md).  A report
# is a table, a listing or a figure; its data step and its content step are
# each the definition (the GUI) or the user's own R -- the report sheet's
# data_code and content_code (tflspec): a step whose code is not blank is
# that code.  The code follows rules the program relies on
# (tflspec::tfl_check_code()): the datasets it reads are the report's own
# (its row of the report list, `datasets`; the program reads them first, by
# their names in lower case), its ARD is `ard` when the report reads one,
# and the data step leaves `data` (a figure: `df`), the content step
# `content`.

#' A report's own code for a step
#'
#' A report's data step and its content step are each the definition (the
#' GUI) or the report's own R: `step_code()` is that code (`NA`: the
#' definition makes the step), kept in the report sheet's `data_code` /
#' `content_code` (see [tflspec::tfl_check_code()] for the rules the code
#' follows).  `report_datasets()` are the datasets the report registers --
#' the ones a step of its own code reads (the program reads them first, by
#' their names in lower case).
#'
#' @param x A `tflplanner`.
#' @param output_id A report.
#' @param step `"data"` or `"content"`.
#' @param code The code, or `NA` / `""` (the definition makes the step).
#' @param datasets Dataset names (`"ADSL"`, `"ADVS"`).
#' @return `step_code()`: the code or `NA`; `report_datasets()`: the
#'   names; the setters: `x`.
#' @examples
#' p <- add_output(new_planner(), "F-1", type = "figure")
#' p <- set_report_datasets(p, "F-1", c("ADSL", "ADVS"))
#' p <- set_step_code(p, "F-1", "content",
#'                    "content <- ggplot2::ggplot(advs) + ggplot2::geom_point()")
#' step_code(p, "F-1", "content")
#' report_datasets(p, "F-1")
#' @export
step_code <- function(x, output_id, step = c("data", "content")) {
  step <- match.arg(step)
  r <- x$sheets$report
  cn <- paste0(step, "_code")
  v <- if (!is.null(r) && cn %in% names(r)) {
    r[[cn]][match(output_id, r$output_id)]
  } else NA_character_
  v <- v %||% NA_character_
  if (is.na(v) || !nzchar(trimws(v))) NA_character_ else v
}

#' @rdname step_code
#' @export
set_step_code <- function(x, output_id, step = c("data", "content"), code) {
  step <- match.arg(step)
  code <- as.character(code %||% NA_character_)[1L]
  if (!is.na(code)) {
    code <- sub("\n+$", "", gsub("\r\n?", "\n", code))
    if (!nzchar(trimws(code))) code <- NA_character_
  }
  r <- x$sheets$report
  cn <- paste0(step, "_code")
  if (!cn %in% names(r)) r[[cn]] <- rep(NA_character_, nrow(r))
  i <- which(!is.na(r$output_id) & r$output_id == output_id)
  if (!length(i)) {
    if (is.na(code)) return(x)
    r[nrow(r) + 1L, ] <- NA_character_
    i <- nrow(r)
    r$output_id[i] <- output_id
  }
  r[[cn]][i] <- code
  x$sheets$report <- r
  x
}

#' @rdname step_code
#' @export
report_datasets <- function(x, output_id) {
  .split_bar(x$outputs$datasets[match(output_id, x$outputs$output_id)] %||% NA)
}

#' @rdname step_code
#' @export
set_report_datasets <- function(x, output_id, datasets) {
  i <- match(output_id, x$outputs$output_id)
  if (is.na(i)) stop("No report ", output_id, ".", call. = FALSE)
  ds <- unique(toupper(trimws(as.character(datasets))))
  ds <- ds[!is.na(ds) & nzchar(ds)]
  x$outputs$datasets[i] <- if (length(ds)) paste(ds, collapse = " | ") else NA_character_
  x
}

# Is the step the report's own code?
.is_code_step <- function(x, output_id, step) !is.na(step_code(x, output_id, step))

# What a step of its own code leaves
.step_leaves <- function(type, step) {
  if (identical(step, "content")) "content" else if (identical(type, "figure")) "df" else "data"
}

# Does a report's own code have its ARD (`ard`) in reach?  A figure's as its
# ARD source says.  A table's data step of its own code makes its data
# itself (its ARD, when it has one, is what its definition's data step
# reads); a listing has none.
.code_reads_ard <- function(x, output_id, type = report_info(x, output_id)$type) {
  identical(type, "figure") && !identical(.fig_ard_source(x, output_id)$kind, "none")
}

# A step's code checked against the rules (tflspec::tfl_check_code()): the
# review's rows (U01-U05)
.check_step_code <- function(x, output_id, step) {
  code <- step_code(x, output_id, step)
  if (is.na(code)) return(NULL)
  tflspec::tfl_check_code(code, step, report_info(x, output_id)$type,
                          datasets = report_datasets(x, output_id),
                          catalog = x$ard$datasets$dataset,
                          ard = .code_reads_ard(x, output_id),
                          output_id = output_id)
}

# The review's rows of every report's own code (U01-U05)
.rules_step_code <- function(x) {
  ids <- x$outputs$output_id
  rows <- list()
  for (id in ids) for (step in c("data", "content")) {
    r <- tryCatch(.check_step_code(x, id, step), error = function(e) NULL)
    if (NROW(r)) rows[[length(rows) + 1L]] <- r
  }
  .review_bind(rows)
}

# ---- the screen of a step of the report's own code ------------------------

# The rules a step of the report's own code follows (the help the switch
# opens; the ja guide says the same).  What the review checks of them:
# tflspec::tfl_check_code() (U01-U05).
.code_rules <- c(
  "Register the datasets it reads (above the data step's code). The program reads them first, by their names in lower case (adsl, advs); the code does not read files itself (readRDS(), read_xpt() ...).",
  "Its ARD through the report: a table's ARD definition or the ARD taken in, a figure's ARD source (step 1). This report's ARD rows are `ard`; the code does not read ard.rds itself.",
  "Each step leaves what the next one reads: the data step `data` (a figure: `df`; a table may leave `ard`, normalized for it), the content step `content` -- a table plan, a data frame, rtftable pages, a ggplot or rtfplot, or a list of them.",
  "Code inside the definition stays there: a table's or a listing's rework, a figure's code step or code layer. R program makes the whole step your code.",
  "No library(): the study's setup attaches its packages (the figures': ggplot2, patchwork, dplyr and those of its designs); call others as pkg::fn().",
  "No side effects: it writes no files and does not change the session (setwd(), options()); the program writes the report.",
  "One report: it does not read another report's outputs. Another report's ARD comes through the ARD source.")

# The rules, as the help dialog shows them
.code_rules_ui <- function(t = identity) {
  # (`name`: code)
  coded <- function(x) shiny::HTML(gsub("`([^`]+)`", "<code>\\1</code>", htmltools::htmlEscape(x)))
  shiny::tagList(
    shiny::tags$ol(class = "small", lapply(.code_rules, function(r) shiny::tags$li(coded(t(r))))),
    shiny::p(class = "small text-muted",
             coded(t("The code is checked as you write it (under the code) and in Review: a dataset it reads that is not registered, nothing left for the next step, files read or packages attached, `ard` where the report reads none (rules U01-U05)."))))
}

# A step's code: its editor and its check (left), the program and its run
# (right).  `prefix` dc (the data step) or cc (the content step).
code_step_ui <- function(prefix, t = identity, lang = "en") {
  step <- if (identical(prefix, "dc")) "data" else "content"
  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
    bslib::card(
      bslib::card_header(with_tip(
        if (identical(step, "data")) t("Your code: the data") else t("Your code: the content"),
        if (identical(step, "data")) t("It leaves `data` (a figure: `df`). The rules: at the top of this step.")
        else t("It leaves `content`. The rules: at the top of this step."))),
      # taller to the window's foot (.rp-code-step, the app's style)
      shiny::div(class = "rp-code rp-code-step",
                 shiny::textAreaInput(paste0(prefix, "_code"), NULL, rows = 26,
                                      width = "100%", resize = "vertical",
                                      placeholder = if (identical(step, "data"))
                                        "df <- advs |> dplyr::filter(PARAMCD == \"SYSBP\")" else
                                          "content <- ggplot2::ggplot(df) + ggplot2::geom_line()")),
      shiny::uiOutput(paste0(prefix, "_check"))),
    result_tabs_ui(
      paste0(prefix, "_right"), lang = lang, selected = "result",
      spec = shiny::p(class = "small text-muted",
                      t("This step is its code, on the left: the definition does not make it.")),
      code = code_view(paste0("program_", prefix), lang),
      result = shiny::tagList(
        shiny::div(class = "d-flex gap-2 align-items-center mb-1",
                   .btn(paste0(prefix, "_run"), t("Run the code"), class = "btn-sm btn-outline-primary"),
                   help_tip(t("Runs the report's program up to its report in a fresh R process from the study folder, as its official run will, and shows what it left: the first table page, the first figure."))),
        shiny::div(style = "overflow-x: auto;", shiny::uiOutput(paste0(prefix, "_result"))))))
}

# ---- the program's lines around a step of the report's own code ----------

# The datasets the report registers, read; and its ARD as `ard` when it
# reads one
.code_inputs <- function(x, output_id) {
  ds <- report_datasets(x, output_id)
  ard <- if (.code_reads_ard(x, output_id)) .code_ard_lines(x, output_id)
  c(if (length(ds)) c(
      paste0("# the report's data: ", paste(ds, collapse = ", "), " (data catalog)"),
      unlist(lapply(ds, function(d) .read_dataset_lines(x, d))), ""),
    ard)
}

# this report's ARD rows as `ard`: a figure's as a designed figure's
# (its own, a table's, or taken in -- with the definition it was made
# from, which the report records); else taken in, or the study ARD's
.code_ard_lines <- function(x, output_id) {
  if (identical(report_info(x, output_id)$type, "figure")) {
    a <- .fig_ard_lines(x, output_id)
    return(if (length(a)) a)
  }
  imp <- .ard_import_of(x, output_id)
  src <- .fig_ard_source(x, output_id)
  of <- if (identical(src$kind, "table")) src$id else output_id
  a <- if (!is.null(imp)) .imported_ard_code(imp, output_id) else
    .fill_template("table_data", x, of)
  if (is.na(a)) return(NULL)
  c(paste0("# this report's ARD (`ard`)",
           if (!identical(of, output_id)) paste0(": ", of, "'s")),
    .code_block(a), "")
}

# A step of the report's own code, as the program runs it: its code, then
# the check that it left what the next step reads
.code_step_lines <- function(x, output_id, step) {
  code <- step_code(x, output_id, step)
  leaves <- .step_leaves(report_info(x, output_id)$type, step)
  # (a figure's content: drawn at its size on the page first)
  sized <- if (identical(step, "content") &&
               identical(report_info(x, output_id)$type, "figure")) {
    utils::tail(.fig_sized_lines(x, output_id, "content"), -1L)
  }
  c(paste0("# your code: leaves `", leaves, "`"),
    .code_block(code),
    "",
    sized,
    if (identical(step, "content")) "content <- report_content(content)" else
      sprintf("%s <- report_data(%s, \"%s\")", leaves, leaves, leaves))
}

# ---- migration: "user" is no longer a type --------------------------------

# Before tflspec reads the workbooks: a report sheet that says `type = user`
# is read from a copy that says table / listing / figure (from the report's
# ID), and the reports are named for .migrate_step_code().  The files are
# not changed: the next save writes them as they now are.
.old_user_type_copies <- function(path) {
  ids <- character()
  out <- path
  for (k in seq_along(path)) {
    f <- path[k]
    if (!"report" %in% readxl::excel_sheets(f)) next
    d <- .read_sheet_text(f, "report")
    if (!"type" %in% names(d) || !"output_id" %in% names(d)) next
    i <- which(tolower(trimws(d$type)) %in% "user")
    if (!length(i)) next
    ids <- c(ids, d$output_id[i])
    wb <- openxlsx::loadWorkbook(f)
    j <- match("type", names(d))
    for (r in i) {
      openxlsx::writeData(wb, "report", .type_of_id(d$output_id[r] %||% ""),
                          startCol = j, startRow = r + 1L, colNames = FALSE)
    }
    tmp <- file.path(tempfile("tflplanner-user-"), basename(f))
    dir.create(dirname(tmp))
    openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
    out[k] <- tmp
  }
  structure(out, user = ids[!is.na(ids)])
}

# a report's type from its ID: F... a figure, L... a listing, else a table
.type_of_id <- function(id) {
  if (grepl("^F", id, ignore.case = TRUE)) "figure" else
    if (grepl("^L", id, ignore.case = TRUE)) "listing" else "table"
}

# The report list's old `data_code` (before 0.0.2.9206), named by report
.old_data_code <- function(d) {
  if (is.null(d) || !"data_code" %in% names(d) || !"output_id" %in% names(d)) {
    return(stats::setNames(character(), character()))
  }
  v <- as.character(d$data_code)
  id <- as.character(d$output_id)
  keep <- !is.na(id) & nzchar(id) & !is.na(v) & nzchar(trimws(v))
  stats::setNames(v[keep], id[keep])
}

# The reports' old code (the report list's data_code) and type `user` as
# steps of their own code.  `old` the old codes by report, `user` the
# reports that said type `user` (when the workbooks were read; a state
# keeps its type), `has_design(id)` whether a figure has a design.
#
# * user: its type from its ID, its code the content step's; its datasets
#   (its figures row's, and the catalog's ones its code names) the
#   report's; its ARD switch the report's ARD source (a figure: own).
# * table: the code made its ARD -- its data step.
# * listing: with columns, the code reworked `data` -- its rework
#   (process_code); without, it made the pages -- its content step.
# * figure written by hand (no design): the content step (`plot` left:
#   checked and given as `content`, as its program did); its datasets the
#   report's.  A designed figure never ran its old code: dropped.
.migrate_step_code <- function(p, old, user = character(),
                               has_design = function(id) !is.null(p$fig_designs[[id]])) {
  r <- p$sheets$report
  user <- union(user, r$output_id[!is.na(r$type) & tolower(r$type) == "user"])
  user <- user[!is.na(user)]
  # a figure with no design: the datasets its figures row names are the
  # report's (what its own code reads)
  f <- p$lf$figures
  for (id in setdiff(unique(stats::na.omit(f$output_id)), user)) {
    if (identical(report_info(p, id)$type, "figure") && !isTRUE(has_design(id))) {
      p <- .move_figures_row(p, id)
    }
  }
  ids <- union(names(old), user)
  if (!length(ids)) return(p)
  for (id in ids) {
    code <- if (id %in% names(old)) old[[id]] else NA_character_
    if (!id %in% p$outputs$output_id) next
    was_user <- id %in% user
    if (was_user) {
      p <- .set_report_type(p, id, .type_of_id(id))
      p <- .move_figures_row(p, id, ard = TRUE)
      if (!is.na(code)) {
        p <- set_step_code(p, id, "content", .content_left(code))
        p <- .register_code_datasets(p, id, code)
      }
      next
    }
    if (is.na(code)) next
    type <- report_info(p, id)$type
    if (identical(type, "table")) {
      p <- set_step_code(p, id, "data", code)
    } else if (identical(type, "listing")) {
      if (nrow(lf_rows(p, "listings", id))) {
        i <- match(id, p$outputs$output_id)
        pc <- p$outputs$process_code[i]
        p$outputs$process_code[i] <- if (is.na(pc)) code else paste(code, pc, sep = "\n\n")
      } else {
        p <- set_step_code(p, id, "content", code)
        p <- .register_code_datasets(p, id, code)
      }
    } else if (identical(type, "figure") && !isTRUE(has_design(id))) {
      p <- .move_figures_row(p, id)
      p <- set_step_code(p, id, "content", .content_left(code))
      p <- .register_code_datasets(p, id, code)
    }
  }
  p
}

# a code that leaves `plot` and not `content`: the figure checks and
# `content <- plot` after it, as the figure's program did
.content_left <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
  leaves <- function(nm) any(grepl(sprintf("(^|[^A-Za-z0-9_.])%s[[:space:]]*(<-|=)[^=]", nm), lines))
  if (leaves("content") || !leaves("plot")) return(code)
  paste(c(sub("\n+$", "", code), "tfl_check(plot)", "content <- plot"), collapse = "\n")
}

# a figures row (a user-code report's or a figure's written by hand): its
# datasets the report's; with `ard`, its ARD switch the report's ARD
# source (own, unless it takes an ARD in or has one); the row dropped
.move_figures_row <- function(p, id, ard = FALSE) {
  f <- lf_rows(p, "figures", id)
  if (!nrow(f)) return(p)
  p <- set_report_datasets(p, id, c(report_datasets(p, id), .split_bar(f$datasets[1L])))
  if (ard && "ard" %in% names(f) && isTRUE(as.logical(f$ard[1L])) &&
      identical(report_info(p, id)$type, "figure") &&
      identical(.fig_ard_source(p, id)$kind, "none")) {
    p <- set_fig_ard_source(p, id, "own")
  }
  d <- p$lf$figures
  p$lf$figures <- d[is.na(d$output_id) | d$output_id != id, , drop = FALSE]
  p
}

# the catalog's datasets a code names, registered as the report's (with no
# catalog in reach -- the workbooks read alone -- the ADaM-like names it
# uses: adsl, advs ...)
.register_code_datasets <- function(p, id, code) {
  catalog <- p$ard$datasets$dataset
  if (!length(stats::na.omit(catalog))) {
    nm <- tryCatch(all.names(parse(text = code)), error = function(e) character())
    catalog <- unique(nm[grepl("^ad[a-z0-9]{2,}$", nm)])
  }
  found <- tryCatch(attr(tflspec::tfl_check_code(code, "content", "table",
                                                  catalog = catalog),
                         "datasets"), error = function(e) character())
  if (!length(found)) return(p)
  set_report_datasets(p, id, c(report_datasets(p, id), found))
}
