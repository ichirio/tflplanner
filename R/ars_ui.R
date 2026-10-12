# ============================================================================
#  The CDISC ARS export dialog: what the ARS holds, its checks and what it
#  leaves out, before the files are written (R/study.R's .ars_build()).
#  A row's report is a link: it goes there as a Review item does.
# ============================================================================

# The dialog's body: one line of facts, one of counts, the table.
.ars_summary_ui <- function(b, t) {
  a <- b$ars
  facts <- sprintf(t("%d analyses \u00b7 %d outputs \u00b7 %d analysis sets \u00b7 %d data subsets"),
                   length(a$analyses), length(a$outputs),
                   length(a$analysisSets), length(a$dataSubsets))
  pr <- b$problems
  n_err <- sum(pr$kind == "check")
  n_out <- sum(pr$kind == "not in ARS")
  counts <- if (!nrow(pr)) {
    shiny::p(class = "small text-success mb-1", t("No problems found."))
  } else {
    shiny::p(class = "small mb-1",
      shiny::span(class = if (n_err) "text-danger" else "text-muted",
                  sprintf(t("%d errors"), n_err)), " \u00b7 ",
      shiny::span(class = "text-secondary",
                  sprintf(t("%d not in the ARS"), n_out)))
  }
  shiny::tagList(
    shiny::p(class = "small text-muted mb-1", facts),
    counts,
    if (nrow(pr)) shiny::div(
      style = "max-height: 50vh; overflow: auto;",
      .ars_problem_table(pr, t)))
}

# One row a problem and its report: the same problem on many analyses of a
# report (a blank purpose on each) is one row, with how many; the link goes
# to the first of them.  ars_check.csv keeps every row.
.ars_problem_groups <- function(pr) {
  key <- paste(pr$kind, pr$output_id, pr$item, pr$note, sep = "\r")
  first <- !duplicated(key)
  out <- pr[first, , drop = FALSE]
  out$n <- as.integer(table(factor(key, levels = unique(key))))
  out$row <- which(first)
  out
}

.ars_problem_table <- function(pr, t) {
  kind <- c(check = t("an error"), "not in ARS" = t("not in the ARS"))
  g <- .ars_problem_groups(pr)
  rows <- lapply(seq_len(nrow(g)), function(k) {
    r <- g[k, ]
    i <- r$row
    id <- if (is.na(r$output_id)) "" else r$output_id
    shiny::tags$tr(
      shiny::tags$td(class = if (r$kind == "check") "text-danger" else "text-secondary",
                     kind[[r$kind]]),
      shiny::tags$td(shiny::tags$a(
        href = "#", title = t("Go to it"),
        onclick = sprintf("Shiny.setInputValue('ars_goto', %d, {priority: 'event'}); return false;", i),
        if (nzchar(id)) id else r$where)),
      shiny::tags$td(r$item, if (r$n > 1L) shiny::span(class = "text-muted",
                                                         sprintf(" \u00d7%d", r$n))),
      shiny::tags$td(r$note))
  })
  shiny::tags$table(
    class = "table table-sm small mb-0",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(""), shiny::tags$th(t("Report")),
      shiny::tags$th(t("Item")), shiny::tags$th(t("Problem")))),
    shiny::tags$tbody(rows))
}

# A problem row as a Review item, for review_jump(): the sheet the item is
# in and the row's key there.  An analysis is An_<output>_<analysis> in a
# check and "<output> / <analysis>" when it is left out; a population or an
# analysis data names its sheet in `item` (populations$derive).
.ars_jump_row <- function(r) {
  id <- r$output_id
  item <- r$item %||% ""
  sheet <- if (grepl("$", item, fixed = TRUE)) sub("\\$.*$", "", item) else
    if (item %in% table_sheets()) item else "analyses"
  key <- switch(sheet,
    populations = r$where,
    analysis_data = sub(":.*$", "", r$note),
    analyses = if (is.na(id)) "" else {
      w <- r$where
      if (grepl(" / ", w, fixed = TRUE)) sub("^.*? / ", "", w)
      else sub(paste0("^.*An_", id, "_"), "", w)
    },
    "")
  list(output_id = id, sheet = sheet, area = "ard", row = key,
       field = if (identical(sheet, "analyses") && !grepl("$", item, fixed = TRUE)) item else NA)
}
