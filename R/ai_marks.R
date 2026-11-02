# A report copied into the study from the AI's draft: its mark in the Report
# list (R/srv_figure_page.R), and before an official run (.ai_run_warnings()).


# Each of `ids`: "" (not the AI's), "adopted" (adopted from its draft,
# ai/adopted.csv), "newer" (its draft changed since: adopting again would
# change it)
.ai_adopted_marks <- function(p, ids) {
  out <- stats::setNames(rep("", length(ids)), ids)
  f <- if (!is.null(p)) file.path(p, "ai", "adopted.csv")
  if (is.null(f) || !file.exists(f)) return(out)
  a <- tryCatch(utils::read.csv(f, colClasses = "character", stringsAsFactors = FALSE,
                                fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
  if (is.null(a) || !all(c("output_id", "md5") %in% names(a))) return(out)
  a <- a[!duplicated(a$output_id, fromLast = TRUE), , drop = FALSE]
  k <- ids[ids %in% a$output_id]
  out[k] <- "adopted"
  drafts <- file.path(p, "ai", "spec", "outputs", paste0(k, ".yml"))
  have <- file.exists(drafts)
  now <- unname(tools::md5sum(drafts[have]))
  out[k[have][now != a$md5[match(k[have], a$output_id)]]] <- "newer"
  out
}

.ai_mark_html <- function(marks, t) {
  marks <- unname(marks)
  ifelse(marks == "", "", ifelse(
    marks == "newer",
    sprintf(' <span class="badge text-bg-warning" title="%s">AI</span>',
            htmltools::htmlEscape(t("Adopted from the AI's draft; the draft changed since (step 4)"), TRUE)),
    sprintf(' <span class="badge text-bg-light border" title="%s">AI</span>',
            htmltools::htmlEscape(t("Adopted from the AI's draft"), TRUE))))
}
