# Taking a TOC in by the company's rules: the problems (rule catalog and
# toc_problems()).  Split from toc_rules.R (no code change).

# ---- problems -----------------------------------------------------------------

# The import's rules: level and message (the message is the key of
# inst/i18n/strings/); #288's shape, area "toc"
.toc_rule_catalog <- data.frame(
  rule = c("TOC01", "TOC01b", "TOC02", "TOC03", "TOC04", "TOC04b", "TOC05", "TOC06",
           "TOC07", "TOC08", "TOC09", "TOC10", "TOC10b", "TOC11", "TOC12"),
  level = c("error", "error", "error", "error", "hand", "hand", "hand", "check",
            "check", "check", "check", "check", "check", "check", "check"),
  message = c(
    "No column for %s in %s: no report can be read.",
    "No report ID can be made in %s: no ID column, and the titles do not start with one (Table 14.1.1).",
    "Report ID %s is on more than one row (rows %s).",
    "Report ID %s has a character a file name cannot hold.",
    "Row %s has no report ID: not taken in.",
    "Row %s (%s) has no title: not taken in.",
    "Row %s: phase value %s is not in the rules (%s): not taken in.",
    "%s has no SAP number.",
    "SAP number %s is on more than one report: %s.",
    "No shell sheet for %s.",
    "Shell sheet %s is linked to no report.",
    "%s is flagged for %s but is not on the sheet %s.",
    "%s is on the sheet %s but is not flagged for %s.",
    "Sheets %s look like the TOC in %s: %s was taken (more rows).",
    "The rule set names %s, but the TOC has no column for it: skipped."),
  stringsAsFactors = FALSE)

.toc_problem <- function(rule, output_id = NA_character_, row = "", field = "",
                         args = character(), sheet = "") {
  k <- match(rule, .toc_rule_catalog$rule)
  tpl <- .toc_rule_catalog$message[k]
  args <- as.character(args)
  d <- data.frame(output_id = as.character(output_id),
                  level = .toc_rule_catalog$level[k], area = "toc",
                  sheet = sheet, row = as.character(row), field = field,
                  message = do.call(sprintf, c(list(tpl), as.list(args))),
                  template = tpl, hint = NA_character_, rule = sub("b$", "", rule),
                  draft = TRUE, stringsAsFactors = FALSE)
  d$args <- list(args)
  d
}

.toc_no_problems <- function() {
  d <- .toc_problem("TOC08", "x", args = "x")[0L, , drop = FALSE]
  d
}

#' @rdname toc_import_read
#' @param x What `toc_import_read()` returned.
#' @param lang The language of the messages (`"en"`, `"ja"`).
#' @return `toc_problems()`: the problems as rows of the review's shape
#'   (#288: `output_id`, `level` -- `error`, `check`, `hand` --, `area`
#'   `"toc"`, `sheet`, `row`, `field`, `message`, `hint`, `rule`, `draft`),
#'   errors first.
#' @export
toc_problems <- function(x, lang = "en") {
  p <- x$problems
  if (!nrow(p) || identical(lang, "en")) return(p)
  for (i in seq_len(nrow(p))) {
    loc <- tr(p$template[i], lang)
    m <- tryCatch(do.call(sprintf, c(list(loc), as.list(p$args[[i]]))),
                  error = function(e) NA_character_)
    if (!is.na(m)) p$message[i] <- m
  }
  p
}
