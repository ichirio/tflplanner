# The example TOC workbooks of inst/extdata, made from the sample study
# (SAMPLE-01) as this tflplanner makes it, for trying "Take in a TOC":
#
#   SAMPLE-01_TOC.xlsx          the sample's reports as a company TOC: a
#                               cover line, the header on row 2 (Shell
#                               Number, Phase, Topline, Title1-3,
#                               Population, Footnote1-2), section heading
#                               rows, and one shell sheet per report.
#                               Taken into the sample study: no change but
#                               the Topline batch.
#   SAMPLE-01_TOC_changed.xlsx  the same with two rows changed: T-14-1-2's
#                               title, a footnote for T-14-2-1 ("Changed").
#
# Run it from the package folder:  Rscript data-raw/make-toc-sample.R
# (it makes the sample study in a temporary home of its own).
out <- tempfile("tocsample")
Sys.setenv(R_USER_CONFIG_DIR = file.path(out, "config"), R_USER_DATA_DIR = file.path(out, "data"),
           R_USER_CACHE_DIR = file.path(out, "cache"))
options(tflplanner.home = file.path(out, "home"), tflplanner.check_updates = FALSE)
pkgload::load_all(".", quiet = TRUE)
suppressMessages(setup_tflplanner(studies_root = file.path(out, "studies"), language = "en",
                                  sample = TRUE))
p <- open_study("SAMPLE-01")$planner
o <- p$outputs
tk <- p$sheets$tokens
tok <- function(id, name) {
  v <- tk$value[!is.na(tk$output_id) & tk$output_id == id & tk$name == name]
  if (length(v)) v[1L] else NA_character_
}
type <- vapply(o$output_id, function(id) report_info(p, id)$type, "")
kind <- c(table = "Table", listing = "Listing", figure = "Figure", user = "Figure")[type]
num <- gsub("-", ".", sub("^[A-Z]+-", "", o$output_id))
section_of <- function(n) {
  if (startsWith(n, "14.0")) "14.0 Study information" else
    if (startsWith(n, "14.1")) "14.1 Demographic and baseline characteristics" else
      if (startsWith(n, "14.2")) "14.2 Efficacy" else
        if (startsWith(n, "14.3")) "14.3 Safety" else "16.2 Subject data listings"
}
topline <- c("T-14-1-1", "T-14-2-1", "T-14-3-1")
title <- vapply(o$output_id, tok, "", name = "OUTPUT_TITLE")
pop <- vapply(o$output_id, tok, "", name = "OUTPUT_POPULATION")

rows_of <- function(title, foot) {
  rows <- list(c("SAMPLE-01 Table of contents (made up from tflplanner's sample study)"),
               c("Shell Number", "Phase", "Topline", "Title1", "Title2", "Title3",
                 "Population", "Footnote1", "Footnote2"))
  cur <- ""
  for (i in seq_len(nrow(o))) {
    sec <- section_of(num[i])
    if (!identical(sec, cur)) {
      rows[[length(rows) + 1L]] <- c(NA, NA, NA, sec, NA, NA, NA, NA, NA)
      cur <- sec
    }
    rows[[length(rows) + 1L]] <- c(
      o$output_id[i], NA, if (o$output_id[i] %in% topline) "Y" else NA,
      paste(kind[[i]], num[i]), title[[i]], NA, pop[[i]], foot[i], NA)
  }
  rows
}
shell <- function(id) list(c(id, "Shell"),
                           c("", "Drug X", "Control"),
                           c("n", "xx", "xx"))
book <- function(rows, file) {
  sheets <- c(list(TOC = rows), stats::setNames(lapply(o$output_id, shell), o$output_id))
  d <- lapply(sheets, function(rs) {
    w <- max(lengths(rs))
    as.data.frame(do.call(rbind, lapply(rs, function(r) c(r, rep(NA, w - length(r))))),
                  stringsAsFactors = FALSE)
  })
  writexl::write_xlsx(d, file.path("inst", "extdata", file), col_names = FALSE)
}
dir.create(file.path("inst", "extdata"), showWarnings = FALSE)
none <- rep(NA_character_, nrow(o))
book(rows_of(title, none), "SAMPLE-01_TOC.xlsx")
title2 <- title
title2[o$output_id == "T-14-1-2"] <- "Subject Disposition (All Randomised Subjects)"
foot2 <- none
foot2[o$output_id == "T-14-2-1"] <- "LOCF: last observation carried forward."
book(rows_of(title2, foot2), "SAMPLE-01_TOC_changed.xlsx")
unlink(out, recursive = TRUE)
