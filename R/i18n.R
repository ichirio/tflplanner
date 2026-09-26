# The app speaks English, or Japanese.  Every text is written in English in
# the code and looked up in inst/i18n/strings.csv (columns `en`, `ja`); a
# text the table lacks stays English.

.i18n <- new.env()

.strings <- function() {
  if (is.null(.i18n$table)) {
    f <- system.file("i18n", "strings.csv", package = "tflplanner")
    .i18n$table <- if (nzchar(f)) {
      utils::read.csv(f, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                      na.strings = character())
    } else {
      data.frame(en = character(), ja = character())
    }
  }
  .i18n$table
}

#' The app's languages
#'
#' `tr()` translates an English text of the app; `tflplanner_language()` is
#' the language set with [setup_tflplanner()] (English by default).
#'
#' @param x English text.
#' @param lang `"en"` or `"ja"`.
#' @return The text in `lang`.
#' @export
tr <- function(x, lang = tflplanner_language()) {
  if (identical(lang, "en") || !length(x)) return(x)
  t <- .strings()
  if (!lang %in% names(t)) return(x)
  i <- match(x, t$en)
  out <- t[[lang]][i]
  ifelse(is.na(i) | !nzchar(out), x, out)
}

#' @rdname tr
#' @export
tflplanner_language <- function() {
  l <- tryCatch(tflplanner_config()$language, error = function(e) NULL)
  if (is.null(l) || !l %in% app_languages()) "en" else l
}

#' @rdname tr
#' @export
app_languages <- function() c(English = "en", "\u65e5\u672c\u8a9e" = "ja")
