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
  # the user's choice, else the company standards', else English
  if (is.null(l)) l <- tryCatch(.std_setting("language", "en"),
                                error = function(e) "en")
  if (!l %in% app_languages()) "en" else l
}

#' @rdname tr
#' @export
app_languages <- function() c(English = "en", "\u65e5\u672c\u8a9e" = "ja")

# An error's message on screen: R's and tflplanner's errors are written in
# English; the common ones get a lead in the app's language, the message
# itself (the names in it) kept after it.  A message no pattern knows, and
# the English app, show the message alone.
.error_leads <- list(
  c("^A study id is letters", "The study ID cannot be used"),
  c("^Study '.+' is already registered", "The study is already registered"),
  c("^A folder '.+' is already there", "A folder of that name is already there"),
  c("^Not a study folder", "This is not a study folder"),
  c("^No study '.+': not registered", "There is no such study"),
  c("^Report '.+' is already on the list", "A report of that ID is already on the list"),
  c("^Report '.+' already exists", "A report of that ID is already on the list"),
  c("^An output_id is a single non-blank string", "The report ID is empty"),
  c("^output_id '.+' has a character a file name cannot hold", "The report ID has a character a file name cannot hold"),
  c("^A code list is an [.]xlsx or a [.]csv file", "A code list is an .xlsx or a .csv file"),
  c("^The code list has no column", "The code list lacks a column"),
  c("^Reading [.].+ files needs the .+ package", "A package is needed to read this file"),
  c("^No file ", "The file is not there"),
  c("^The study has no analyses in its ARD definition", "The ARD definition has no analyses"),
  c("^The study ARD has no rows for .+: make its ARD first", "Make the report's ARD first"),
  c("^There is an ARD function .+ already", "An ARD function of that name is already there"),
  c("^An ARD function's name starts with ard_", "An ARD function's name starts with ard_"),
  c("^The company standards' folder cannot be written", "The company standards' folder cannot be written"),
  c("^The company has no ARD function", "The company standards have no such ARD function"),
  c("^No ARD function ", "There is no such ARD function"),
  c("^No dataset .+ in the ARD definition", "The ARD definition has no such dataset"),
  c("^The report has no code yet", "The report has no code yet"),
  c("^The definition files were changed outside tflplanner", "The definition files were changed"),
  c("^The definition files do not read", "The definition files do not read"))

.error_view <- function(msg, t = identity) {
  for (p in .error_leads) {
    if (grepl(p[1L], msg)) {
      lead <- t(p[2L])
      return(if (identical(lead, p[2L])) msg else paste0(lead, "\uff1a", msg))
    }
  }
  msg
}
