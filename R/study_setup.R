# programs/study_setup.R: what every program of the study starts with.
#
#   programs/ard/<id>.R  -> programs/ard/ard_setup.R    -> programs/study_setup.R
#   programs/tfl/<id>.R  -> programs/tfl/report_setup.R -> programs/study_setup.R
#   figure programs      -> programs/tfl/fig_setup.R    -> programs/study_setup.R
#
# One file, three parts, run in this order (a later part wins):
#
#   1  the company standard  copied from the standards (sheet setup_code)
#                            when the file is made; never rewritten after
#   2  the study             written by tflplanner on every save: the
#                            study's folders (study_layout()) and what the
#                            study is (study.yml)
#   3  the user's            the study's own code; never touched
#
# A marker line opens each part.  Saving rewrites part 2 only, so parts 1
# and 3 stay byte for byte.  Part 2 carries a checksum, as a generated
# program does: one edited by hand is put in programs/.edited/ before
# part 2 is written again.

.study_setup_file <- "study_setup.R"

# programs/study_setup.R, relative to the study folder: next to the
# folders of the ARD and report programs
.study_setup_path <- function() {
  file.path(dirname(study_layout()[["programs_ard"]]), .study_setup_file)
}

# the line a setup file starts with
.source_study_setup <- function() {
  sprintf("source(%s)", encodeString(.study_setup_path(), quote = "\""))
}

.setup_marker <- c(
  standard = "^# ==== tflplanner: company standard",
  study = "^# ==== tflplanner: study",
  user = "^# ==== your study")

.setup_marker_standard <- function(date = Sys.Date(),
                                   about = company_standards()$about) {
  v <- function(k) {
    x <- about$value[match(k, about$key)]
    if (length(x) && !is.na(x) && nzchar(x)) x else NA_character_
  }
  std <- paste(stats::na.omit(c(v("name"), v("version"))), collapse = " ")
  if (!nzchar(std)) std <- "built-in"
  upd <- v("updated")
  paste0("# ==== tflplanner: company standard (copied ", format(date, "%Y-%m-%d"),
         ", standards ", std, if (!is.na(upd)) paste0(" of ", upd), ") ====")
}

.setup_marker_study <-
  "# ==== tflplanner: study (written by tflplanner -- do not edit) ===="
.setup_marker_user <- "# ==== your study: edit freely below ===="

#' The company's setup code
#'
#' The code that opens every study's `programs/study_setup.R` (its part
#' 1): the company standards' sheet `setup_code`, one line of R a row
#' ([company_standards()]), with `{STUDY_ID}` filled in.  A new study gets
#' a copy; a later change to the standards does not change a study made
#' before.
#'
#' @param study_id The study's id (`{STUDY_ID}`); `NA` leaves it.
#' @param standards The standards ([company_standards()]).
#' @return The code, one element per line.
#' @export
setup_code <- function(study_id = NA, standards = company_standards()) {
  d <- standards$setup_code
  code <- if (is.null(d)) character() else d$code
  code[is.na(code)] <- ""
  if (!is.na(study_id)) code <- gsub("{STUDY_ID}", study_id, code, fixed = TRUE)
  code
}

# The setup code of a standards workbook, checked: Excel's curly quotes
# (AutoCorrect turns " and ' into them) are not R's, and the lines must
# parse together.  `code` as read, a line a row (row 1 under the header).
.check_setup_code <- function(code) {
  code[is.na(code)] <- ""
  smart <- "[\u201c\u201d\u2018\u2019]"
  bad <- grep(smart, code)
  if (length(bad)) {
    stop("Standards sheet `setup_code`: curly quotes (\u201c \u201d \u2018 \u2019) ",
         "on line(s) ", paste(bad, collapse = ", "),
         " (Excel row(s) ", paste(bad + 1L, collapse = ", "), "): ",
         "R needs straight quotes (\" '). Retype them, or turn off ",
         "Excel's AutoCorrect \"smart quotes\".", call. = FALSE)
  }
  txt <- gsub("{STUDY_ID}", "STUDYID", code, fixed = TRUE)
  tryCatch(parse(text = txt, keep.source = FALSE), error = function(e) {
    stop("Standards sheet `setup_code` is not R code that runs: ",
         sub("^<text>:", "line ", conditionMessage(e)), call. = FALSE)
  })
  invisible(code)
}

# part 2: the study's folders and what the study is, as R variables
.study_setup_part2 <- function(meta) {
  lay <- study_layout()
  val <- function(v) {
    v <- as.character(v %||% NA_character_)
    if (length(v) != 1L || is.na(v)) "NA_character_" else
      encodeString(enc2utf8(v), quote = "\"")
  }
  nm <- function(x) formatC(x, width = -max(nchar(x)))
  paths <- paste0("path_", names(lay))
  ids <- paste0("study_", sub("^study_", "", .study_fields))
  code <- c(
    "# The study's folders, relative to the study folder (the programs run",
    "# from it), and what the study is (study.yml).  tflplanner writes this",
    "# part again on every save: change the study in the app, and put code",
    "# of your own in the part below.",
    paste(nm(paths), "<-", vapply(lay, val, "")),
    "",
    paste(nm(ids), "<-", vapply(.study_fields, function(k) val(meta[[k]]), "")),
    "")
  c(.setup_marker_study,
    paste("#  Checksum   :", .body_hash(code)),
    code)
}

# a study_setup.R split into its parts (each a string, the line ends
# kept), or NULL when its markers are not there in order.  Lines before the
# first marker go with part 1; a marker line repeated further down is the
# part's text.
.split_study_setup <- function(txt) {
  if (!nzchar(txt)) return(NULL)
  lines <- strsplit(txt, "(?<=\n)", perl = TRUE, useBytes = TRUE)[[1L]]
  at <- integer()
  from <- 1L
  for (m in .setup_marker) {
    i <- grep(m, lines, useBytes = TRUE)
    i <- i[i >= from]
    if (!length(i)) return(NULL)
    at <- c(at, i[1L])
    from <- i[1L] + 1L
  }
  part <- function(from, to) {
    x <- if (to < from) "" else paste(lines[seq.int(from, to)], collapse = "")
    Encoding(x) <- "UTF-8"
    x
  }
  list(standard = part(1L, at[2L] - 1L),
       study = part(at[2L], at[3L] - 1L),
       user = part(at[3L], length(lines)))
}

.read_bytes <- function(f) {
  n <- file.size(f)
  if (is.na(n) || !n) return("")
  txt <- rawToChar(readBin(f, "raw", n))
  Encoding(txt) <- "UTF-8"
  txt
}

.write_bytes <- function(txt, f) {
  con <- file(f, "wb")
  on.exit(close(con))
  writeBin(charToRaw(txt), con)
}

.lines_text <- function(lines) paste0(paste(enc2utf8(lines), collapse = "\n"), "\n")

# a part 2 as written still matches its checksum
.study_part_intact <- function(part) {
  lines <- strsplit(gsub("\r", "", part, fixed = TRUE), "\n", fixed = TRUE)[[1L]]
  lines <- lines[-1L]
  chk <- sub("^#  Checksum   : *", "",
             grep("^#  Checksum   :", lines, value = TRUE, useBytes = TRUE))
  length(chk) == 1L && identical(chk, .body_hash(lines))
}

#' The study's setup program
#'
#' `study_setup_code()` is a new study's `programs/study_setup.R`, which
#' `programs/ard/ard_setup.R`, `programs/tfl/report_setup.R` and
#' `programs/tfl/fig_setup.R` each source first, so every ARD, report and
#' figure program runs it.  It is one file of three marked parts, run in
#' this order:
#'
#' 1. the company standard: [setup_code()], copied when the study is made
#'    (its marker line says when, and which standards); tflplanner does
#'    not change it after.
#' 2. the study: its folders ([study_layout()]: `path_adam`, `path_sdtm`
#'    ...) and what it is (`study_id`, `study_title`, `study_compound`,
#'    `study_phase`, `study_description` from `study.yml`).  tflplanner
#'    writes it again on every save.
#' 3. the study's own code: tflplanner never touches it.
#'
#' Saving a study rewrites part 2 only, and parts 1 and 3 stay byte for
#' byte.  A part 2 edited by hand (its checksum no longer matches) is
#' written again too, the edited file first put in `programs/.edited/`.
#'
#' @param meta The study's fields (`study_id`, `title`, ...), as an
#'   `rtfstudy`'s `meta`.
#' @param date The date the marker of part 1 says it was copied.
#' @param standards The standards to copy part 1 from.
#' @return The program, one element per line.
#' @export
study_setup_code <- function(meta, date = Sys.Date(),
                             standards = company_standards()) {
  c(.setup_marker_standard(date, standards$about),
    setup_code(meta[["study_id"]] %||% NA, standards),
    "",
    .study_setup_part2(meta),
    .setup_marker_user)
}

# programs/study_setup.R, written with the study: made when missing (part 1
# from the standards in use, part 3 empty); otherwise part 2 only, when it
# has changed.  A file whose markers are gone, or whose part 2 was edited,
# goes to programs/.edited/ first.  The file's row of what the save did.
.save_study_setup <- function(meta, root) {
  f <- file.path(root, .study_setup_path())
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  row <- function(status) data.frame(file = f, status = status,
                                     stringsAsFactors = FALSE)
  parts <- if (file.exists(f)) .split_study_setup(.read_bytes(f))
  if (is.null(parts)) {
    edited <- file.exists(f)
    if (edited) .back_up_edited(f, root)
    .write_bytes(.lines_text(study_setup_code(meta)), f)
    return(row(if (edited) "rewritten" else "written"))
  }
  new <- .lines_text(.study_setup_part2(meta))
  same <- identical(charToRaw(gsub("\r", "", parts$study, fixed = TRUE)),
                    charToRaw(new))
  if (same) {
    return(row("unchanged"))
  }
  edited <- !.study_part_intact(parts$study)
  if (edited) .back_up_edited(f, root)
  .write_bytes(paste0(parts$standard, new, parts$user), f)
  row(if (edited) "rewritten" else "written")
}

# The fingerprint of the study's setup: what an ARD or a report records
# when it is built, so a change to study_setup.R marks it for a rebuild
# ("" when there is none).  The programs work it out the same way
# (tools::md5sum() of the file).
.study_setup_hash <- function(root) {
  f <- file.path(root, .study_setup_path())
  if (file.exists(f)) unname(tools::md5sum(f)) else ""
}

# Built with another study setup than the one there now?  An output built
# before the setup was recorded ("" or NA) is not.
.setup_changed <- function(recorded, root, now = .study_setup_hash(root)) {
  !is.na(recorded) & nzchar(recorded) & recorded != now
}

# the line of a generated program that works out the setup's fingerprint
.setup_hash_code <- function() {
  p <- encodeString(.study_setup_path(), quote = "\"")
  sprintf("if (file.exists(%s)) unname(tools::md5sum(%s)) else \"\"", p, p)
}
