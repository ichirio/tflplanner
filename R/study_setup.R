# programs/study_setup.R: what every program of the study starts with.
#
#   programs/ard/<id>.R  -> programs/ard/ard_setup.R    -> programs/study_setup.R
#   programs/tfl/<id>.R  -> programs/tfl/report_setup.R -> programs/study_setup.R
#   figure programs      -> programs/tfl/fig_setup.R    -> programs/study_setup.R
#
# Three files, run in this order (a later one wins):
#
#   programs/company_setup.R  the company's code: copied from the standards
#                             (sheet setup_code) when the study is made; the
#                             study's own after that -- tflplanner never
#                             writes it again
#   programs/study_setup.R    generated on every save: the study's folders
#                             (study_layout()), what the study is
#                             (study.yml), its rounding and data, the
#                             helpers.  It runs the other two: the company's
#                             first, the user's last, once per program
#   programs/study_user.R     the user's code; tflplanner never writes it
#                             again
#
# A study made before (#910) has one study_setup.R of three marked parts.
# Its first save splits it (.split_old_setup()): part 1 to
# company_setup.R and part 3 to study_user.R, byte for byte.  When it
# cannot be split cleanly, nothing is converted: the old file stays as it
# is and runs, a copy goes to programs/.edited/ and the save says why.

.study_setup_file <- "study_setup.R"
.study_helpers_file <- "study_helpers.R"
.company_setup_file <- "company_setup.R"
.study_user_file <- "study_user.R"

# programs/study_helpers.R, relative to the study folder: next to the setup
# that sources it
.study_helpers_path <- function() {
  file.path(dirname(study_layout()[["programs_ard"]]), .study_helpers_file)
}

#' The functions a study's programs call
#'
#' `programs/study_helpers.R`: the code of the functions the generated
#' programs call as they run -- `set_levels()` (a report's code lists on its
#' data: each listed column a factor in the list's order, its values the
#' labels; a value not listed stops the program), `tag_ard()`, `fmt_ard()`,
#' `fmt_pvalue()`, `keep_stats()` and `save_ard()` -- as tflspec gives them
#' ([tflspec::tfl_helpers_code()]).  The study's setup sources it, so its
#' programs do not call tflspec for these.  They still need tflspec to run
#' in some cases: a report that reads an imported ARD calls
#' `tflspec::tfl_read_ard()`, and a KM curve or forest plot calls tflspec's
#' `km_curves()` or `forest_hr()`.  Saving the study writes it again when
#' tflspec's have changed (one edited by hand is copied to
#' `programs/.edited/` first).
#'
#' @param date The date stamped in the banner.
#' @return The code, one element per line.
#' @examples
#' # the functions the study's programs call
#' code <- study_helpers_code()
#' head(code)
#' @export
study_helpers_code <- function(date = Sys.Date()) {
  c(.banner(
      paste("Program    :", .study_helpers_path()),
      "The functions the study's programs call: the code lists on a data,",
      "the ARD's ids and formats, the study ARD.  The study's setup sources it.",
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             " (tflspec ", utils::packageVersion("tflspec"), "), ",
             format(date, "%Y-%m-%d"))),
    "",
    tflspec::tfl_helpers_code())
}

# write programs/study_helpers.R (its row of what the save did)
.save_study_helpers <- function(root) {
  f <- file.path(root, .study_helpers_path())
  data.frame(file = f, status = .put_program(study_helpers_code(), f, root),
             stringsAsFactors = FALSE)
}

# programs/study_setup.R, relative to the study folder: next to the
# folders of the ARD and report programs
.study_setup_path <- function() {
  file.path(dirname(study_layout()[["programs_ard"]]), .study_setup_file)
}

# programs/company_setup.R and programs/study_user.R, relative to the study
# folder: next to the setup that runs them
.company_setup_path <- function() file.path(dirname(.study_setup_path()), .company_setup_file)
.study_user_path <- function() file.path(dirname(.study_setup_path()), .study_user_file)

# The study's setup files in the order a program runs them, relative to the
# study folder: the company's, the generated one, the user's
.study_setup_files <- function() {
  c(company = .company_setup_path(), setup = .study_setup_path(), user = .study_user_path())
}

# Those of them that are in `root`, in that order (a study made before
# #910 has study_setup.R alone)
.study_setup_present <- function(root) {
  f <- file.path(root, .study_setup_files())
  names(f) <- names(.study_setup_files())
  f[file.exists(f)]
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
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' setup_code("ABC-1")  # the company's lines, with the study's id in
#' options(old)
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

# The generated lines of study_setup.R: the study's folders and what the
# study is, as R variables, the study's rounding and data (tflspec writes
# them: tflspec#791) and the helpers
.study_setup_body <- function(meta, study = NULL) {
  lay <- study_layout()
  val <- function(v) {
    v <- as.character(v %||% NA_character_)
    if (length(v) != 1L || is.na(v)) "NA_character_" else
      encodeString(enc2utf8(v), quote = "\"")
  }
  nm <- function(x) formatC(x, width = -max(nchar(x)))
  paths <- paste0("path_", names(lay))
  ids <- paste0("study_", sub("^study_", "", .study_fields))
  c("# ---- the study: its folders, relative to the study folder (the programs",
    "#      run from it), and what it is (study.yml)",
    paste(nm(paths), "<-", vapply(lay, val, "")),
    "",
    paste(nm(ids), "<-", vapply(.study_fields, function(k) val(meta[[k]]), "")),
    "",
    if (length(rnd <- .study_rounding_lines(study))) c(rnd, ""),
    "# the functions the programs call (tflplanner writes them)",
    sprintf("source(%s)", encodeString(.study_helpers_path(), quote = "\"")),
    "")
}

# The study's rounding as study_setup.R sets it: tflspec's lines, from the study's
# spec or the sheets' (none: NULL, or a study that says no rounding).  The
# ARD and report setups then leave it out (option tflspec.study_setup).
.study_rounding_lines <- function(study) {
  if (is.null(study)) return(character())
  # study_setup.R is written outside .with_study_code(): the data lines
  # name the folders through the variables it defines above them
  old <- options(tflspec.paths = .study_path_vars())
  on.exit(options(old), add = TRUE)
  tflspec::tfl_study_setup_code(study)
}

# What a planner's rounding is read from: the study's spec, or (a planner
# written from its sheets) the sheets' spec, as the setups read it
.study_rounding_source <- function(x) {
  s <- if (inherits(x$spec$study, "tfl_study_spec")) x$spec$study else
    tryCatch(.spec_object_last(x, c(table_sheets(), report_sheets()),
                               unique(c(.study_keys$table, .study_keys$report))),
             error = function(e) NULL)
  .setup_catalog_used(s, x)
}

# The datasets the study's reports use (their analyses, populations,
# listings' data, figures' reads and code steps' registers), upper case.
.reports_datasets <- function(x) {
  po <- x$ard$populations
  ad <- .adata_rows(x)
  l <- x$lf$listings
  pops <- unique(c(x$ard$analyses$population_id, l$population_id))
  ids <- unique(c(x$ard$analyses$data, x$ard$analyses$denominator, l$data))
  ds <- c(unlist(lapply(x$outputs$output_id, function(id)
            tryCatch(.report_datasets(x, id), error = function(e) character()))),
          po$dataset[match(pops[!is.na(pops)], po$population_id)],
          unlist(lapply(ids[!is.na(ids) & ids %in% ad$data_id], .adata_datasets,
                        ad = ad, po = po)))
  toupper(unique(ds[!is.na(ds) & nzchar(ds)]))
}

# The study's spec with its data catalog cut to the datasets a report uses:
# the setup reads what the study's reports need, not every file the catalog
# lists (a dataset no report uses may not be on disk).  The order is the
# catalog's.
.setup_catalog_used <- function(study, x) {
  if (is.null(study)) return(study)
  used <- .reports_datasets(x)
  if (inherits(study, "tfl_study_spec")) {
    d <- study$data$datasets
    if (length(d)) {
      keep <- vapply(d, function(z) toupper(as.character(z$dataset %||% NA)[1L]) %in% used, NA)
      study$data$datasets <- d[keep]
    }
  } else if (is.data.frame(study$datasets)) {
    study$datasets <- study$datasets[toupper(study$datasets$dataset) %in% used, , drop = FALSE]
  }
  study
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

#' The study's setup programs
#'
#' Every ARD, report and figure program of a study starts with its setup
#' (`programs/ard/ard_setup.R`, `programs/tfl/report_setup.R` and
#' `programs/tfl/fig_setup.R` each source `programs/study_setup.R` first).
#' The study's setup is three files, run in this order:
#'
#' 1. `programs/company_setup.R`, the company's code: [setup_code()],
#'    copied when the study is made (`company_setup_code()`; its first
#'    line says when, and which standards).  It is the study's after that:
#'    tflplanner never writes it again.
#' 2. `programs/study_setup.R` (`study_setup_code()`), written by
#'    tflplanner on every save -- do not edit it: the study's folders
#'    ([study_layout()]: `path_adam`, `path_sdtm` ...), what it is
#'    (`study_id`, `study_title`, `study_short_title`, `study_compound`,
#'    `study_phase`, `study_description` from `study.yml`), its rounding
#'    and data ([tflspec::tfl_study_setup_code()]), and the helpers
#'    ([study_helpers_code()]).  It runs the other two: the company's
#'    first, the user's last.
#' 3. `programs/study_user.R`, the study's own code; tflplanner never
#'    writes it again.
#'
#' The company's code and the user's run once per program: a figure
#' report sources the study's setup twice (through `report_setup.R` and
#' `fig_setup.R`) and runs them once.  They run again in another study
#' folder, another R process, and when either file has changed;
#' `rm(.tfl_free_ran)` runs them again by hand.  A `study_setup.R` edited
#' by hand is written again on the next save, the edited one first put in
#' `programs/.edited/`.
#'
#' A study made before has one `study_setup.R` of three marked parts.  Its
#' first save splits it: part 1 to `company_setup.R` and part 3 to
#' `study_user.R`, byte for byte, and `study_setup.R` is written new (the
#' old one kept in `programs/.edited/`).  When its marker lines are missing
#' or repeated, its part 2 was edited, or `company_setup.R` or
#' `study_user.R` is there already with other content, nothing is
#' converted: the old file stays as it is and runs, a copy goes to
#' `programs/.edited/`, and the save says why.
#'
#' @param meta The study's fields (`study_id`, `title`, ...), as an
#'   `rtfstudy`'s `meta`.
#' @param date The date the banner says it was generated (or, for the
#'   company's code, copied).
#' @param study The study's spec ([tflspec::tfl_read_study_spec()]), or a
#'   table / report spec of the sheets, whose rounding and data
#'   `study_setup.R` sets; `NULL` (the default): none.
#' @param standards The standards to copy the company's code from.
#' @return The program, one element per line.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' meta <- list(study_id = "ABC-1", title = "A study of Drug X")
#' head(study_setup_code(meta), 12)
#' company_setup_code(meta)
#' options(old)
#' @export
study_setup_code <- function(meta, date = Sys.Date(), study = NULL) {
  q <- function(x) encodeString(x, quote = "\"")
  c(.banner(
      paste("Program    :", .study_setup_path()),
      "What every program of the study starts with (ard_setup.R, report_setup.R",
      "and fig_setup.R source it).  tflplanner writes it on every save: do not",
      paste("edit it.  The company's code is in", .company_setup_file, "and yours in"),
      paste0(.study_user_file, ": this file runs them, the company's first and yours last."),
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    "# The company's code and yours run once per program: a program that",
    "# sources this file twice (a figure report: report_setup.R, fig_setup.R)",
    "# runs them once, and again when either file has changed.  To run them",
    "# again by hand: rm(.tfl_free_ran)",
    sprintf(".tfl_free <- c(company = %s, user = %s)",
            q(.company_setup_path()), q(.study_user_path())),
    ".tfl_free_key <- paste(c(getwd(), Sys.getpid(),",
    "                         tools::md5sum(.tfl_free[file.exists(.tfl_free)])),",
    "                       collapse = \"|\")",
    ".tfl_free_run <- !identical(get0(\".tfl_free_ran\", envir = environment(),",
    "                                 inherits = FALSE), .tfl_free_key)",
    "",
    paste0("# ---- the company's code (", .company_setup_file, ")"),
    "if (.tfl_free_run && file.exists(.tfl_free[[\"company\"]])) {",
    "  source(.tfl_free[[\"company\"]], local = environment())",
    "}",
    "",
    .study_setup_body(meta, study),
    paste0("# ---- your code (", .study_user_file, ")"),
    "if (.tfl_free_run && file.exists(.tfl_free[[\"user\"]])) {",
    "  source(.tfl_free[[\"user\"]], local = environment())",
    "}",
    ".tfl_free_ran <- .tfl_free_key")
}

#' @rdname study_setup_code
#' @export
company_setup_code <- function(meta, date = Sys.Date(), standards = company_standards()) {
  about <- standards$about
  v <- function(k) {
    x <- about$value[match(k, about$key)]
    if (length(x) && !is.na(x) && nzchar(x)) x else NA_character_
  }
  std <- paste(stats::na.omit(c(v("name"), v("version"))), collapse = " ")
  if (!nzchar(std)) std <- "built-in"
  upd <- v("updated")
  c(paste0("# The company's code (", .company_setup_path(), "): copied from the company"),
    paste0("# standards (", std, if (!is.na(upd)) paste0(" of ", upd), ") on ",
           format(date, "%Y-%m-%d"), ".  The study's own now: tflplanner"),
    "# never writes it again.  Every program runs it first (study_setup.R).",
    "",
    setup_code(meta[["study_id"]] %||% NA, standards))
}

# programs/study_user.R as a new study gets it
.study_user_code <- function() {
  c(paste0("# Your study's code (", .study_user_path(), "): helper functions, options,"),
    "# library() lines.  Every program runs it after the study's setup",
    "# (study_setup.R), before the ARD, report or figure setup.  tflplanner never",
    "# writes it again.  Values the study's YAML defines (the rounding, the",
    "# folders, the formats) are set there, not here.",
    "")
}

# a study_setup.R in the form before #910: its marker lines
.is_old_setup <- function(f) {
  any(grepl(.setup_marker[["study"]], readLines(f, warn = FALSE, encoding = "UTF-8"),
            useBytes = TRUE))
}

# Write `txt` to `f` through a file beside it, so a failure leaves `f` as it was
.write_bytes_staged <- function(txt, f) {
  tmp <- paste0(f, ".tmp-split")
  .write_bytes(txt, tmp)
  if (!file.rename(tmp, f)) {
    unlink(tmp)
    stop("could not write ", f, call. = FALSE)
  }
  invisible(f)
}

# Split an old study_setup.R (three marked parts) into company_setup.R
# (part 1), study_user.R (part 3) and a new generated study_setup.R.  The
# rows of what it did; attribute "note": why it kept the old file.
.split_old_setup <- function(meta, root, study = NULL) {
  f <- file.path(root, .study_setup_path())
  free <- file.path(root, c(company = .company_setup_path(), user = .study_user_path()))
  names(free) <- c("company", "user")
  row <- function(file, status) data.frame(file = file, status = status, stringsAsFactors = FALSE)
  txt <- .read_bytes(f)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  once <- vapply(.setup_marker, function(m) sum(grepl(m, lines, useBytes = TRUE)) == 1L, NA)
  parts <- if (all(once)) .split_study_setup(txt)
  why <- if (is.null(parts)) {
    "its marker lines are missing, repeated or out of order"
  } else if (!.study_part_intact(parts$study)) {
    "its tflplanner part was edited by hand"
  } else {
    want <- c(company = parts$standard, user = parts$user)
    clash <- vapply(names(free), function(k) file.exists(free[[k]]) &&
                      !identical(.read_bytes(free[[k]]), want[[k]]), NA)
    if (any(clash)) "company_setup.R or study_user.R is there already, with other content"
  }
  if (!is.null(why)) {
    .back_up_edited(f, root)
    out <- row(f, "kept")
    attr(out, "note") <- why
    return(out)
  }
  # staged: the two free files, then the new study_setup.R; a failure
  # takes back what was written and leaves the old file
  .back_up_edited(f, root)
  made <- character()
  ok <- tryCatch({
    for (k in names(free)) {
      if (!file.exists(free[[k]])) {
        .write_bytes_staged(want[[k]], free[[k]])
        made <- c(made, free[[k]])
      }
    }
    .write_bytes_staged(.lines_text(.program_lines(study_setup_code(meta, study = study))), f)
    TRUE
  }, error = function(e) {
    unlink(made)
    e
  })
  if (!isTRUE(ok)) {
    out <- row(f, "kept")
    attr(out, "note") <- conditionMessage(ok)
    return(out)
  }
  rbind(row(free[["company"]], "split"), row(free[["user"]], "split"), row(f, "split"))
}

# The lines of a generated program as .write_program() writes them (its
# checksum after the Generated line)
.program_lines <- function(code) {
  at <- grep("^#  Generated  :", code)[1L]
  append(code, paste("#  Checksum   :", .body_hash(code)), at)
}

# The study's setup files, written with the study.  study_setup.R: written
# when it changed (one edited by hand goes to programs/.edited/ first);
# company_setup.R and study_user.R: written when missing, never after.  An
# old study_setup.R is split first (.split_old_setup()).  The rows of what
# the save did; attribute "note": why an old file was kept as it was.
.save_study_setup <- function(meta, root, study = NULL) {
  f <- file.path(root, .study_setup_path())
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  row <- function(file, status) data.frame(file = file, status = status, stringsAsFactors = FALSE)
  rows <- list()
  if (file.exists(f) && .is_old_setup(f)) {
    sp <- .split_old_setup(meta, root, study)
    if (!is.null(attr(sp, "note"))) return(sp)
    rows <- list(sp)
  }
  for (k in c("company", "user")) {
    g <- file.path(root, .study_setup_files()[[k]])
    if (!file.exists(g)) {
      .write_bytes(.lines_text(if (k == "company") company_setup_code(meta) else .study_user_code()), g)
      rows <- c(rows, list(row(g, "written")))
    }
  }
  status <- .put_program(study_setup_code(meta, study = study), f, root)
  # (a split wrote it already: that row says so)
  if (!length(rows) || !any(rows[[1L]]$status == "split")) rows <- c(rows, list(row(f, status)))
  do.call(rbind, rows)
}

# The fingerprint of the study's setup: what an ARD or a report records
# when it is built, so a change to one of its files (company_setup.R,
# study_setup.R, study_user.R) marks it for a rebuild.  Their md5s joined
# by "+", those that are there, in that order (a study made before #910:
# study_setup.R's alone; "" when there is none).  The programs work it
# out the same way.
.study_setup_hash <- function(root) {
  paste(unname(tools::md5sum(.study_setup_present(root))), collapse = "+")
}

# The study setup an ARD was built with, from its row of ard_status.csv:
# the md5s save_ard() recorded of company_setup.R, study_setup.R and
# study_user.R (option tflspec.ard_sources), joined as .study_setup_hash()
# joins them ("" when none was recorded)
.ard_recorded_setup <- function(r) {
  v <- vapply(c("company", "setup", "user"), function(k) {
    x <- if (k %in% names(r)) as.character(r[[k]])[1L] else NA_character_
    if (length(x) && !is.na(x)) x else ""
  }, "")
  paste(v[nzchar(v)], collapse = "+")
}

# Built with another study setup than the one there now?  An output built
# before the setup was recorded ("" or NA) is not.
.setup_changed <- function(recorded, root, now = .study_setup_hash(root)) {
  !is.na(recorded) & nzchar(recorded) & recorded != now
}

# A report's setup: the study's setup files (company_setup.R, study_setup.R,
# study_user.R: their fingerprints joined by "+", those that are there; a
# study made before #910, study_setup.R's alone) and
# programs/tfl/report_setup.R (the study's font, tokens, header, rounding:
# every report program sources it): "<study>|<report>" -- what a report
# program records (.report_setup_hash_code()); the ARD reads study_setup.R
# only
.report_setup_hash <- function(root) {
  md5 <- function(x) if (file.exists(x)) unname(tools::md5sum(x)) else ""
  st <- .study_setup_present(root)
  paste(paste(vapply(st, md5, ""), collapse = "+"),
        md5(file.path(root, .report_setup_path())), sep = "|")
}

.report_setup_path <- function() file.path(study_layout()[["programs_tfl"]], .report_setup_file)

# Made with another report setup than the one there now?  A record of
# study_setup.R alone (made before report_setup.R was counted) is held to
# it alone; one made before the setup was recorded ("" or NA) is not.
.report_setup_changed <- function(recorded, root, now = .report_setup_hash(root)) {
  old <- !grepl("|", recorded, fixed = TRUE)
  cmp <- ifelse(old, sub("[|].*$", "", now), now)
  !is.na(recorded) & nzchar(recorded) & recorded != cmp
}

# ... the lines of a report program's record_report() that work it out
.report_setup_hash_code <- function() {
  q <- function(x) paste(encodeString(x, quote = "\""), collapse = ", ")
  sprintf(paste0("paste(paste(unname(tools::md5sum(Filter(file.exists, c(%s)))),\n",
                 "                collapse = \"+\"),\n",
                 "          if (file.exists(%s)) unname(tools::md5sum(%s)) else \"\",\n",
                 "          sep = \"|\")"),
          q(.study_setup_files()), q(.report_setup_path()), q(.report_setup_path()))
}

# the line of a generated program that works out the setup's fingerprint
.setup_hash_code <- function() {
  p <- paste(encodeString(.study_setup_files(), quote = "\""), collapse = ", ")
  sprintf("paste(unname(tools::md5sum(Filter(file.exists, c(%s)))),\n        collapse = \"+\")", p)
}
