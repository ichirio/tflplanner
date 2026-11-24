# A Rows field whose value the SPEC keeps as text (tflspec's def
# `serializer:`, e.g. plan_sort's keys "sort_keys": "-n@group1[Total]" one
# row a key).  The field shows the rows and writes the text: tflspec turns
# one into the other (tfl_rows_read() / tfl_rows_write()), so nothing here
# knows the text's grammar.  A type that also takes `false` (plan_sort's
# keys: the data's order) has a switch under the rows that writes it.
#
#   .def_serializer(def)                    the def's serializer word, NULL none
#   .rows_text_read(value, def)             the value -> list(value = rows, false)
#   .rows_text_write(rows, def, false)      the rows (records) -> the value
#   .rows_false_switch(fid, def, on, lang)  the switch, NULL when the type has no false

.def_serializer <- function(def) {
  s <- def[["serializer"]]
  if (is.null(s) || length(s) != 1L || is.na(s) || !nzchar(s)) NULL else as.character(s)
}

# whether the def's type takes a logical (its `false`) beside the rows
.def_takes_false <- function(def) {
  type <- def$def_type %||% def$type
  !is.null(type) && length(type) == 1L && !is.na(type) && "logical" %in% trimws(.type_split(type, "|"))
}

# The value as the field shows it: the rows (a data frame) and whether the
# switch is on (the value is FALSE).  A value the serializer cannot read
# (plan_sort(TRUE)) is given back as its R -- the field then shows it as R,
# nothing lost.
.rows_text_read <- function(value, def) {
  ser <- .def_serializer(def)
  if (is.null(ser) || is.null(value) || inherits(value, c("tfl_r", "tfl_unset"))) {
    return(list(value = value, false = FALSE))
  }
  rd <- get0("tfl_rows_read", envir = asNamespace("tflspec"), inherits = FALSE)
  rows <- if (is.function(rd)) tryCatch(rd(value, ser), error = function(e) NULL)
  if (is.null(rows)) return(list(value = tflspec::tfl_r(.arg_to_r(value)), false = FALSE))
  list(value = rows, false = isTRUE(attr(rows, "false", exact = TRUE)))
}

# The rows read back (.tbl_read()'s records) as the value the SPEC keeps:
# FALSE when the switch is on; NULL when no row is left (the argument not
# written).  Rows the text cannot say (a `by` with @ in it and a level) are
# not written: NULL, the value as it was.
.rows_text_write <- function(rows, def, false = FALSE) {
  ser <- .def_serializer(def)
  if (is.null(ser)) return(rows)
  if (isTRUE(false) && .def_takes_false(def)) return(FALSE)
  wr <- get0("tfl_rows_write", envir = asNamespace("tflspec"), inherits = FALSE)
  if (!is.function(wr)) return(rows)
  tryCatch(wr(rows, ser), error = function(e) NULL)
}

# The switch under the rows that writes the type's `false`: its label the
# def's false_label (in the language), checked when the value is FALSE
.rows_false_switch <- function(fid, def, on = FALSE, lang = "en") {
  if (is.null(.def_serializer(def)) || !.def_takes_false(def)) return(NULL)
  ok <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  ja <- def$false_label_ja
  en <- def$false_label_en
  label <- if (identical(lang, "ja") && ok(ja)) ja else if (ok(en)) tr(en, lang) else tr("Keep the data's order", lang)
  shiny::tags$label(
    class = "form-check rp-tbl-false-wrap",
    shiny::tags$input(type = "checkbox", id = paste0(fid, "__false"), class = "form-check-input rp-tbl-false",
                      checked = if (isTRUE(on)) NA),
    shiny::tags$span(class = "form-check-label", label))
}

# What a table's sort keys (plan_sort's Rows: by = fixed, columns(row_keys),
# stats; level = columns(row_groups); column = columns(spread)) offer, as the
# builder knows them: the row keys by their names in the plan (group1 ...
# for the outer ones, label the innermost), labelled with their variables;
# the column key's values and the Total; the ARD's statistics.  The
# .choice_values() ctx (`columns` by object, `stats`).
.sort_rows_sources <- function(st, stats = character(), total = NA, lang = "en") {
  rk <- as.character(st$row_keys %||% character())
  n <- length(rk)
  outs <- if (n) c(if (n > 1L) sprintf("group%d", seq_len(n - 1L)), "label") else character()
  keys <- stats::setNames(outs, if (n) sprintf("%s (%s)", rk, outs) else character())
  key <- if (length(st$key)) st$key[1L] else NA_character_
  arms <- if (!is.na(key)) as.character(st$arms[[key]] %||% character()) else character()
  spread <- unique(c(arms, if (length(total) && !is.na(total[1L]) && nzchar(total[1L])) total[1L]))
  # (the statistics but N, n and p first, each by its name; n and p said)
  sx <- unique(as.character(unlist(stats)))
  sx <- sx[!is.na(sx) & sx != "N"]
  sx <- c(intersect(c("n", "p"), sx), setdiff(sx, c("n", "p")))
  said <- c(n = "Count (n)", p = "Percent (p)")
  sl <- vapply(sx, function(x) if (x %in% names(said)) tr(said[[x]], lang) else x, "")
  list(stats = stats::setNames(sx, sl),
       columns = list(row_keys = keys, row_groups = keys[seq_len(max(0L, n - 1L))],
                      spread = stats::setNames(spread, spread)))
}
