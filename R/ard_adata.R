# The analysis data of a study's ARD definition (tflspec's sheet
# `analysis_data`): named data the analyses read -- made from a dataset or
# another analysis data, kept to a population's subjects and the records a
# condition keeps, with columns of the population's data added, columns
# derived, one row per set of values.  The study's: one name, one meaning,
# for every report.  The ARD tab shows a report's first (1. the analysis
# data, 2. the analyses); an analysis names one in `data`.

# the sheet (no rows when the study has none)
.adata_rows <- function(x) {
  .normalize_ard_sheet(x$ard$analysis_data %||% data.frame(), "analysis_data")
}

# the rows a data is made from, from the first to itself (NULL: none)
.adata_chain <- function(ad, id) {
  out <- character()
  while (length(id) && !is.na(id) && id %in% ad$data_id && !id %in% out) {
    out <- c(id, out)
    id <- ad$from[match(id, ad$data_id)]
  }
  if (length(out)) out
}

# its analysis set: its own population, else that of the data whose
# subjects it keeps, else the nearest above it (tflspec's rule)
.adata_pop <- function(ad, id, seen = character()) {
  for (d in rev(.adata_chain(ad, id))) {
    i <- match(d, ad$data_id)
    p <- ad$population_id[i]
    if (!is.na(p)) return(p)
    s <- ad$subjects[i] %||% NA
    if (!is.na(s) && !s %in% c(seen, d)) return(.adata_pop(ad, s, c(seen, d)))
  }
  NA_character_
}

# Is an analysis data a report's subjects (an analysis set's own data, kept
# to some of its subjects: one row a subject, a denominator) or data
# analysed (another dataset, kept to subjects)?  "subjects" / "data"
.adata_kind <- function(x, id) {
  ad <- .adata_rows(x)
  po <- x$ard$populations
  ds <- .adata_dataset(ad, id)
  pop <- .adata_pop(ad, id)
  pop_ds <- po$dataset[match(pop, po$population_id)]
  if (!is.na(ds) && !is.na(pop_ds) && identical(ds, pop_ds)) "subjects" else "data"
}

# the subjects data it is kept to: its own `subjects`, else the nearest
# above it (NA when none)
.adata_subjects_of <- function(ad, id) {
  for (d in rev(.adata_chain(ad, id))) {
    s <- ad$subjects[match(d, ad$data_id)] %||% NA
    if (!is.na(s)) return(s)
  }
  NA_character_
}

# the dataset it is first made from
.adata_dataset <- function(ad, id) {
  ch <- .adata_chain(ad, id)
  if (is.null(ch)) NA_character_ else ad$from[match(ch[1L], ad$data_id)]
}

# The analysis data a report reads (`data`, `denominator`), with the rows
# they are made from, in the sheet's order
.adata_of_report <- function(x, output_id) {
  ad <- .adata_rows(x)
  a <- x$ard$analyses
  a <- a[!is.na(a$output_id) & a$output_id %in% output_id, , drop = FALSE]
  ids <- intersect(c(a$data %||% character(), a$denominator), ad$data_id)
  all <- character()
  while (length(new <- setdiff(unique(unlist(lapply(ids, function(id)
    .adata_chain(ad, id)))), all))) {
    all <- c(all, new)
    ids <- intersect(stats::na.omit(ad$subjects[match(new, ad$data_id)]), ad$data_id)
  }
  ad$data_id[ad$data_id %in% all]
}

# Where an analysis data is used: the analyses that read it or divide by it
# ("T1 / AE"), and the analysis data made from it
.adata_uses <- function(x, id) {
  a <- x$ard$analyses
  d <- a$data %||% rep(NA_character_, nrow(a))
  hit <- (!is.na(d) & d == id) |
    (!is.na(a$denominator) & a$denominator == id)
  ad <- .adata_rows(x)
  list(analyses = paste(a$output_id[hit], a$analysis_id[hit], sep = " / "),
       data = ad$data_id[(!is.na(ad$from) & ad$from == id) |
                           (!is.na(ad$subjects) & ad$subjects == id)])
}

# A name for a new analysis data: the dataset (or data) and the value a
# condition keeps (`AVISIT == "Week 24"`: advs_week24), else the analysis
# set; lower case, and not a name the program gives anything (a dataset, a
# population, a dataset x analysis set's own: adae_saf)
.adata_suggest <- function(x, from, population_id = NA, where = NA) {
  rn <- function(v) gsub("[^a-z0-9_.]", "_", tolower(v))
  val <- if (!.is_blank(where)) regmatches(where, regexpr("\"[^\"]+\"", where))
  val <- if (length(val)) gsub("[^a-z0-9]", "", tolower(val)) else ""
  base <- paste(c(rn(from), if (nzchar(val)) val else
    if (!.is_blank(population_id)) rn(population_id)), collapse = "_")
  if (!grepl("^[a-z]", base)) base <- paste0("d_", base)
  ds <- x$ard$datasets$dataset
  pops <- x$ard$populations$population_id
  auto <- unlist(lapply(pops, function(p) vapply(ds, function(d)
    .an_data_name(d, p, x$ard$populations), "")))
  taken <- c(rn(ds), paste0("pop_", rn(pops)), auto,
             .adata_rows(x)$data_id, "data", "population", "ard", "ards", "status")
  nm <- base
  k <- 1L
  while (nm %in% taken) {
    nm <- paste0(base, "_", k)
    k <- k + 1L
  }
  nm
}

#' Add, change or remove an analysis data of the ARD definition
#'
#' The analysis data are named data the analyses read (tflspec's sheet
#' `analysis_data`): made `from` a dataset or an analysis data above,
#' kept to a population's subjects and the records `where` keeps, with
#' columns of the population's data added (`add`), columns derived
#' (`derive`) and one row per set of values (`distinct`).  An analysis
#' names one in `data` (instead of `dataset` / `population_id`), or as its
#' `denominator`.
#'
#' `set_analysis_data()` adds one, or replaces the one named `old` (a new
#' name is followed in the analyses and the analysis data made from it).
#' `remove_analysis_data()` takes one out, refused while it is used.
#' `name_analysis_data()` gives a report's data -- a dataset x analysis set
#' its analyses read -- a name: the analysis data is made, those analyses
#' read it, and a condition all of them have moves into it.
#'
#' @param x A `tflplanner`.
#' @param data_id The name (lower case; also the object's name in the ARD
#'   program).
#' @param from A dataset, or an analysis data above.
#' @param population_id,subjects,where,add,derive,keep,distinct,label The
#'   other columns (`subjects`: an analysis data above whose subjects it
#'   keeps, instead of `population_id`; `add`, `keep`, `distinct`: several
#'   columns with `" | "` between them).
#' @param old The name of the one to replace; `NULL`: a new one.
#' @return The `tflplanner`.
#' @export
set_analysis_data <- function(x, data_id, from, population_id = NA,
                              where = NA, add = NA, derive = NA,
                              distinct = NA, label = NA, old = NULL,
                              subjects = NA, keep = NA) {
  ad <- .adata_rows(x)
  row <- .normalize_ard_sheet(data.frame(
    data_id = data_id, label = label, from = from,
    population_id = population_id, subjects = subjects, where = where,
    add = add, derive = derive, keep = keep, distinct = distinct,
    stringsAsFactors = FALSE), "analysis_data")
  if (!nrow(row) || is.na(row$data_id)) {
    stop("An analysis data needs its name.", call. = FALSE)
  }
  if (is.null(old)) {
    if (row$data_id %in% ad$data_id) {
      stop("There is an analysis data '", row$data_id, "' already.", call. = FALSE)
    }
    ad <- rbind(ad, row)
  } else {
    i <- match(old, ad$data_id)
    if (is.na(i)) stop("No analysis data '", old, "'.", call. = FALSE)
    if (!identical(old, row$data_id) && row$data_id %in% ad$data_id) {
      stop("There is an analysis data '", row$data_id, "' already.", call. = FALSE)
    }
    ad[i, ] <- row
    # a new name: what used the old one uses it
    if (!identical(old, row$data_id)) {
      a <- x$ard$analyses
      if (!is.null(a$data)) a$data[!is.na(a$data) & a$data == old] <- row$data_id
      a$denominator[!is.na(a$denominator) & a$denominator == old] <- row$data_id
      x$ard$analyses <- a
      ad$from[!is.na(ad$from) & ad$from == old] <- row$data_id
      ad$subjects[!is.na(ad$subjects) & ad$subjects == old] <- row$data_id
    }
  }
  x$ard$analysis_data <- ad
  x
}

#' @rdname set_analysis_data
#' @export
remove_analysis_data <- function(x, data_id) {
  ad <- .adata_rows(x)
  if (!data_id %in% ad$data_id) stop("No analysis data '", data_id, "'.", call. = FALSE)
  u <- .adata_uses(x, data_id)
  if (length(u$analyses) || length(u$data)) {
    stop("'", data_id, "' is used: ",
         paste(c(u$analyses, u$data), collapse = ", "), call. = FALSE)
  }
  x$ard$analysis_data <- ad[ad$data_id != data_id, , drop = FALSE]
  rownames(x$ard$analysis_data) <- NULL
  x
}

#' @rdname set_analysis_data
#' @param output_id The report.
#' @param dataset The dataset its analyses read (`NA`: the analysis set's
#'   own).
#' @export
name_analysis_data <- function(x, output_id, dataset, population_id,
                               data_id, label = NA) {
  a <- x$ard$analyses
  if (is.null(a$data)) a$data <- NA_character_
  same <- function(v, w) if (.is_blank(w)) .is_blank_v(v) else !.is_blank_v(v) & v == w
  pop_ds <- x$ard$populations$dataset[match(population_id, x$ard$populations$population_id)]
  ds_now <- if (.is_blank(dataset)) pop_ds else dataset
  # the report's analyses on it (not inside another: those take its data);
  # the analysis set's own dataset written out is the same data
  ds_of <- ifelse(.is_blank_v(a$dataset),
                  x$ard$populations$dataset[match(a$population_id,
                                                  x$ard$populations$population_id)],
                  a$dataset)
  par <- a$parent %||% rep(NA_character_, nrow(a))
  hit <- !is.na(a$output_id) & a$output_id == output_id & .is_blank_v(a$data) &
    .is_blank_v(par) & same(a$population_id, population_id) &
    (if (.is_blank(ds_now)) .is_blank_v(ds_of) else !is.na(ds_of) & ds_of == ds_now)
  if (!any(hit)) stop("No analysis of ", output_id, " reads that data.", call. = FALSE)
  # a condition every one of them has moves into the data
  w <- unique(a$where[hit])
  common <- if (length(w) == 1L && !is.na(w)) w else NA_character_
  x <- set_analysis_data(x, data_id, from = ds_now, population_id = population_id,
                         where = common, label = label)
  a$data[hit] <- data_id
  a$dataset[hit] <- NA
  a$population_id[hit] <- NA
  if (!is.na(common)) a$where[hit] <- NA
  x$ard$analyses <- a
  x
}

# The analysis data `id` as the ARD program makes it (tflspec writes the
# code), read from the study folder; NULL when it cannot be made
.adata_make <- function(x, path, id) {
  a <- x$ard
  a$analysis_data <- .adata_rows(x)
  a$analyses <- .normalize_ard_sheet(data.frame(
    output_id = ".try", analysis_id = "TRY", method = "cards::ard_summary",
    data = id, variables = "TRY_", stringsAsFactors = FALSE), "analyses")
  code <- tryCatch(tflspec::tfl_ard_code(structure(a, class = "tfl_ard_spec"),
                                         part = "body"),
                   error = function(e) NULL)
  if (is.null(code)) return(NULL)
  stop_at <- match("# ---- analyses", code)
  if (is.na(stop_at)) return(NULL)
  env <- new.env(parent = globalenv())
  old <- setwd(path)
  on.exit(setwd(old), add = TRUE)
  tryCatch({
    eval(parse(text = code[seq_len(stop_at - 1L)]), envir = env)
    as.data.frame(env[[id]])
  }, error = function(e) structure(list(), error = conditionMessage(e)))
}

# Its records and subjects, as words (`words`: records, one_row): one row a
# subject (a denominator can be one) is said
.adata_count_words <- function(d, words) {
  if (is.null(d)) return(NA_character_)
  if (!is.null(attr(d, "error"))) return(NA_character_)
  n <- if ("USUBJID" %in% names(d)) length(unique(d$USUBJID)) else nrow(d)
  if (nrow(d) > n) sprintf(words$records, nrow(d), n) else
    sprintf(words$one_row %||% words$subjects, n)
}

# The preview's columns: the subject, those the data's own columns name
# (its condition, added, derived, one row per), then the rest
.adata_preview_cols <- function(d, r, subj = "USUBJID") {
  named <- unique(c(subj, .split_bar(r$add), .split_bar(r$distinct),
                    sub("\\s*=.*$", "", .split_bar(r$derive)),
                    if (!.is_blank(r$where)) all.vars(str2lang(r$where))))
  front <- intersect(named, names(d))
  c(front, setdiff(names(d), front))
}

# What an analysis data is, in a line: "ADAE x SAF, TRTEMFL == "Y", +
# TRT01A, one row per USUBJID" (`words`: added, per)
.adata_words <- function(ad, id, words = list(added = "+ %s", per = "one row per %s")) {
  r <- ad[match(id, ad$data_id), , drop = FALSE]
  if (!nrow(r)) return("")
  src <- paste(c(r$from, if (!is.na(r$population_id)) r$population_id,
                 if (!is.na(r$subjects %||% NA)) sprintf(words$subj %||% "%s's subjects", r$subjects)),
               collapse = " \u00d7 ")
  bar <- function(v) paste(.split_bar(v), collapse = ", ")
  paste(c(src, if (!is.na(r$where)) r$where,
          if (!is.na(r$add)) sprintf(words$added, bar(r$add)),
          if (!is.na(r$derive)) bar(r$derive),
          if (!is.na(r$distinct)) sprintf(words$per, bar(r$distinct))),
        collapse = ", ")
}
