# The analysis data of a study's ARD definition (tflspec's sheet
# `analysis_data`): named data the analyses read -- made from a dataset or
# another analysis data, kept to a population's subjects and the records a
# condition keeps, with columns of the population's data added, columns
# derived, one row per set of values.  The study's: one name, one meaning,
# for every report.  The ARD tab shows a report's first (1. the analysis
# data, 2. the analyses); an analysis names one in `data`.

# the sheet (no rows when the study has none)
.adata_rows <- function(x, output_id = NULL) {
  d <- .normalize_ard_sheet(x$ard$analysis_data %||% data.frame(), "analysis_data")
  if (is.null(output_id)) d else
    d[!is.na(d$output_id) & d$output_id %in% output_id, , drop = FALSE]
}

# A report's rows of the sheet replaced by `rows` (in the place of its
# first row; a report with none yet: at the end).  An analysis data is a
# report's: its rows name the report.
.adata_set_rows <- function(x, output_id, rows) {
  ad <- .adata_rows(x)
  rows <- .normalize_ard_sheet(as.data.frame(rows, stringsAsFactors = FALSE), "analysis_data")
  rows$output_id <- rep(output_id, nrow(rows))
  mine <- !is.na(ad$output_id) & ad$output_id == output_id
  at <- if (any(mine)) which(mine)[1L] - 1L else nrow(ad)
  rest <- ad[!mine, , drop = FALSE]
  before <- sum(!mine[seq_len(at)])
  out <- rbind(rest[seq_len(before), , drop = FALSE], rows,
               rest[setdiff(seq_len(nrow(rest)), seq_len(before)), , drop = FALSE])
  rownames(out) <- NULL
  x$ard$analysis_data <- out
  x
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

# the datasets an analysis data is made from: the first one, its analysis
# set's, and those of the data whose subjects it keeps (in that order)
.adata_datasets <- function(ad, id, po = NULL, seen = character()) {
  out <- character()
  for (d in .adata_chain(ad, id)) {
    i <- match(d, ad$data_id)
    f <- ad$from[i]
    if (!f %in% ad$data_id) out <- c(out, f)
    p <- ad$population_id[i]
    if (!is.na(p) && !is.null(po)) out <- c(out, po$dataset[match(p, po$population_id)])
    s <- ad$subjects[i] %||% NA
    if (!is.na(s) && !s %in% c(seen, d)) out <- c(out, .adata_datasets(ad, s, po, c(seen, d)))
  }
  unique(out[!is.na(out)])
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

# The analysis data of one row a subject: made from the analysis sets'
# dataset (ADSL).  The others are kept to their subjects.
.adata_subject_level <- function(x, output_id) {
  ad <- .adata_rows(x, output_id)
  if (!nrow(ad)) return(character())
  ds <- unique(stats::na.omit(x$ard$populations$dataset))
  if (!length(ds)) ds <- "ADSL"
  ad$data_id[vapply(ad$data_id, function(id) .adata_dataset(ad, id) %in% ds, NA)]
}

# The subject key of the study (study sheet `id`, USUBJID by default)
.adata_subject_key <- function(x) {
  st <- x$ard$study
  v <- if (!is.null(st)) st$value[match("id", st$key)] else NA
  if (is.null(v) || !length(v) || is.na(v) || !nzchar(v)) "USUBJID" else v
}

# Keeps the analysis data `ids` to the subjects of `subj`: those kept to no
# subjects nor analysis set yet (themselves or the data they are made from).
# `subj` moves above the first of them (`subjects` names a data above),
# unless what it is made from is below: then only those below it.  The ids
# kept are in attribute "kept".
.adata_keep_to <- function(x, output_id, subj, ids) {
  ad <- .adata_rows(x, output_id)
  k <- match(subj, ad$data_id)
  free <- vapply(ids, function(id) id %in% ad$data_id && id != subj &&
                   !subj %in% .adata_chain(ad, id) && !id %in% .adata_chain(ad, subj) &&
                   is.na(.adata_subjects_of(ad, id)) && is.na(.adata_pop(ad, id)), NA)
  ids <- ids[free]
  # one made from another kept here follows it
  ids <- ids[!vapply(ids, function(id) any(setdiff(.adata_chain(ad, id), id) %in% ids), NA)]
  if (length(ids) && !is.na(k)) {
    up <- setdiff(.adata_chain(ad, subj), subj)
    first <- min(match(ids, ad$data_id))
    if (length(up) && max(match(up, ad$data_id)) >= first) {
      ids <- ids[match(ids, ad$data_id) > k]
      first <- if (length(ids)) min(match(ids, ad$data_id)) else k
    }
    if (length(ids)) {
      ad$subjects[match(ids, ad$data_id)] <- subj
      if (k > first) {
        ord <- c(setdiff(seq_len(first - 1L), k), k, setdiff(first:nrow(ad), k))
        ad <- ad[ord, , drop = FALSE]
      }
      rownames(ad) <- NULL
      x <- .adata_set_rows(x, output_id, ad)
    }
  }
  attr(x, "kept") <- ids
  x
}

# A name for a data from what it is made from, its analysis set and its
# condition: adsl_saf for ADSL of the analysis set SAF, or kept to an
# analysis set's condition, or to a flag set to "Y" (SAFFL == "Y"); NA
# when they say no such thing
.adata_name_from <- function(root, where, populations = NULL, pop = NA) {
  if (.is_blank(root)) return(NA_character_)
  if (!.is_blank(pop)) return(paste0(tolower(root), "_", tolower(pop)))
  if (.is_blank(where)) return(NA_character_)
  w <- trimws(where)
  k <- if (!is.null(populations)) match(w, trimws(populations$where)) else NA
  suf <- if (!is.na(k)) populations$population_id[k] else {
    m <- regmatches(w, regexec('^([A-Za-z0-9_]+)FL\\s*(==|%in%)\\s*"Y"$', w))[[1L]]
    if (length(m)) m[2L] else NA_character_
  }
  if (is.na(suf)) NA_character_ else paste0(tolower(root), "_", tolower(suf))
}

# A condition's terms: what `&` joins, outermost first (NULL when it does
# not read as R)
.cond_terms <- function(where) {
  if (.is_blank(where)) return(list())
  e <- tryCatch(str2lang(where), error = function(err) NULL)
  if (is.null(e)) return(NULL)
  out <- list()
  while (is.call(e) && identical(e[[1L]], as.name("&"))) {
    out <- c(list(e[[3L]]), out)
    e <- e[[2L]]
  }
  c(list(e), out)
}

# The first term of a condition, as R writes it (NA: none, or not R)
.cond_first <- function(where) {
  tm <- .cond_terms(where)
  if (!length(tm)) NA_character_ else deparse1(tm[[1L]])
}

# A condition whose first term is `first`: a first term among `known` (an
# analysis set's) is replaced, else `first` goes before the rest; `first`
# NA takes a known first term out.  A condition that is not R stays.
.cond_set_first <- function(where, first, known = character()) {
  tm <- .cond_terms(where)
  if (is.null(tm)) return(where)
  kn <- vapply(known, function(k) .cond_first(k), "")
  if (length(tm) && deparse1(tm[[1L]]) %in% kn) tm <- tm[-1L]
  if (!.is_blank(first)) tm <- c(list(str2lang(first)), tm)
  .cond_join(tm)
}

# Terms put back as one condition (NA: none)
.cond_join <- function(tm) {
  if (!length(tm)) return(NA_character_)
  paste(vapply(tm, function(x) {
    d <- deparse1(x)
    if (is.call(x) && identical(x[[1L]], as.name("|"))) paste0("(", d, ")") else d
  }, ""), collapse = " & ")
}

# The analysis set `pop` taken out of a condition whose first rows are its
# condition as it is: list(pop, where = the rest).  Changed (another value,
# "!="), or not first: pop NA and the condition as it is.
.cond_take_pop <- function(where, populations, pop) {
  out <- list(pop = NA_character_, where = if (.is_blank(where)) NA_character_ else where)
  if (.is_blank(pop) || is.null(populations)) return(out)
  pt <- .cond_terms(populations$where[match(pop, populations$population_id)])
  wt <- .cond_terms(where)
  if (!length(pt) || is.null(wt) || length(wt) < length(pt)) return(out)
  k <- seq_along(pt)
  if (!identical(unname(vapply(wt[k], deparse1, "")), unname(vapply(pt, deparse1, "")))) {
    return(out)
  }
  list(pop = pop, where = .cond_join(wt[-k]))
}

# A condition with an analysis set's condition as its first rows (NA when
# either is not R, or the set has none)
.cond_put_pop <- function(where, pop_where) {
  pt <- .cond_terms(pop_where)
  wt <- .cond_terms(where)
  if (!length(pt) || is.null(wt)) return(NA_character_)
  .cond_join(c(pt, wt))
}

# An analysis data already there that is this same data (made from the
# same, the same subjects, analysis set and condition, nothing else): its
# name, else NA
.adata_same_as <- function(ad, from, pop, subjects, where, but = NULL) {
  if (is.null(ad) || !nrow(ad) || .is_blank(from)) return(NA_character_)
  norm <- function(v) {
    if (.is_blank(v)) return(NA_character_)
    tm <- .cond_terms(v)
    if (is.null(tm)) trimws(v) else .cond_join(tm)
  }
  val <- function(v) if (.is_blank(v)) NA_character_ else as.character(v)
  for (i in seq_len(nrow(ad))) {
    if (!is.null(but) && identical(ad$data_id[i], but)) next
    other <- c("add", "derive", "keep", "distinct", "code")
    if (any(vapply(intersect(other, names(ad)), function(cn) !.is_blank(ad[[cn]][i]), NA))) next
    if (identical(val(ad$from[i]), val(from)) &&
        identical(val(ad$population_id[i]), val(pop)) &&
        identical(val(ad$subjects[i]), val(subjects)) &&
        identical(norm(ad$where[i]), norm(where))) {
      return(ad$data_id[i])
    }
  }
  NA_character_
}

# An analysis set's id for a population flag (SAFFL: SAF, PPROTFL: PP),
# not one the study has
.population_id_for <- function(flag, taken = character()) {
  known <- c(SAFFL = "SAF", ITTFL = "ITT", FASFL = "FAS", PPROTFL = "PP",
             RANDFL = "RAND", ENRLFL = "ENRL", COMPLFL = "COMPL", MITTFL = "MITT",
             PPSFL = "PPS", PKFL = "PK")
  id <- if (flag %in% names(known)) known[[flag]] else sub("FL$", "", flag)
  base <- id
  k <- 1L
  while (id %in% taken) {
    k <- k + 1L
    id <- paste0(base, k)
  }
  id
}

# The opposite of a condition, the rows it does not keep: a blank flag too
# (SAFFL != "Y" would drop the rows where SAFFL is blank)
.cond_not <- function(w) {
  m <- regmatches(w, regexec('^\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*==\\s*("[^"]*")\\s*$', w))[[1L]]
  if (length(m)) sprintf("!(%s %%in%% %s)", m[2L], m[3L]) else
    sprintf("!((%s) %%in%% TRUE)", w)
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
  .adata_rows(x, output_id)$data_id
}

# Where an analysis data is used: the analyses that read it or divide by it
# ("T1 / AE"), and the analysis data made from it
.adata_uses <- function(x, output_id, id) {
  a <- x$ard$analyses
  a <- a[!is.na(a$output_id) & a$output_id == output_id, , drop = FALSE]
  d <- a$data %||% rep(NA_character_, nrow(a))
  hit <- (!is.na(d) & d == id) |
    (!is.na(a$denominator) & a$denominator == id)
  ad <- .adata_rows(x, output_id)
  list(analyses = paste(a$output_id[hit], a$analysis_id[hit], sep = " / "),
       data = ad$data_id[(!is.na(ad$from) & ad$from == id) |
                           (!is.na(ad$subjects) & ad$subjects == id)])
}

# A name for a new analysis data: the dataset (or data) and the value a
# condition keeps (`AVISIT == "Week 24"`: advs_week24), else the analysis
# set; lower case, and not a name the program gives anything (a dataset, a
# population, a dataset x analysis set's own: adae_saf)
# The names the program has already: the datasets, the analysis sets, the
# data a dataset x analysis set is read as (adae_saf), the analysis data
.adata_taken_names <- function(x, output_id) {
  rn <- function(v) gsub("[^a-z0-9_.]", "_", tolower(v))
  ds <- x$ard$datasets$dataset
  pops <- x$ard$populations$population_id
  auto <- unlist(lapply(pops, function(p) vapply(ds, function(d)
    .an_data_name(d, p, x$ard$populations), "")))
  c(rn(ds), paste0("pop_", rn(pops)), auto,
    .adata_rows(x, output_id)$data_id, "data", "population", "ard", "ards", "status")
}

.adata_suggest <- function(x, output_id, from, population_id = NA, where = NA) {
  rn <- function(v) gsub("[^a-z0-9_.]", "_", tolower(v))
  val <- if (!.is_blank(where)) regmatches(where, regexpr("\"[^\"]+\"", where))
  val <- if (length(val)) gsub("[^a-z0-9]", "", tolower(val)) else ""
  base <- paste(c(rn(from), if (nzchar(val)) val else
    if (!.is_blank(population_id)) rn(population_id)), collapse = "_")
  if (!grepl("^[a-z]", base)) base <- paste0("d_", base)
  taken <- .adata_taken_names(x, output_id)
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
#' (`derive`) and one row per set of values (`distinct`) -- or, when the
#' columns cannot say it, made by R of its own (`code`, the other columns
#' but `from` blank).  An analysis names one in `data` (instead of
#' `dataset` / `population_id`), or as its `denominator`.
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
#' @param code R that makes the data itself (its value is the data); with
#'   it the other columns but `from` stay blank.
#' @return The `tflplanner`.
#' @export
set_analysis_data <- function(x, output_id, data_id, from, population_id = NA,
                              where = NA, add = NA, derive = NA,
                              distinct = NA, label = NA, old = NULL,
                              subjects = NA, keep = NA, code = NA) {
  if (.is_blank(output_id)) stop("An analysis data is a report's: give the report.", call. = FALSE)
  ad <- .adata_rows(x, output_id)
  row <- .normalize_ard_sheet(data.frame(
    output_id = output_id, data_id = data_id, label = label, from = from,
    population_id = population_id, subjects = subjects, where = where,
    add = add, derive = derive, keep = keep, distinct = distinct,
    code = code, stringsAsFactors = FALSE), "analysis_data")
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
    # a new name: what used the old one (in this report) uses it
    if (!identical(old, row$data_id)) {
      a <- x$ard$analyses
      k <- !is.na(a$output_id) & a$output_id == output_id
      if (!is.null(a$data)) a$data[k & !is.na(a$data) & a$data == old] <- row$data_id
      a$denominator[k & !is.na(a$denominator) & a$denominator == old] <- row$data_id
      x$ard$analyses <- a
      ad$from[!is.na(ad$from) & ad$from == old] <- row$data_id
      ad$subjects[!is.na(ad$subjects) & ad$subjects == old] <- row$data_id
    }
  }
  .adata_set_rows(x, output_id, ad)
}

#' @rdname set_analysis_data
#' @export
remove_analysis_data <- function(x, output_id, data_id) {
  ad <- .adata_rows(x, output_id)
  if (!data_id %in% ad$data_id) {
    stop("No analysis data '", data_id, "' in ", output_id, ".", call. = FALSE)
  }
  u <- .adata_uses(x, output_id, data_id)
  if (length(u$analyses) || length(u$data)) {
    stop("'", data_id, "' is used: ",
         paste(c(u$analyses, u$data), collapse = ", "), call. = FALSE)
  }
  .adata_set_rows(x, output_id, ad[ad$data_id != data_id, , drop = FALSE])
}

#' @rdname set_analysis_data
#' @param from_output The report whose analysis data are copied.
#' @param data_ids Which of them (`NULL`: all), with what they are made
#'   from and kept to, when those are analysis data as well.
#' @details `import_analysis_data()` copies a report's analysis data into
#'   another report, under the same names (a name the report has already
#'   gets `_1`, `_2` ...); attribute `copied` names the rows added.
#' @export
import_analysis_data <- function(x, from_output, output_id, data_ids = NULL) {
  src <- .adata_rows(x, from_output)
  if (is.null(data_ids)) data_ids <- src$data_id
  miss <- setdiff(data_ids, src$data_id)
  if (length(miss)) {
    stop("No analysis data ", paste(miss, collapse = ", "), " in ", from_output, ".",
         call. = FALSE)
  }
  # with what they are made from and kept to (those of the same report)
  want <- character()
  ids <- data_ids
  while (length(new <- setdiff(unique(unlist(lapply(ids, function(id) .adata_chain(src, id)))),
                               want))) {
    want <- c(want, new)
    ids <- intersect(stats::na.omit(src$subjects[match(new, src$data_id)]), src$data_id)
  }
  rows <- src[src$data_id %in% want, , drop = FALSE]
  # the names: the same, unless the report's program has them
  taken <- .adata_names_in_use(x, output_id)
  new_id <- vapply(rows$data_id, function(id) {
    nm <- id
    k <- 1L
    while (nm %in% taken) {
      nm <- paste0(id, "_", k)
      k <- k + 1L
    }
    taken <<- c(taken, nm)
    nm
  }, "")
  ren <- function(v) ifelse(!is.na(v) & v %in% rows$data_id, new_id[match(v, rows$data_id)], v)
  rows$from <- ren(rows$from)
  rows$subjects <- ren(rows$subjects)
  rows$data_id <- unname(new_id)
  x <- .adata_set_rows(x, output_id, rbind(.adata_rows(x, output_id), rows))
  attr(x, "copied") <- unname(new_id)
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
  x <- set_analysis_data(x, output_id, data_id, from = ds_now, population_id = population_id,
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
.adata_make <- function(x, path, output_id, id) {
  a <- x$ard
  a$analysis_data <- .adata_rows(x, output_id)
  a$analysis_data$output_id <- rep(".try", nrow(a$analysis_data))
  a$analyses <- .normalize_ard_sheet(data.frame(
    output_id = ".try", analysis_id = "TRY", method = "cards::ard_summary",
    data = id, variables = "TRY_", stringsAsFactors = FALSE), "analyses")
  code <- tryCatch(tflspec::tfl_ard_code(structure(a, class = "tfl_ard_spec"),
                                         part = "body"),
                   error = function(e) NULL)
  if (is.null(code)) return(NULL)
  stop_at <- match("# ---- analyses ----", code)
  if (is.na(stop_at)) return(NULL)
  env <- new.env(parent = .ard_program_env())
  old <- setwd(path)
  on.exit(setwd(old), add = TRUE)
  tryCatch({
    # its warnings (a file not there yet ...) are not the app's: the
    # error, if any, is said with the preview
    suppressWarnings(eval(parse(text = code[seq_len(stop_at - 1L)]), envir = env))
    as.data.frame(env[[id]])
  }, error = function(e) structure(list(), error = conditionMessage(e)))
}

# Where an ARD program's lines run in the app: as under its setup, with
# cards', dplyr's and tflspec's functions (the setup attaches them), the
# app's own session left as it is
.ard_program_env <- function() {
  # the functions the programs call (set_levels() ...), then the packages'
  e <- .helpers_env(globalenv())
  for (pk in c("tflspec", "dplyr", "cards")) {
    ns <- asNamespace(pk)
    e <- list2env(mget(getNamespaceExports(ns), envir = ns, inherits = TRUE),
                  parent = e)
  }
  e
}

# The lines the program makes a data with, as R its `code` can start from:
# the lines that write it, its name last (the value of the code); NA when
# the program cannot be written yet
.adata_code_start <- function(x, output_id, id) {
  a <- x$ard
  a$analysis_data <- .adata_rows(x, output_id)
  a$analysis_data$output_id <- rep(".try", nrow(a$analysis_data))
  a$analyses <- .normalize_ard_sheet(data.frame(
    output_id = ".try", analysis_id = "TRY", method = "cards::ard_summary",
    data = id, variables = "TRY_", stringsAsFactors = FALSE), "analyses")
  code <- tryCatch(tflspec::tfl_ard_code(structure(a, class = "tfl_ard_spec"),
                                         part = "body"),
                   error = function(e) NULL)
  if (is.null(code)) return(NA_character_)
  mine <- code[startsWith(code, paste0(id, " <- "))]
  if (!length(mine)) return(NA_character_)
  paste(c(mine, id), collapse = "\n")
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

#' @rdname set_analysis_data
#' @export
copy_analysis_data <- function(x, output_id, data_id) {
  ad <- .adata_rows(x, output_id)
  i <- match(data_id, ad$data_id)
  if (is.na(i)) stop("No analysis data '", data_id, "' in ", output_id, ".", call. = FALSE)
  k <- 2L
  while (paste0(data_id, "_", k) %in% ad$data_id) k <- k + 1L
  new <- ad[i, , drop = FALSE]
  new$data_id <- paste0(data_id, "_", k)
  x <- .adata_set_rows(x, output_id, rbind(ad[seq_len(i), , drop = FALSE], new,
                                           ad[seq_len(nrow(ad)) > i, , drop = FALSE]))
  attr(x, "copied") <- new$data_id
  x
}
