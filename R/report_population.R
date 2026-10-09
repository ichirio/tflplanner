# A report's analysis set: one value, the report list's `population`
# (outputs$population, a population_id).  The TOC fills it, the report list
# and step 1 change it; step 1's defaults read it.  The analysis data of the
# set's subjects (adsl_<set>) is made when a report's set is chosen, and a
# report whose set changes has its analyses moved to the new set's data.

# a report's analysis set (NA: none given)
report_population <- function(x, output_id) {
  o <- x$outputs
  v <- if (is.null(o$population)) NA_character_ else
    o$population[match(output_id, o$output_id)]
  if (length(v) != 1L || .is_blank(v)) NA_character_ else v
}

# An analysis set's label: the company standards' (their populations
# sheet's note), else the label of its flag in `data` (SAFFL: "Safety
# Population Flag"), else its id
.pop_label <- function(x, pop, data = NULL) {
  po <- x$ard$populations
  k <- match(pop, po$population_id)
  if (is.na(k)) return(pop)
  std <- tryCatch(company_standards()$populations, error = function(e) NULL)
  note <- if (!is.null(std$note)) std$note[match(pop, std$population_id)] else NA
  if (length(note) == 1L && !.is_blank(note)) return(note)
  fl <- .pop_flag(po$where[k])
  if (!is.na(fl) && !is.null(data) && fl %in% names(data)) {
    l <- attr(data[[fl]], "label")
    if (length(l) == 1L && !is.na(l) && nzchar(l)) return(l)
  }
  pop
}

# the flag an analysis set's condition is (SAFFL == "Y": SAFFL); NA if it
# is not one flag set to "Y"
.pop_flag <- function(where) {
  if (.is_blank(where)) return(NA_character_)
  m <- regmatches(where, regexec('^\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*(==|%in%)\\s*"Y"\\s*$', where))[[1L]]
  if (length(m)) m[2L] else NA_character_
}

# The analysis data of an analysis set's subjects: the one there (made from
# its dataset, its set, nothing else), else adsl_<set> is added.  The planner
# with it; attr "data_id" its name, "added" TRUE when it was made.
.ensure_pop_adata <- function(x, output_id, pop) {
  po <- x$ard$populations
  k <- match(pop, po$population_id)
  if (is.na(k)) stop("No population '", pop, "'.", call. = FALSE)
  from <- po$dataset[k]
  if (.is_blank(from)) from <- "ADSL"
  have <- .adata_same_as(.adata_rows(x, output_id), from, pop, NA, NA)
  if (!is.na(have)) {
    attr(x, "data_id") <- have
    attr(x, "added") <- FALSE
    return(x)
  }
  nm <- paste0(gsub("[^a-z0-9_.]", "_", tolower(from)), "_",
               gsub("[^a-z0-9_.]", "_", tolower(pop)))
  nm <- .adata_free_name(x, output_id, nm)
  lab <- .pop_label(x, pop)
  x <- set_analysis_data(x, output_id, nm, from = from, population_id = pop,
                         label = if (identical(lab, pop)) NA else lab)
  attr(x, "data_id") <- nm
  attr(x, "added") <- TRUE
  x
}

# The counterpart of analysis data `id` for analysis set `to` (its set
# `from`, or the data whose subjects it keeps, moved to `to`'s): the one
# there with that definition, else one made.  NA when it is not of that
# kind (made from another analysis data, written as R, of no set).  The
# planner with attr "data_id".
.adata_for_pop <- function(x, output_id, id, from_pop, to_pop) {
  ad <- .adata_rows(x, output_id)
  i <- match(id, ad$data_id)
  out <- function(x, v) {
    attr(x, "data_id") <- v
    x
  }
  if (is.na(i)) return(out(x, NA_character_))
  r <- ad[i, , drop = FALSE]
  if (!.is_blank(r$code) || r$from %in% ad$data_id) return(out(x, NA_character_))
  if (identical(r$population_id, from_pop) && .is_blank(r$subjects)) {
    pop <- to_pop
    subj <- NA_character_
  } else if (!.is_blank(r$subjects)) {
    x <- .adata_for_pop(x, output_id, r$subjects, from_pop, to_pop)
    subj <- attr(x, "data_id")
    if (is.na(subj)) return(out(x, NA_character_))
    pop <- NA_character_
  } else {
    return(out(x, NA_character_))
  }
  # the subjects' own data of a set: adsl_<set>, made as a set's is
  if (!is.na(pop) && .is_blank(r$where) && .is_blank(r$add) && .is_blank(r$derive) &&
      .is_blank(r$keep) && .is_blank(r$distinct)) {
    x <- .ensure_pop_adata(x, output_id, pop)
    return(out(x, attr(x, "data_id")))
  }
  ad <- .adata_rows(x, output_id)
  same <- vapply(seq_len(nrow(ad)), function(j) {
    identical(ad$from[j], r$from) && identical(.blank_na(ad$population_id[j]), .blank_na(pop)) &&
      identical(.blank_na(ad$subjects[j]), .blank_na(subj)) &&
      all(vapply(c("where", "add", "derive", "keep", "distinct"), function(cn)
        identical(.blank_na(ad[[cn]][j]), .blank_na(r[[cn]])), NA)) &&
      .is_blank(ad$code[j])
  }, NA)
  if (any(same)) return(out(x, ad$data_id[which(same)[1L]]))
  # its name with the set's part changed (adae_saf: adae_itt), else the set's added
  lo <- function(v) gsub("[^a-z0-9_.]", "_", tolower(v))
  base <- if (endsWith(id, paste0("_", lo(from_pop)))) {
    paste0(substr(id, 1L, nchar(id) - nchar(from_pop)), lo(to_pop))
  } else paste0(id, "_", lo(to_pop))
  nm <- .adata_free_name(x, output_id, base)
  x <- set_analysis_data(x, output_id, nm, from = r$from, population_id = pop, subjects = subj,
                         where = r$where, add = r$add, derive = r$derive, keep = r$keep,
                         distinct = r$distinct, label = r$label)
  out(x, nm)
}

# `base`, else base_1, base_2 ...: a name no data of the program has (the
# datasets, the sets' pop_<id>, the analysis data, and the names of the
# dataset x set data the analyses read)
.adata_free_name <- function(x, output_id, base) {
  taken <- .adata_names_in_use(x, output_id)
  nm <- base
  k <- 1L
  while (nm %in% taken) {
    nm <- paste0(base, "_", k)
    k <- k + 1L
  }
  nm
}

# The names a report's program has: the datasets, the sets' pop_<id>, the
# report's analysis data, and the names of the dataset x set data its
# analyses read
.adata_names_in_use <- function(x, output_id) {
  rn <- function(v) gsub("[^a-z0-9_.]", "_", tolower(v))
  a <- x$ard$analyses
  a <- a[!is.na(a$output_id) & a$output_id == output_id, , drop = FALSE]
  used <- if (nrow(a)) unique(vapply(seq_len(nrow(a)), function(i)
    .an_data_name(a$dataset[i], a$population_id[i], x$ard$populations), "")) else character()
  c(rn(x$ard$datasets$dataset), paste0("pop_", rn(x$ard$populations$population_id)),
    used, .adata_rows(x, output_id)$data_id, "data", "population", "ard", "ards", "status")
}

# How many subjects of `d` have another `flag` than in the subjects' data
# `subj` (by `key`); NA when either has no such column
.flag_disagrees <- function(d, subj, flag, key = "USUBJID") {
  if (is.null(d) || is.null(subj) || !all(c(flag, key) %in% names(d)) ||
      !all(c(flag, key) %in% names(subj))) return(NA_integer_)
  v <- unique(d[c(key, flag)])
  s <- subj[[flag]][match(v[[key]], subj[[key]])]
  norm <- function(x) ifelse(is.na(x), "", as.character(x))
  length(unique(v[[key]][norm(v[[flag]]) != norm(s)]))
}

# The words a TOC may use for an analysis set, by its flag
.pop_words <- list(
  SAFFL = c("safety", "safety population", "safety set", "safety analysis set",
            "safety analysis population", "saf"),
  ITTFL = c("intent-to-treat", "intent to treat", "intention-to-treat",
            "intention to treat", "itt", "itt population", "itt set"),
  FASFL = c("full analysis set", "full analysis population", "fas"),
  PPROTFL = c("per-protocol", "per protocol", "per-protocol set", "per-protocol population",
              "per protocol set", "per protocol population", "pp", "pps"),
  EFFFL = c("efficacy", "efficacy population", "efficacy set", "efficacy analysis set"),
  ENRLFL = c("enrolled", "enrolled set", "enrolled population", "all enrolled"),
  RANDFL = c("randomized", "randomised", "randomized set", "randomized population",
             "all randomized"))

# A TOC's word for an analysis set ("Safety Population") as a population_id
# of the study: its id, its label (.pop_label()), a usual word for its flag;
# "Safety Population (SAF)" by the id in it.  NA when none.
.pop_match <- function(x, text, data = NULL) {
  if (.is_blank(text)) return(NA_character_)
  norm <- function(v) gsub("\\s+", " ", trimws(tolower(v)))
  w <- norm(text)
  po <- x$ard$populations
  ids <- po$population_id[!is.na(po$population_id)]
  if (!length(ids)) return(NA_character_)
  hit <- ids[norm(ids) == w]
  if (length(hit)) return(hit[1L])
  labs <- vapply(ids, function(id) norm(.pop_label(x, id, data)), "")
  hit <- ids[labs == w]
  if (length(hit)) return(hit[1L])
  flags <- vapply(po$where[match(ids, po$population_id)], .pop_flag, "")
  # "... Population", "... Analysis Set" said or not
  w2 <- sub(" (analysis )?(population|set)$", "", w)
  for (k in seq_along(ids)) {
    words <- .pop_words[[flags[k]]] %||% character()
    if (!is.na(flags[k]) && any(c(w, w2) %in% words)) return(ids[k])
  }
  # an id in brackets or as a word ("Safety Population (SAF)")
  for (id in ids) {
    if (grepl(paste0("(^|[^a-z0-9])", norm(id), "($|[^a-z0-9])"), w)) return(id)
  }
  NA_character_
}

.blank_na <- function(v) if (.is_blank(v)) NA_character_ else as.character(v)

#' A report's analysis set
#'
#' The one value the report list, the TOC and step 1 share: `set_report_population()`
#' writes it, makes the analysis data of the set's subjects (`adsl_<set>`)
#' when there is none, and moves the report's analyses from the set it had
#' to the new one: the data of the old set's subjects, and the data kept to
#' them, to the new set's (found, or made with the same definition); an
#' analysis of a dataset and the old set to the new set.  An analysis data
#' of another kind (made from another analysis data, written as R) is left
#' as it is and named in attribute `left`.  Analysis data are the study's:
#' none is removed.
#'
#' @param x A `tflplanner`.
#' @param output_id The report.
#' @param population A population_id of the study (`NA`: none).
#' @return The `tflplanner`, with attributes `made` (the analysis data
#'   added) and `left`.
#' @export
set_report_population <- function(x, output_id, population) {
  o <- x$outputs
  i <- match(output_id, o$output_id)
  if (is.na(i)) stop("No report '", output_id, "'.", call. = FALSE)
  new <- if (.is_blank(population)) NA_character_ else population
  if (!is.na(new) && !new %in% x$ard$populations$population_id) {
    stop("No population '", new, "' in the study.", call. = FALSE)
  }
  old <- report_population(x, output_id)
  if (is.null(o$population)) o$population <- rep(NA_character_, nrow(o))
  o$population[i] <- new
  x$outputs <- o
  before <- .adata_rows(x, output_id)$data_id
  left <- character()
  if (!is.na(new)) {
    x <- .ensure_pop_adata(x, output_id, new)
    if (!is.na(old) && !identical(old, new)) {
      a <- x$ard$analyses
      mine <- which(!is.na(a$output_id) & a$output_id == output_id)
      moved <- list()
      move <- function(id) {
        if (.is_blank(id) || !id %in% .adata_rows(x, output_id)$data_id) return(id)
        if (!is.null(moved[[id]])) return(moved[[id]])
        x <<- .adata_for_pop(x, output_id, id, old, new)
        v <- attr(x, "data_id")
        if (is.na(v)) {
          if (!id %in% left) left <<- c(left, id)
          v <- id
        }
        moved[[id]] <<- v
        v
      }
      for (j in mine) {
        if (!is.null(a$data)) a$data[j] <- move(a$data[j])
        a$denominator[j] <- move(a$denominator[j])
        if (identical(a$population_id[j], old)) a$population_id[j] <- new
        if (identical(a$denominator[j], old)) a$denominator[j] <- new
      }
      x$ard$analyses <- a
    }
  }
  attr(x, "data_id") <- NULL
  attr(x, "added") <- NULL
  attr(x, "made") <- setdiff(.adata_rows(x, output_id)$data_id, before)
  attr(x, "left") <- left
  x
}
