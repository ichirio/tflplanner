# The data an analysis reads, as one choice on the ARD tab: a dataset and an
# analysis set.  The ARD definition keeps its two columns (`dataset`,
# `population_id`; a blank dataset is the analysis set's own), and the choice
# is named as the program tflspec writes names the data (`pop_saf`,
# `adae_saf`, `adsl`).

# one choice's value: "<dataset>|<population_id>", either part blank
.an_data_value <- function(dataset, population_id) {
  b <- function(x) if (is.null(x) || !length(x) || is.na(x[1L])) "" else x[1L]
  paste0(b(dataset), "|", b(population_id))
}

# an analysis data's value: "@<data_id>"
.an_adata_value <- function(id) paste0("@", id)

# the analysis's present choice: its analysis data, else its dataset x set
.an_data_value_row <- function(r) {
  d <- r$data %||% NA_character_
  if (length(d) && !is.na(d[1L]) && nzchar(d[1L])) return(.an_adata_value(d[1L]))
  .an_data_value(r$dataset, r$population_id)
}

# a value back to its parts (NA for a blank part): `dataset`, `pop` and,
# for an analysis data (`adata`: the sheet), `data` -- its dataset and
# analysis set those it is made from
.an_data_split <- function(value, adata = NULL) {
  value <- value %||% ""
  if (startsWith(value, "@")) {
    id <- substring(value, 2L)
    ad <- adata %||% data.frame(data_id = character(), from = character(),
                                population_id = character())
    return(list(dataset = .adata_dataset(ad, id), pop = .adata_pop(ad, id), data = id))
  }
  v <- strsplit(paste0(value, "|"), "|", fixed = TRUE)[[1L]]
  na <- function(x) if (is.na(x) || !nzchar(x)) NA_character_ else x
  list(dataset = na(v[1L]), pop = na(v[2L]), data = NA_character_)
}

# the name the program gives the data (tflspec's rule, without a subset):
# the analysis set's own data is the population, another dataset is its
# records of the analysis set's subjects
.an_data_name <- function(dataset, population_id, populations) {
  rn <- function(x) make.names(tolower(x))
  has <- function(x) length(x) && !is.na(x) && nzchar(x)
  if (!has(population_id)) return(if (has(dataset)) rn(dataset) else "")
  pds <- populations$dataset[match(population_id, populations$population_id)]
  if (!has(dataset) || identical(dataset, pds)) {
    return(paste0("pop_", rn(population_id)))
  }
  paste(rn(dataset), rn(population_id), sep = "_")
}

# The choices: each analysis set's own data, then every other dataset with
# it, then every dataset alone; the analysis's present choice is kept when
# it is none of these.  `words`: `with` ("%s x %s (%s)"), `alone`
# ("%s, no analysis set (%s)"), `none`; `counts` (from .an_data_counts(),
# named by value) are put after the name when given: `words$count`
# ("%s: %s").
.an_data_choices <- function(datasets, populations, now = "|", words, counts = NULL,
                             adata = NULL, first = character(), adata_counts = NULL) {
  datasets <- unique(datasets[!is.na(datasets) & nzchar(datasets)])
  po <- populations[!is.na(populations$population_id), , drop = FALSE]
  val <- character()
  lab <- character()
  add <- function(ds, pop) {
    v <- .an_data_value(ds, pop)
    if (v %in% val) return()
    nm <- .an_data_name(ds, pop, populations)
    if (!is.null(counts) && !is.na(counts[v])) nm <- sprintf(words$count %||% "%s: %s", nm, counts[[v]])
    pds <- po$dataset[match(pop, po$population_id)]
    shown <- if (is.na(ds) || !nzchar(ds)) pds else ds
    l <- if (is.na(pop)) sprintf(words$alone, ds, nm) else
      sprintf(words$with, if (is.na(shown)) "?" else shown, pop, nm)
    val <<- c(val, v)
    lab <<- c(lab, l)
  }
  for (i in seq_len(nrow(po))) {
    p <- po$population_id[i]
    add(NA_character_, p)
    for (d in setdiff(datasets, po$dataset[i])) add(d, p)
  }
  for (d in datasets) add(d, NA_character_)
  # the analysis set's own dataset written out ("ADSL|SAF") is the same
  # data as the blank one ("|SAF"): that choice keeps the analysis's value
  x <- .an_data_split(now)
  same <- match(.an_data_value(NA, x$pop), val)
  if (!now %in% val && !is.na(x$pop) && !is.na(same) &&
      identical(x$dataset, po$dataset[match(x$pop, po$population_id)])) {
    val[same] <- now
  }
  # the analysis data: the report's first, then the study's others; in
  # groups (`words$groups`: the report's, the study's others, the rest)
  if (!is.null(adata) && nrow(adata)) {
    ids <- c(intersect(first, adata$data_id), setdiff(adata$data_id, first))
    av <- .an_adata_value(ids)
    al <- vapply(ids, function(id) {
      n <- adata_counts[id]
      nm <- if (!is.null(adata_counts) && !is.na(n)) sprintf(words$count %||% "%s: %s", id, n) else id
      sprintf(words$named %||% "%s (%s)", .adata_words(adata, id, words), nm)
    }, "")
    if (!is.null(words$groups)) {
      mine <- ids %in% first
      out <- list()
      if (any(mine)) out[[words$groups[[1L]]]] <- stats::setNames(av[mine], al[mine])
      if (any(!mine)) out[[words$groups[[2L]]]] <- stats::setNames(av[!mine], al[!mine])
      rest <- .an_data_choices(datasets, populations, now = if (startsWith(now, "@")) "|" else now,
                               words = words, counts = counts)
      if (startsWith(now, "@") && !now %in% av) {
        out[[words$groups[[1L]]]] <- c(out[[words$groups[[1L]]]],
                                      stats::setNames(now, substring(now, 2L)))
      }
      out[[words$groups[[3L]]]] <- rest[rest != "|" | !startsWith(now, "@")]
      return(out)
    }
    val <- c(av, val)
    lab <- c(unname(al), lab)
  }
  if (!now %in% val) {
    if (startsWith(now, "@")) {
      val <- c(now, val)
      lab <- c(substring(now, 2L), lab)
    } else if (is.na(x$dataset) && is.na(x$pop)) {
      val <- c(now, val)
      lab <- c(words$none, lab)
    } else {
      add(x$dataset, x$pop)
    }
  }
  stats::setNames(val, lab)
}

# The subjects (and records) each data choice reads, counted once: the
# analysis set's own data, another dataset cut to its subjects, a dataset
# alone -- as the ARD programs make them.  `words`: `subjects` ("%d
# subjects"), `records` ("%d records, %d subjects").  Named by value
# (.an_data_value()); a dataset that cannot be read is left out.
.an_data_counts <- function(study, words = list(subjects = "%d subjects",
                                                records = "%d records, %d subjects")) {
  ds <- study$planner$ard$datasets
  po <- study$planner$ard$populations
  po <- po[!is.na(po$population_id), , drop = FALSE]
  read <- function(d) {
    pth <- ds$path[match(d, ds$dataset)]
    if (is.na(pth)) return(NULL)
    f <- file.path(study$path, pth)
    if (!file.exists(f)) return(NULL)
    tryCatch(as.data.frame(read_data_head(f, n = .Machine$integer.max)),
             error = function(e) NULL)
  }
  data <- list()
  get <- function(d) {
    if (is.null(data[[d]])) data[[d]] <<- read(d) %||% FALSE
    if (isFALSE(data[[d]])) NULL else data[[d]]
  }
  subj <- function(x) if ("USUBJID" %in% names(x)) length(unique(x$USUBJID)) else nrow(x)
  # records only when there are more than subjects
  both <- function(x) {
    n <- subj(x)
    if (nrow(x) > n) sprintf(words$records, nrow(x), n) else sprintf(words$subjects, n)
  }
  out <- character()
  for (i in seq_len(nrow(po))) {
    p <- po$population_id[i]
    base <- get(po$dataset[i])
    if (is.null(base)) next
    keep <- tryCatch(with(base, eval(parse(text = po$where[i]))), error = function(e) NULL)
    if (is.null(keep) && !is.na(po$where[i])) next
    pop <- if (is.null(keep)) base else base[!is.na(keep) & keep, , drop = FALSE]
    out[.an_data_value(NA, p)] <- sprintf(words$subjects, subj(pop))
    for (d in setdiff(ds$dataset, po$dataset[i])) {
      x <- get(d)
      if (is.null(x) || !"USUBJID" %in% names(x) || !"USUBJID" %in% names(pop)) next
      x <- x[x$USUBJID %in% pop$USUBJID, , drop = FALSE]
      out[.an_data_value(d, p)] <- both(x)
    }
  }
  for (d in ds$dataset) {
    x <- get(d)
    if (is.null(x)) next
    out[.an_data_value(d, NA)] <- both(x)
  }
  out
}

# The form's data choice written to analysis row `i`: its analysis data
# (the dataset and analysis set blank: the data has them), or the two
# columns
.an_data_write <- function(a, i, fd) {
  if (is.null(a$data)) a$data <- rep(NA_character_, nrow(a))
  if (!is.na(fd$data %||% NA)) {
    a$data[i] <- fd$data
    a$dataset[i] <- NA
    a$population_id[i] <- NA
  } else {
    a$data[i] <- NA
    a$dataset[i] <- fd$dataset
    a$population_id[i] <- fd$pop
  }
  a
}
