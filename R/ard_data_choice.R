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

# a value back to its two parts (NA for a blank part)
.an_data_split <- function(value) {
  v <- strsplit(paste0(value %||% "", "|"), "|", fixed = TRUE)[[1L]]
  na <- function(x) if (is.na(x) || !nzchar(x)) NA_character_ else x
  list(dataset = na(v[1L]), pop = na(v[2L]))
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
# ("%s, no analysis set (%s)"), `none`.
.an_data_choices <- function(datasets, populations, now = "|", words) {
  datasets <- unique(datasets[!is.na(datasets) & nzchar(datasets)])
  po <- populations[!is.na(populations$population_id), , drop = FALSE]
  val <- character()
  lab <- character()
  add <- function(ds, pop) {
    v <- .an_data_value(ds, pop)
    if (v %in% val) return()
    nm <- .an_data_name(ds, pop, populations)
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
  if (!now %in% val) {
    if (is.na(x$dataset) && is.na(x$pop)) {
      val <- c(now, val)
      lab <- c(words$none, lab)
    } else {
      add(x$dataset, x$pop)
    }
  }
  stats::setNames(val, lab)
}
