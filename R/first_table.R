# From data files to a first table, without knowing the sheets.
#
# catalog_add_files(): the files put in the study's data folders become the
# ARD definition's datasets (name, level, path), so nobody types them.
# first_table(): the few answers a summary table needs -- which data, which
# analysis set, which group for the columns, which variables for the rows --
# written as the rows of every sheet that says them (datasets, populations,
# analyses, tables, variables).  Everything stays editable in the grids.

# the level of a study data folder
.folder_level <- function(folder) {
  lay <- study_layout()
  if (identical(folder, lay[["adam"]])) return("ADaM")
  if (identical(folder, lay[["sdtm"]])) return("SDTM")
  NA_character_
}

.data_exts <- c("rds", "csv", "xpt", "sas7bdat", "parquet")

# a dataset's name from its file: data/adam/adsl.rds -> ADSL
.dataset_name <- function(file) {
  toupper(tools::file_path_sans_ext(basename(file)))
}

#' Put a study's data files into its data catalog
#'
#' Every data file of the ADaM and SDTM folders ([study_files()]) that the
#' ARD definition's `datasets` sheet does not have yet -- by path, or by
#' name -- gets a row: `dataset` the file name in capitals, `level` the
#' folder's (`ADaM`, `SDTM`), `path` relative to the study folder.  Rows
#' already there are left as they are.
#'
#' @param x A `tflplanner`.
#' @param files [study_files()] of the study.
#' @return The `tflplanner`, with attribute `added` (the names added).
#' @export
catalog_add_files <- function(x, files) {
  ds <- ard_rows(x, "datasets")
  lvl <- vapply(files$folder, .folder_level, "", USE.NAMES = FALSE)
  rel <- file.path(files$folder, files$file)
  ok <- !is.na(lvl) & tolower(tools::file_ext(files$file)) %in% .data_exts
  added <- character()
  for (i in which(ok)) {
    nm <- .dataset_name(files$file[i])
    if (rel[i] %in% ds$path || nm %in% ds$dataset) next
    ds[nrow(ds) + 1L, ] <- NA
    ds$dataset[nrow(ds)] <- nm
    ds$level[nrow(ds)] <- lvl[i]
    ds$path[nrow(ds)] <- rel[i]
    added <- c(added, nm)
  }
  if (length(added)) x <- set_ard_rows(x, "datasets", "", ds)
  attr(x, "added") <- added
  x
}

# The groups in the order the data give them: a factor's levels, else the
# order of its numeric twin (TRT01A by TRT01AN), else none (the ARD's).
.group_order <- function(data, group) {
  v <- data[[group]]
  if (is.factor(v)) return(levels(droplevels(v)))
  n <- data[[paste0(group, "N")]]
  if (is.character(v) && is.numeric(n)) {
    u <- unique(data.frame(v = v, n = n, stringsAsFactors = FALSE))
    u <- u[!is.na(u$v) & nzchar(u$v), , drop = FALSE]
    if (!anyDuplicated(u$v)) return(u$v[order(u$n)])
  }
  character()
}

# a column's kind for a summary table: numbers are summarized, the rest
# counted (dates and times are neither)
.column_kind <- function(v) {
  if (inherits(v, c("Date", "POSIXt", "difftime"))) return(NA_character_)
  if (is.numeric(v)) "continuous" else "categorical"
}

#' Start a summary table from the data
#'
#' Writes what a summary table (columns = a group, rows = variables) needs,
#' from the answers the app's "first table" form asks: the dataset in the
#' data catalog (added when missing), the analysis set (`population` flag
#' `== "Y"`, added when missing), one analysis per variable (numbers
#' summarized, the rest counted), the report (a Table, added when missing)
#' and its `tables` row (the group as columns, one group per variable) and
#' `variables` rows (their order, and the data's labels).
#'
#' @param x A `tflplanner`.
#' @param output_id The report.
#' @param path The data file, relative to the study folder.
#' @param data Its data (the kinds and labels of its columns).
#' @param population The analysis set flag column (`SAFFL`).
#' @param group The column of the groups (`TRT01A`).
#' @param variables The variables of the rows, in order.
#' @param description The report's description.
#' @return The `tflplanner`.
#' @export
first_table <- function(x, output_id, path, data, population, group,
                        variables, description = NA_character_) {
  id <- output_id
  miss <- setdiff(c(population, group, variables), names(data))
  if (length(miss)) {
    stop("The data have no ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  if (!length(variables)) stop("Choose the variables of the rows.", call. = FALSE)
  if (nrow(ard_rows(x, "analyses", id))) {
    stop("'", id, "' has analyses already: edit them on the ARD tab.",
         call. = FALSE)
  }
  kind <- vapply(variables, function(v) .column_kind(data[[v]]), "")
  if (anyNA(kind)) {
    stop("Dates and times cannot be rows of a summary table: ",
         paste(variables[is.na(kind)], collapse = ", "), ".", call. = FALSE)
  }

  # the dataset
  ds <- ard_rows(x, "datasets")
  dsn <- ds$dataset[match(path, ds$path)]
  if (is.na(dsn)) {
    dsn <- .dataset_name(path)
    if (!dsn %in% ds$dataset) {
      lv <- .folder_level(dirname(path))
      if (is.na(lv)) lv <- if (grepl("sdtm", path, ignore.case = TRUE)) "SDTM" else "ADaM"
      ds[nrow(ds) + 1L, ] <- NA
      ds$dataset[nrow(ds)] <- dsn
      ds$level[nrow(ds)] <- lv
      ds$path[nrow(ds)] <- path
      x <- set_ard_rows(x, "datasets", "", ds)
    }
  }

  # the analysis set
  pop <- sub("FL$", "", population)
  po <- ard_rows(x, "populations")
  where <- sprintf("%s == \"Y\"", population)
  have <- po$population_id[!is.na(po$where) & po$where == where &
                             !is.na(po$dataset) & po$dataset == dsn]
  if (length(have)) {
    pop <- have[1L]
  } else {
    while (pop %in% po$population_id) pop <- paste0(pop, "_", dsn)
    po[nrow(po) + 1L, ] <- NA
    po$population_id[nrow(po)] <- pop
    po$dataset[nrow(po)] <- dsn
    po$where[nrow(po)] <- where
    x <- set_ard_rows(x, "populations", "", po)
  }

  # the report; the subjects per group (the N of the column headers), then
  # one analysis per variable
  if (!id %in% x$outputs$output_id) {
    x <- add_output(x, id, description = description, type = "table")
  }
  an <- data.frame(
    analysis_id = c("BIGN", variables),
    label = c("Subjects per group", rep(NA, length(variables))),
    method = c("categorical", unname(kind)),
    dataset = dsn, population_id = pop,
    by = c(NA, rep(group, length(variables))),
    variables = c(group, variables), stringsAsFactors = FALSE)
  x <- set_ard_rows(x, "analyses", id, an)

  # the table: the group as columns, one group per variable
  tb <- sheet_rows(x, "tables", id)
  tb$output_id <- NULL
  if (!nrow(tb)) tb[1L, ] <- NA
  tb$cols[1L] <- group
  tb$rows[1L] <- "group = variable"
  x <- set_sheet_rows(x, "tables", id, tb)

  # the variables: the groups' order; the rows' order and the data's labels
  vr <- sheet_rows(x, "variables", id)
  vr$output_id <- NULL
  arms <- .group_order(data, group)
  if (length(arms)) {
    i <- match(group, vr$variable)
    if (is.na(i)) {
      vr[nrow(vr) + 1L, ] <- NA
      i <- nrow(vr)
      vr$variable[i] <- group
    }
    if (is.na(vr$levels[i])) vr$levels[i] <- paste(arms, collapse = " | ")
  }
  for (k in seq_along(variables)) {
    v <- variables[k]
    i <- match(v, vr$variable)
    if (is.na(i)) {
      vr[nrow(vr) + 1L, ] <- NA
      i <- nrow(vr)
      vr$variable[i] <- v
    }
    vr$order[i] <- as.character(k)
    lab <- attr(data[[v]], "label", exact = TRUE)
    if (is.na(vr$label[i]) && is.character(lab) && length(lab) == 1L &&
        nzchar(lab)) vr$label[i] <- lab
  }
  x <- set_sheet_rows(x, "variables", id, vr)

  # the cells: the usual statistics of a continuous variable (the builder
  # changes them), unless the study's defaults state them
  ce <- sheet_rows(x, "cells", id)
  ce$output_id <- NULL
  v_all <- c(ce$variable, inherited_rows(x, "cells", id)$variable)
  if (any(kind == "continuous") &&
      !any(!is.na(v_all) & v_all == "continuous")) {
    bs <- builder_stats()
    for (k in intersect(.builder_default_stats, bs$key)) {
      ce[nrow(ce) + 1L, ] <- NA
      ce$variable[nrow(ce)] <- "continuous"
      ce$row[nrow(ce)] <- bs$row[bs$key == k]
      ce$template[nrow(ce)] <- bs$template[bs$key == k]
      ce$digits[nrow(ce)] <- .stat_digits(k, 0)
    }
    x <- set_sheet_rows(x, "cells", id, ce)
  }
  x
}
