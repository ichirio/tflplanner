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

.data_exts <- c("rds", "rda", "rdata", "csv", "xpt", "sas7bdat", "parquet")

# a dataset's name from its file: data/adam/adsl.rds -> ADSL
.dataset_name <- function(file) {
  toupper(tools::file_path_sans_ext(basename(file)))
}

# The formats a dataset's file may be in, the one to use first: the file R
# reads whole and fastest first
.data_pref <- c("rds", "rda", "rdata", "parquet", "csv", "xpt", "sas7bdat")

#' Put a study's data files into its data catalog
#'
#' Every data file of the data folders (`data/adam`, `data/sdtm`,
#' `data/other`, subfolders too; [study_files()]) that the ARD definition's
#' `datasets` sheet does not have yet -- by path, or by name -- gets a row:
#' `dataset` the file name in capitals, `level` the folder's (`ADaM`,
#' `SDTM`; blank for `other`), `path` relative to the study folder.  Rows
#' already there are left as they are.  When one name has files in several
#' formats (`adsl.rds`, `adsl.xpt`), the row is made for the format read
#' first (`rds`, `rda`, `parquet`, `csv`, `xpt`, `sas7bdat`); the others
#' stay files with no row of their own, and [catalog_name_clashes()] says
#' which names have more than one file.
#'
#' @param x A `tflplanner`.
#' @param files [study_files()] of the study.
#' @return The `tflplanner`, with attribute `added` (the names added).
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (needs the sample data; a few seconds)
#' s <- create_sample_study(run = FALSE)
#' # a new data file in the study's data folder ...
#' saveRDS(data.frame(USUBJID = 1, PARAMCD = "ALT"), file.path(s$path, "data/adam/adlb.rds"))
#' # ... gets its row in the data catalog
#' p <- catalog_add_files(s$planner, study_files(s))
#' attr(p, "added")
#' }
#' options(old)
#' @export
catalog_add_files <- function(x, files) {
  ds <- ard_rows(x, "datasets")
  lay <- study_layout()
  folders <- unname(lay[c("adam", "sdtm", "other")])
  lvl <- vapply(files$folder, .folder_level, "", USE.NAMES = FALSE)
  ext <- tolower(tools::file_ext(files$file))
  rel <- file.path(files$folder, files$file)
  ok <- files$folder %in% folders & ext %in% .data_exts
  # one file a name: the format read first, then the folder's order (ADaM,
  # SDTM, other); the rows are made in the files' order
  rank <- order(match(ext, .data_pref), match(files$folder, folders))
  best <- rank[ok[rank]]
  best <- best[!duplicated(.dataset_name(files$file[best]))]
  added <- character()
  for (i in sort(best)) {
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

#' The dataset names with more than one data file
#'
#' A name several files make (`adsl.rds` and `adsl.xpt`, or `adam/adsl.rds`
#' and `other/adsl.rds`): the catalog's row uses one, and this says which
#' others there are.
#'
#' @param x A `tflplanner`.
#' @param files [study_files()] of the study.
#' @return A data frame: `dataset`, `path` (every file of that name),
#'   `used` (the catalog's row's path, `NA` when it has none).
#' @noRd
catalog_name_clashes <- function(x, files) {
  ds <- ard_rows(x, "datasets")
  lay <- study_layout()
  keep <- files$folder %in% unname(lay[c("adam", "sdtm", "other")]) &
    tolower(tools::file_ext(files$file)) %in% .data_exts
  f <- files[keep, , drop = FALSE]
  nm <- .dataset_name(f$file)
  rel <- file.path(f$folder, f$file)
  out <- lapply(unique(nm[duplicated(nm)]), function(n) data.frame(
    dataset = n, path = rel[nm == n],
    used = ds$path[match(n, ds$dataset)], stringsAsFactors = FALSE))
  if (!length(out)) return(data.frame(dataset = character(), path = character(),
                                      used = character(), stringsAsFactors = FALSE))
  do.call(rbind, out)
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
#' `== "Y"`, added when missing), one analysis of the numeric variables
#' (summarized) and one of the rest (counted), the report (a Table, added when missing)
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
#' @param stack Run the analyses together, with `cards::ard_stack()` (a
#'   `STACK` analysis with the data, analysis set and group, the numeric and
#'   the other variables inside it; it counts the subjects per group and in
#'   all), rather than one by one (`GROUPN`, `CONT`, `CAT`).  Not when a
#'   subject of the analysis set has no group: `ard_stack()` leaves them
#'   out (the result's attribute `group_missing` says how many).
#' @return The `tflplanner`.
#' @examples
#' adsl <- data.frame(USUBJID = 1:4, SAFFL = "Y", TRT01A = c("A", "A", "B", "B"),
#'                    AGE = c(34, 51, 47, 62), SEX = c("F", "M", "F", "M"))
#' x <- first_table(new_planner(), "T-1", "data/adam/adsl.rds", adsl,
#'                   population = "SAFFL", group = "TRT01A",
#'                   variables = c("AGE", "SEX"))
#' # the analyses it wrote: one of the numbers (AGE), one of the counts (SEX)
#' ard_rows(x, "analyses", "T-1")[, c("analysis_id", "method", "variables")]
#' @export
first_table <- function(x, output_id, path, data, population, group,
                        variables, description = NA_character_, stack = TRUE) {
  id <- output_id
  miss <- setdiff(c(population, group, variables), names(data))
  if (length(miss)) {
    stop("The data have no ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  if (!length(variables)) stop("Choose the variables of the rows.", call. = FALSE)
  if (nrow(ard_rows(x, "analyses", id))) {
    stop("'", id, "' has analyses already: edit them in step 1 (ARD).",
         call. = FALSE)
  }
  kind <- vapply(variables, function(v) .column_kind(data[[v]]), "")
  if (anyNA(kind)) {
    stop("Dates and times cannot be rows of a summary table: ",
         paste(variables[is.na(kind)], collapse = ", "), ".", call. = FALSE)
  }

  # the dataset
  cd <- .catalog_dataset(x, path)
  x <- cd$x
  dsn <- cd$dataset

  # the analysis set
  x <- .first_population(x, dsn, population)
  pop <- attr(x, "population_id")

  # the report; the subjects per group (the N of the column headers), then
  # one analysis per call: the numeric variables together (CONT), the
  # categorical ones together (CAT) -- the table's rows keep the order the
  # variables were chosen in (the variables sheet's `order`)
  if (!id %in% x$outputs$output_id) {
    x <- add_output(x, id, description = description, type = "table")
  }
  groups <- list(CONT = variables[kind == "continuous"],
                 CAT = variables[kind == "categorical"])
  groups <- groups[lengths(groups) > 0L]
  methods <- c(CONT = "cards::ard_summary", CAT = "cards::ard_tabulate")[names(groups)]
  vars <- vapply(groups, paste, "", collapse = " | ")
  # a subject of the analysis set with no group: ard_stack() would leave
  # them out, so the analyses are made one by one
  g <- data[[group]]
  miss <- sum((is.na(g) | !nzchar(trimws(as.character(g)))) &
                data[[population]] %in% "Y")
  if (miss > 0L) stack <- FALSE
  an <- if (isTRUE(stack)) {
    data.frame(
      analysis_id = c("STACK", names(groups)),
      parent = c(NA, rep("STACK", length(groups))),
      label = c(if (is.na(description)) "Summary table" else description,
                rep(NA, length(groups))),
      method = c(.stack_fn, methods),
      dataset = c(dsn, rep(NA, length(groups))),
      population_id = c(pop, rep(NA, length(groups))),
      by = c(group, rep(NA, length(groups))),
      variables = c(NA, vars),
      args = c(".total_n = TRUE", rep(NA, length(groups))),
      stringsAsFactors = FALSE, row.names = NULL)
  } else {
    data.frame(
      analysis_id = c("GROUPN", names(groups)),
      label = c("Subjects per group", rep(NA, length(groups))),
      method = c("cards::ard_tabulate", methods),
      dataset = dsn, population_id = pop,
      by = c(NA, rep(group, length(groups))),
      variables = c(group, vars),
      stringsAsFactors = FALSE, row.names = NULL)
  }
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
    for (lb in intersect(.builder_default_rows, bs$row)) {
      ce[nrow(ce) + 1L, ] <- NA
      ce$variable[nrow(ce)] <- "continuous"
      ce$row[nrow(ce)] <- lb
      ce$template[nrow(ce)] <- bs$template[bs$row == lb]
    }
    x <- set_sheet_rows(x, "cells", id, ce)
    # their decimals: the study's; a study that states none takes the
    # company standards' here, for this table
    sd <- x$sheets$digits
    if (is.null(sd) || !any(is.na(sd$output_id) & is.na(sd$variable))) {
      used <- unique(unlist(lapply(ce$template[ce$variable %in% "continuous"],
                                   .template_stats)))
      std <- .std_digits()
      used <- intersect(used, names(std))
      if (length(used)) {
        x <- set_sheet_rows(x, "digits", id, data.frame(
          variable = NA_character_, statistic = used,
          digits = as.character(std[used]), stringsAsFactors = FALSE))
      }
    }
  }
  attr(x, "group_missing") <- miss
  x
}

# The analysis set of a flag (SAFFL == "Y") on a dataset: the one there,
# else added (SAF, named after the flag).  The planner, with attribute
# "population_id".
.first_population <- function(x, dataset, flag) {
  pop <- sub("FL$", "", flag)
  po <- ard_rows(x, "populations")
  where <- sprintf("%s == \"Y\"", flag)
  have <- po$population_id[!is.na(po$where) & po$where == where &
                             !is.na(po$dataset) & po$dataset == dataset]
  if (length(have)) {
    pop <- have[1L]
  } else {
    while (pop %in% po$population_id) pop <- paste0(pop, "_", dataset)
    po[nrow(po) + 1L, ] <- NA
    po$population_id[nrow(po)] <- pop
    po$dataset[nrow(po)] <- dataset
    po$where[nrow(po)] <- where
    x <- set_ard_rows(x, "populations", "", po)
  }
  attr(x, "population_id") <- pop
  x
}

#' Start a hierarchical table from the data
#'
#' The other kind of table the app's "first table" form starts: counts of
#' subjects by a hierarchy of terms -- adverse events by system organ class
#' and preferred term (`AEBODSYS | AEDECOD`), and the same for medications
#' or medical history by their own -- in the columns of a group, the
#' percents of the analysis set's subjects.  It writes the rows every sheet
#' says them in, as a table made by hand has them:
#'
#' * the datasets (added to the data catalog when missing) and the analysis
#'   set (`population` flag `== "Y"` of the subject-level data);
#' * two analysis data of the report: the analysis set's subjects
#'   (`adsl_saf`), and the records kept to them and to `where` (`adae_saf`);
#' * one `hierarchical` analysis by the group, over the hierarchy, its
#'   denominator the analysis set (`over_variables = TRUE`: the "any event"
#'   row, named in the table's `overall_label`: `"Any TEAE"`);
#' * the table: the group as columns, the outer terms as groups and the
#'   innermost as rows, by frequency (most first) within each group;
#' * the column header (the standards' `"Arm (N=n) n (%), SOC / PT"`), a
#'   page not splitting a group (22 rows a page, unless the study says),
#'   and a Total column when `total` is given.
#'
#' @param x A `tflplanner`.
#' @param output_id The report.
#' @param path The records' data file (`data/adam/adae.rds`), relative to
#'   the study folder.
#' @param data Its data.
#' @param subjects_path The subject-level data file (`data/adam/adsl.rds`).
#' @param subjects Its data.
#' @param population The analysis set flag column of the subject-level data
#'   (`SAFFL`).
#' @param group The column of the groups (`TRT01A`).
#' @param hierarchy The terms, outermost first (`c("AEBODSYS", "AEDECOD")`).
#' @param where The records counted (`TRTEMFL == "Y"`); `NA` all of them.
#' @param total The heading of a Total column (`"Total"`); `NA` none.
#' @param grade A severity or grade column (`AESEV`, `ATOXGR`) to count by
#'   too, in the columns under each group; `NA` none.  Each subject is
#'   counted once, at the worst: the grade's levels are written in order
#'   (a code list), and cards counts a subject at the last level it has --
#'   of the SOC for the SOC row, of the PT for the PT row.  Not with a Total
#'   column.
#' @param description The report's description.
#' @return The `tflplanner`.
#' @examples
#' adsl <- data.frame(USUBJID = 1:4, SAFFL = "Y", TRT01A = c("A", "A", "B", "B"))
#' adae <- data.frame(USUBJID = 1:3, TRT01A = c("A", "A", "B"), TRTEMFL = "Y",
#'                    AEBODSYS = "Nervous system",
#'                    AEDECOD = c("Headache", "Dizziness", "Headache"))
#' x <- first_hierarchy_table(new_planner(), "T-AE", "data/adam/adae.rds", adae,
#'                            "data/adam/adsl.rds", adsl, population = "SAFFL",
#'                            group = "TRT01A", hierarchy = c("AEBODSYS", "AEDECOD"))
#' ard_rows(x, "analyses", "T-AE")[, c("analysis_id", "method")]
#' @export
first_hierarchy_table <- function(x, output_id, path, data, subjects_path, subjects,
                                  population, group,
                                  hierarchy = c("AEBODSYS", "AEDECOD"),
                                  where = NA_character_, total = NA_character_,
                                  grade = NA_character_,
                                  description = NA_character_) {
  id <- output_id
  hierarchy <- hierarchy[!is.na(hierarchy) & nzchar(hierarchy)]
  if (length(hierarchy) < 2L) {
    stop("A hierarchy has two terms or more (AEBODSYS | AEDECOD).", call. = FALSE)
  }
  miss <- setdiff(c(group, hierarchy), names(data))
  if (length(miss)) stop("The data have no ", paste(miss, collapse = ", "), ".", call. = FALSE)
  miss <- setdiff(c(population, group), names(subjects))
  if (length(miss)) {
    stop("The subject-level data have no ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  if (nrow(ard_rows(x, "analyses", id))) {
    stop("'", id, "' has analyses already: edit them in step 1 (ARD).", call. = FALSE)
  }
  if (.is_blank(where)) where <- NA_character_
  if (.is_blank(grade)) grade <- NA_character_
  if (!is.na(grade)) {
    if (!grade %in% names(data)) stop("The data have no ", grade, ".", call. = FALSE)
    if (grade %in% c(group, hierarchy)) {
      stop("The grade is a column of its own, not the group or a term.", call. = FALSE)
    }
    if (!is.na(total) && nzchar(trimws(total))) {
      stop("A Total column and a grade together are not made here: add the Total ",
           "column afterwards in the sheets.", call. = FALSE)
    }
  }
  if (!is.na(where) && inherits(try(str2lang(where), silent = TRUE), "try-error")) {
    stop("The condition is not R: ", where, call. = FALSE)
  }

  # the datasets and the analysis set (of the subject-level data)
  cd <- .catalog_dataset(x, subjects_path)
  x <- cd$x
  sl <- cd$dataset
  cd <- .catalog_dataset(x, path)
  x <- cd$x
  dsn <- cd$dataset
  x <- .first_population(x, sl, population)
  pop <- attr(x, "population_id")

  if (!id %in% x$outputs$output_id) {
    x <- add_output(x, id, description = description, type = "table")
  }
  # the analysis data: the set's subjects, and the records kept to them
  x <- .ensure_pop_adata(x, id, pop)
  subj <- attr(x, "data_id")
  rec <- .adata_free_name(x, id, .adata_default_name(dsn, pop))
  x <- set_analysis_data(x, id, rec, from = dsn, subjects = subj, where = where)

  # the analysis: the hierarchy by the group, percents of the set's subjects
  # (its name: the records' -- AE; TEAE for the treatment-emergent ones)
  aid <- sub("^AD", "", dsn)
  if (!nzchar(aid)) aid <- "HIER"
  if (!is.na(where) && grepl("\\bTRTEMFL\\b", where)) aid <- paste0("TE", aid)
  an <- data.frame(
    analysis_id = aid,
    label = if (is.na(description)) paste(hierarchy, collapse = " / ") else description,
    method = "hierarchical", data = rec,
    by = paste(c(group, stats::na.omit(grade)), collapse = " | "),
    variables = paste(hierarchy, collapse = " | "), denominator = subj,
    args = "over_variables = TRUE", stringsAsFactors = FALSE)
  x <- set_ard_rows(x, "analyses", id, an)

  # the table: the outer terms as groups, the innermost as rows, by
  # frequency within each group
  n <- length(hierarchy)
  tb <- sheet_rows(x, "tables", id)
  tb$output_id <- NULL
  if (!nrow(tb)) tb[1L, ] <- NA
  tb$cols[1L] <- paste(c(group, stats::na.omit(grade)), collapse = " | ")
  tb$rows[1L] <- paste(sprintf("group%d = %s", seq_len(n - 1L), hierarchy[-n]),
                       collapse = " | ")
  tb$label[1L] <- paste("label =", hierarchy[n])
  # the "any event" row's name: the program reads the ARD as a hierarchy
  # with it (tflspec::tfl_normalize_code())
  tb$overall_label[1L] <- paste("Any", aid)
  tb$sort[1L] <- .hier_sort(c(rep("alpha", n - 1L), "freq"))
  x <- set_sheet_rows(x, "tables", id, tb)

  # the groups' order (the columns): the analysis set's, from the
  # subject-level data
  arms <- .group_order(subjects[subjects[[population]] %in% "Y", , drop = FALSE], group)
  if (length(arms)) {
    vr <- sheet_rows(x, "variables", id)
    vr$output_id <- NULL
    i <- match(group, vr$variable)
    if (is.na(i)) {
      vr[nrow(vr) + 1L, ] <- NA
      i <- nrow(vr)
      vr$variable[i] <- group
    }
    if (is.na(vr$levels[i])) vr$levels[i] <- paste(arms, collapse = " | ")
    x <- set_sheet_rows(x, "variables", id, vr)
  }

  # the grade's levels in order: its code list (cards counts a subject at
  # the last level it has -- its worst)
  if (!is.na(grade)) {
    lv <- .grade_levels(data[[grade]])
    x <- set_codelist(x, id, data.frame(variable = grade, value = lv,
                                        order = as.character(seq_along(lv))))
  }

  # the column header, and a page that does not split a group
  hp <- "Arm (N=n) n (%), SOC / PT"
  if (is.na(grade) && hp %in% names(header_presets())) x <- add_preset(x, id, hp)
  if (!is.na(grade)) {
    # each group over its grades: the group and its N, then each grade
    x <- set_sheet_rows(x, "col_header", id, data.frame(
      line = c("1", "1", "2", "2"), cols = c("row_label", ".values", "row_label", ".values"),
      span = c(NA, group, NA, "each"),
      text = c(NA, "{col1}\n(N={n})", "System Organ Class\n   Preferred Term", "{col}"),
      stringsAsFactors = FALSE))
    # (a group a grade: narrower value columns, room for the terms)
    x <- set_sheet_rows(x, "columns", id, data.frame(
      column = c("row_label", ".values"), rel_width = c("34", "10"),
      stringsAsFactors = FALSE))
  }
  ly <- sheet_rows(x, "layout", id)
  ly$output_id <- NULL
  if (!nrow(ly)) ly[1L, ] <- NA
  ly$pages_split[1L] <- "group_force"
  # (which needs the rows a page: the study's, else as the sample's SOC /
  # PT table)
  inh <- inherited_rows(x, "layout", id)
  if (.is_blank(ly$pages_max_rows[1L]) &&
      (!nrow(inh) || .is_blank(inh$pages_max_rows[1L]))) ly$pages_max_rows[1L] <- "22"
  x <- set_sheet_rows(x, "layout", id, ly)

  if (!is.na(total) && nzchar(trimws(total))) x <- set_total_column(x, id, total)
  x
}

# A grade's levels, mildest first: a factor's own, the usual severities
# (MILD < MODERATE < SEVERE, LIFE THREATENING, FATAL), numbers by value,
# else by name
.grade_levels <- function(v) {
  if (is.factor(v)) return(levels(droplevels(v)))
  u <- unique(as.character(v[!is.na(v) & nzchar(trimws(as.character(v)))]))
  sev <- c("MILD", "MODERATE", "SEVERE", "LIFE THREATENING", "FATAL", "DEATH")
  if (length(u) && all(toupper(u) %in% sev)) return(u[order(match(toupper(u), sev))])
  num <- suppressWarnings(as.numeric(sub("^[^0-9]*", "", u)))
  if (length(u) && !anyNA(num)) return(u[order(num)])
  sort(u)
}

# Does a report's ARD count the subjects per group (the N of the column
# headers)?  An analysis of the group itself, not by anything, or a
# total_n by the group.
.has_group_n <- function(x, output_id, group) {
  a <- ard_rows(x, "analyses", output_id)
  if (!nrow(a) || is.na(group)) return(FALSE)
  stacks <- a$method %in% .stack_fn & a$by %in% group
  stacks[stacks] <- vapply(a$args[stacks], function(g)
    isTRUE(.stack_flags_of(g)$flags[[".by_stats"]]), NA)
  kw <- .method_kw(a$method)
  # (a hierarchy counted of a denominator: its rows count the
  # denominator's subjects per group too)
  den <- if (is.null(a$denominator)) rep(NA_character_, nrow(a)) else a$denominator
  hier <- kw %in% c("hierarchical", "cards::ard_stack_hierarchical") &
    vapply(a$by, function(b) group %in% .split_bar(b), NA) & !is.na(den) & nzchar(den)
  any((kw %in% c("categorical", "subjects") & a$variables %in% group &
         is.na(a$by)) |
        (kw %in% "total_n" & a$by %in% group) | stacks | hier)
}

#' Count the subjects per group for a report's column headers
#'
#' Adds to the report's ARD analyses the subjects per group, which clinical
#' reporting calls big N (`GROUPN`: the group counted, by nothing) -- what
#' the column headers' `(N={n})` read --
#' with the data and analysis set of its first analysis.
#'
#' @param x A `tflplanner`.
#' @param output_id The report.
#' @param group The group column (`TRT01A`).
#' @return The `tflplanner`.
#' @examples
#' adsl <- data.frame(USUBJID = 1:4, SAFFL = "Y", TRT01A = c("A", "A", "B", "B"),
#'                    AGE = c(34, 51, 47, 62), SEX = c("F", "M", "F", "M"))
#' x <- first_table(new_planner(), "T-1", "data/adam/adsl.rds", adsl, "SAFFL",
#'                   "TRT01A", "AGE", stack = FALSE)
#' x <- add_group_n(x, "T-1", "TRT01A")  # the subjects per group, for (N=n)
#' ard_rows(x, "analyses", "T-1")$analysis_id
#' @export
add_group_n <- function(x, output_id, group) {
  a <- ard_rows(x, "analyses", output_id)
  if (!nrow(a)) stop("'", output_id, "' has no analyses.", call. = FALSE)
  id <- "GROUPN"
  while (id %in% a$analysis_id) id <- paste0(id, "_")
  first <- a[1L, , drop = FALSE]
  new <- a[0L, , drop = FALSE]
  new[1L, ] <- NA
  new$analysis_id <- id
  new$label <- "Subjects per group"
  new$method <- "cards::ard_tabulate"
  new$dataset <- first$dataset
  new$population_id <- first$population_id
  new$variables <- group
  new$output_id <- NULL
  a$output_id <- NULL
  set_ard_rows(x, "analyses", output_id, rbind(new, a))
}

#' A Total column for a report
#'
#' Switches a table's Total column on or off, in both halves of its
#' definition at once: the `tables` sheet's `total` (the heading) and
#' `total_position`, written as `plan_total()`; and `overall = TRUE` on the
#' report's own analyses grouped by the column variable, which makes each
#' ARD program run those analyses again without their `by` -- cards' own
#' overall rows, with no group, which the table reads as the column and
#' the ARS writes as an analysis without the grouping.  No `"Total"` value
#' of the arm is made up in the data.
#'
#' @param x A `tflplanner`.
#' @param output_id The report.
#' @param label The column's heading (`"Total"`); `NULL` or `""` switches
#'   the column off.
#' @param position Where it goes among the column variable's values.
#' @return The `tflplanner`.
#' @examples
#' adsl <- data.frame(USUBJID = 1:4, SAFFL = "Y", TRT01A = c("A", "A", "B", "B"),
#'                    AGE = c(34, 51, 47, 62), SEX = c("F", "M", "F", "M"))
#' x <- first_table(new_planner(), "T-1", "data/adam/adsl.rds", adsl, "SAFFL",
#'                   "TRT01A", "AGE")
#' x <- set_total_column(x, "T-1", "All subjects")  # switch it on
#' x$sheets$tables[, c("output_id", "total", "total_position")]
#' x <- set_total_column(x, "T-1", NULL)  # and off again
#' @export
set_total_column <- function(x, output_id, label = "Total",
                             position = c("last", "first")) {
  position <- match.arg(position)
  on <- !is.null(label) && !is.na(label) && nzchar(trimws(label))
  key <- .report_cols(x, output_id)[1L]
  if (on && is.na(key)) {
    stop("'", output_id, "' has no column variable yet (tables$cols).",
         call. = FALSE)
  }
  # the table's half
  tb <- sheet_rows(x, "tables", output_id)
  tb$output_id <- NULL
  if (!nrow(tb)) tb[1L, ] <- NA
  tb$total[1L] <- if (on) trimws(label) else NA_character_
  tb$total_position[1L] <- if (on && position == "first") "first" else NA_character_
  x <- set_sheet_rows(x, "tables", output_id, tb)
  # the ARD's half: the report's own analyses by the column variable (a
  # stack's row, not the rows inside it; not what has no by to leave out)
  a <- ard_rows(x, "analyses", output_id)
  if (!nrow(a)) return(x)
  if (is.null(a$overall)) a$overall <- NA_character_
  a$overall[.total_rows(a, key)] <- if (on) "TRUE" else NA_character_
  a$output_id <- NULL
  set_ard_rows(x, "analyses", output_id, a)
}

# Which of a report's analyses a Total column runs over all subjects too:
# those grouped by the column variable, a stack's own row (not the rows
# inside it), not a subject count, own code, strata or pairwise (no by to
# leave out)
.total_rows <- function(a, key) {
  if (!nrow(a) || is.na(key)) return(logical(nrow(a)))
  par <- a$parent %||% rep(NA_character_, nrow(a))
  kw <- .method_kw(a$method)
  grouped <- vapply(a$by, function(b) key %in% .split_bar(b), NA)
  grouped & is.na(par) & !kw %in% c("custom", "subjects") &
    !a$method %in% c("cards::ard_strata", "cards::ard_pairwise")
}

# A yes in the definition's `overall` column
.overall_on <- function(x) {
  !is.null(x) && !is.na(x) && toupper(trimws(x)) %in% c("TRUE", "YES", "Y", "1")
}

# A report's column variables, its own `tables$cols` or the inherited one
.report_cols <- function(x, output_id) {
  tb <- sheet_rows(x, "tables", output_id)
  v <- if (nrow(tb)) tb$cols[1L] else NA_character_
  if (is.na(v)) {
    inh <- inherited_rows(x, "tables", output_id)
    v <- if (nrow(inh)) inh$cols[1L] else NA_character_
  }
  if (is.na(v)) NA_character_ else .split_bar(v)
}

# The dataset of a data file in the catalog, added when missing:
# list(x, dataset).
.catalog_dataset <- function(x, path) {
  ds <- ard_rows(x, "datasets")
  dsn <- ds$dataset[match(path, ds$path)]
  if (!is.na(dsn)) return(list(x = x, dataset = dsn))
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
  list(x = x, dataset = dsn)
}

#' Start a listing from the data
#'
#' Writes what a listing needs from the answers of the app's "first
#' listing" form: the dataset in the data catalog (added when missing), the
#' report (a Listing, added when missing), its `listings` row (the company's
#' listing type; the order: the group, the subject, the start date when the
#' data have one) and one `listing_cols` row per column, in order, headed by
#' the data's label (a repeated group or subject printed once).
#'
#' @param x A `tflplanner`.
#' @param output_id The report.
#' @param path The data file, relative to the study folder.
#' @param data Its data (the columns' labels).
#' @param columns The columns, in order.
#' @param group A group column the listing is ordered by first, or `NULL`.
#' @param description The report's description.
#' @return The `tflplanner`.
#' @examples
#' adsl <- data.frame(USUBJID = 1:4, SAFFL = "Y", TRT01A = c("A", "A", "B", "B"),
#'                    AGE = c(34, 51, 47, 62), SEX = c("F", "M", "F", "M"))
#' x <- first_listing(new_planner(), "L-1", "data/adam/adsl.rds", adsl,
#'                    columns = c("USUBJID", "AGE", "SEX"), group = "TRT01A")
#' x$sheets$listing_cols[, 1:3]  # one row a column, in order
#' @export
first_listing <- function(x, output_id, path, data, columns, group = NULL,
                          description = NA_character_) {
  id <- output_id
  if (!length(columns)) stop("Choose the listing's columns.", call. = FALSE)
  miss <- setdiff(c(group, columns), names(data))
  if (length(miss)) {
    stop("The data have no ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  if (nrow(lf_rows(x, "listing_cols", id))) {
    stop("'", id, "' has columns already: edit them in step 2 (Content).",
         call. = FALSE)
  }
  cd <- .catalog_dataset(x, path)
  x <- cd$x
  if (!id %in% x$outputs$output_id) {
    x <- add_output(x, id, description = description, type = "listing")
  }
  start <- intersect(c("ASTDT", "ASTDTM", "ADT", "ADTM"), names(data))[1L]
  sort <- unique(stats::na.omit(c(group, intersect("USUBJID", names(data)), start)))
  cols <- unique(c(group, columns))
  x <- set_lf_rows(x, "listings", id, data.frame(
    type = .std_setting("listing_type", "multiline"), dataset = cd$dataset,
    max_rows = .std_setting("listing_max_rows", NA_character_),
    sort = if (length(sort)) paste(sort, collapse = " | ") else NA_character_,
    stringsAsFactors = FALSE))
  lab <- vapply(cols, function(v) {
    l <- attr(data[[v]], "label", exact = TRUE)
    if (is.character(l) && length(l) == 1L && nzchar(l)) l else v
  }, "", USE.NAMES = FALSE)
  set_lf_rows(x, "listing_cols", id, data.frame(
    vars = cols, label = lab,
    width = vapply(seq_along(cols), function(i)
      as.character(.guess_width(data[[cols[i]]], lab[i])), ""),
    collapse_repeats = ifelse(cols %in% c(group, "USUBJID"), "TRUE", NA),
    stringsAsFactors = FALSE))
}

# A column's width in characters: most of its values fit (90%), and its
# header's longest word; 6 to 40.
.guess_width <- function(v, label = "") {
  n <- nchar(as.character(v[!is.na(v)]), type = "width")
  w <- if (length(n)) ceiling(stats::quantile(n, 0.9, names = FALSE)) else 0
  words <- strsplit(label %||% "", "\\s+")[[1L]]
  lw <- if (length(words)) max(nchar(words, type = "width")) else 0
  as.integer(max(6, min(40, max(w, lw))))
}

# The folder (the pill of the Data tab) each catalog row is shown under: the
# folder its file is in; a row with no file, by its level (ADaM, SDTM); a
# file outside the three folders, or a row of neither level: "other".  A
# file not in the study folder (a path written whole, or ../) goes by its
# level too: ADaM kept outside is under ADaM.
.dataset_pill <- function(path, level) {
  lay <- study_layout()
  vapply(seq_along(path), function(i) {
    p <- path[i]
    whole <- !is.na(p) && (.is_abs_path(p) || grepl("^\\.\\.([/\\\\]|$)", p))
    if (!is.na(p) && nzchar(p) && !whole) {
      for (k in c("adam", "sdtm", "other")) {
        if (startsWith(p, paste0(lay[[k]], "/"))) return(k)
      }
      return("other")
    }
    switch(if (is.na(level[i])) "" else toupper(level[i]),
           ADAM = "adam", SDTM = "sdtm", "other")
  }, "")
}

# The catalog's rows with a folder's rows (`now`, as edited) put back in
# the place the folder's first row was; the other folders' rows as they were
.datasets_put_back <- function(full, now, pill) {
  mine <- .dataset_pill(full$path, full$level) == pill
  at <- if (any(mine)) which(mine)[1L] else nrow(full) + 1L
  rest <- seq_len(nrow(full))[!mine]
  now <- now[, names(full), drop = FALSE]
  out <- rbind(full[rest[rest < at], , drop = FALSE], now,
               full[rest[rest > at], , drop = FALSE])
  rownames(out) <- NULL
  out
}
