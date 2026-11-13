# The four-steps report screen: the Preview card (provisional, a mockup).
# What the right-hand card shows for a step -- the code the step makes and, for
# step 1, the first rows of the data -- built here from the report's YAML by
# R/foursteps_read.R.  Both are SAMPLES: the code is written the way DATA-PART
# section 3 writes a definition, but nothing here is run by the report.

.fs_filter_line <- function(cond) if (nzchar(cond)) sprintf("filter(%s)", cond)

# the `# ---- data ----` section of the report's code
.fs_code_data <- function(m) {
  trt <- m$common$treatment$value
  out <- "# ---- data ----"
  for (r in m$steps$data$table) {
    if (isTRUE(r$optional)) {
      out <- c(out, sprintf("# %s: made only when chosen as a denominator", r$id), "")
      next
    }
    from <- tolower(r$input$value)
    steps <- if (identical(r$id, "adsl_sub")) {
      c(from, .fs_filter_line(r$set$cond), .fs_filter_line(r$filter$value))
    } else {
      sel <- paste(c("USUBJID", if (nzchar(trt)) trt), collapse = ", ")
      c(from,
        sprintf("inner_join(select(adsl_sub, %s), by = \"USUBJID\", relationship = \"many-to-one\")", sel),
        .fs_filter_line(r$filter$value))
    }
    out <- c(out, paste0(r$id, " <- ", paste(steps, collapse = " |>\n  ")), "")
  }
  paste(out, collapse = "\n")
}

# a data file of the study by dataset name, or NULL when it is not there
.fs_read_dataset <- function(m, name) {
  root <- m$study_root
  for (d in m$study_datasets) {
    if (!identical(toupper(as.character(d$dataset)), toupper(name))) next
    p <- as.character(d$path %||% "")
    if (!nzchar(p)) return(NULL)
    if (!grepl("^([A-Za-z]:|/)", p)) p <- file.path(root, p)
    if (!file.exists(p)) return(NULL)
    return(tryCatch(
      if (grepl("[.]rds$", p, ignore.case = TRUE)) as.data.frame(readRDS(p)) else
        utils::read.csv(p, stringsAsFactors = FALSE),
      error = function(e) NULL))
  }
  NULL
}

# rows of df for which the condition is TRUE (all rows for an empty one);
# NULL when the condition cannot be evaluated
.fs_eval_cond <- function(df, cond) {
  if (!nzchar(cond)) return(rep(TRUE, nrow(df)))
  x <- tryCatch(eval(parse(text = cond), df, baseenv()), error = function(e) NULL)
  if (!is.logical(x) || length(x) != nrow(df)) return(NULL)
  x[is.na(x)] <- FALSE
  x
}

# the first rows and the counts of each definition, from the study's data
# files; a definition whose file is missing is left out (an empty list when
# no file is there)
.fs_data_read <- function(m) {
  out <- list()
  adsl_ids <- NULL
  for (r in m$steps$data$table) {
    if (isTRUE(r$optional)) next
    df <- .fs_read_dataset(m, r$input$value)
    if (is.null(df)) next
    keep <- if (identical(r$id, "adsl_sub")) {
      a <- .fs_eval_cond(df, r$set$cond)
      b <- .fs_eval_cond(df, r$filter$value)
      if (is.null(a) || is.null(b)) NULL else a & b
    } else {
      k <- if (is.null(adsl_ids) || !"USUBJID" %in% names(df)) rep(TRUE, nrow(df)) else df$USUBJID %in% adsl_ids
      b <- .fs_eval_cond(df, r$filter$value)
      if (is.null(b)) NULL else k & b
    }
    sub <- if (is.null(keep)) df else df[keep, , drop = FALSE]
    if (identical(r$id, "adsl_sub") && "USUBJID" %in% names(sub)) adsl_ids <- sub$USUBJID
    out[[r$id]] <- list(
      id = r$id, head = utils::head(sub, 5L), n_rows = if (is.null(keep)) NA_integer_ else nrow(sub),
      n_subj = if ("USUBJID" %in% names(sub)) length(unique(sub$USUBJID)) else NA_integer_)
  }
  out
}
