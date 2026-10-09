# ============================================================================
#  The review of a study's definition (#288)
# ----------------------------------------------------------------------------
#  tflspec::tfl_review_spec() reviews what the sheets, the catalogs and the
#  facts of the data say; what needs the study's report list, its folder or
#  the home is reviewed here: a report with no title or analysis set, two
#  reports writing one file, analyses of no report, a dataset with no file,
#  the deep check (the definition read back as the programs read it).  The
#  facts of the data (tflspec::tfl_data_facts()) are kept in the store, one
#  file a dataset, made again only for a dataset whose file or definition
#  changed.  Each row's message is built from its rule's template in the
#  app's language; tflspec's English is kept as `message_en`.
# ============================================================================

#' Review a study's definition
#'
#' `study_review()` lists, for the whole study or some reports, what is
#' wrong with the definition (`error`), what is valid but probably wrong
#' (`check`) and what is missing and has to be set by hand (`hand`): the
#' rules of [tflspec::tfl_review_rules()], those of the report list and the
#' study folder among them.  `data = "cached"` checks against the facts of
#' the data kept from the last read and reads no file; `"read"` reads the
#' datasets whose file or definition changed since; `"none"` reviews the
#' definition alone.  `review_problems()` reviews a definition with no study
#' (the planner alone: what an import's draft and the tests call).
#' `review_facts()` gives the facts the review checks against.
#'
#' @param study An `rtfstudy` ([open_study()]).
#' @param output_id Only these reports; `NULL` reviews them all and the
#'   study-wide rows.
#' @param data `"cached"`, `"read"` or `"none"`: see above.
#' @param ard `TRUE` also checks each report's table against what the
#'   study ARD holds, and lists what went wrong while it was made.
#' @param deep `TRUE` also reads the definition back as the report programs
#'   will ([check_planner()]): slow, one read a report.
#' @param home The tflplanner home.
#' @param lang The language of the messages (the app's by default).
#' @param progress A function called with each dataset's name before its
#'   file is read (`data = "read"`), or `NULL`.
#' @return A `tflspec::tfl_review`, its `message` in the app's language and
#'   `message_en` in English; `attr(, "facts_made")`: when the facts of the
#'   data were made (`NA`: none were used); `attr(, "off")`: the rules the
#'   company standards leave out (their settings' `review_off`, `A12 | C02`),
#'   whose rows are not there.
#' @seealso [tflspec::tfl_review_spec()], [tflspec::tfl_review_rules()]
#' @export
study_review <- function(study, output_id = NULL, data = c("cached", "read", "none"),
                         ard = TRUE, deep = FALSE, home = tflplanner_home(),
                         lang = tflplanner_language(), progress = NULL) {
  data <- match.arg(data)
  facts <- if (!identical(data, "none")) {
    review_facts(study, read = identical(data, "read"), home = home,
                 progress = progress)
  }
  if (isTRUE(ard)) {
    a <- tryCatch(.review_ard_facts(study, home), error = function(e) NULL)
    if (length(a)) {
      if (is.null(facts)) facts <- list()
      facts$ard <- a
    }
  }
  p <- study$planner
  r <- .review_planner(p, facts)
  # a figure's dataset the catalog has but whose facts were not read yet is
  # not missing (D02 says it is not reviewed yet)
  if (inherits(facts, "tfl_data_facts") && nrow(r)) {
    gone <- sub("^.*no dataset ([^ ]+).*$", "\\1", r$message)
    unread <- setdiff(toupper(p$ard$datasets$dataset), names(facts$datasets))
    drop <- r$rule == "F03" & grepl("no dataset ", r$message) & toupper(gone) %in% unread
    if (any(drop)) r <- .review_finish(r[!drop, , drop = FALSE])
  }
  more <- list(.rule_d01(study))
  if (inherits(facts, "tfl_data_facts")) {
    more <- c(more, list(.rule_d02(study, attr(facts, "not_read"))))
  }
  if (isTRUE(deep)) more <- c(more, list(.rule_p01(p)))
  r <- .review_bind(c(list(r), more))
  r <- .review_narrow(r, output_id)
  r <- .review_off(r)
  r <- .review_language(r, lang)
  attr(r, "facts_made") <- if (inherits(facts, "tfl_data_facts")) facts$made else NA
  attr(r, "facts_of") <- if (inherits(facts, "tfl_data_facts")) names(facts$datasets) else character()
  r
}

# The rules the company standards leave out (settings `review_off`:
# `A12 | C02`): their rows dropped, said in attr(, "off")
.review_off <- function(r) {
  off <- toupper(.split_bar(.std_setting("review_off", "")))
  if (length(off)) {
    keep <- attributes(r)
    keep <- keep[intersect(names(keep), c("facts_made", "facts_of"))]
    r <- .review_finish(r[!toupper(r$rule) %in% off, , drop = FALSE])
    for (k in names(keep)) attr(r, k) <- keep[[k]]
  }
  attr(r, "off") <- off
  r
}

#' @rdname study_review
#' @param x A planner ([new_planner()], [read_planner()]).
#' @param facts The facts of the data ([review_facts()]), or `NULL`.
#' @export
review_problems <- function(x, output_id = NULL, facts = NULL,
                            lang = tflplanner_language()) {
  r <- .review_planner(x, facts)
  r <- .review_narrow(r, output_id)
  .review_language(.review_off(r), lang)
}

# The review of a planner: tflspec's rules on its sheets, and the report
# list's own
.review_planner <- function(p, facts = NULL) {
  st <- p$study[!is.na(p$study)]
  spec <- c(p$sheets, list(study = if (length(st)) st))
  spec$study <- spec$study %||% NULL
  r <- tflspec::tfl_review_spec(spec = spec, ard = p$ard, listings = p$lf,
                                figures = p$fig_designs, facts = facts)
  .review_bind(list(r, .rules_report_list(p), .rules_ard_list(p)))
}

# rows of the review (tflspec's .rv() shape), with the catalog's level,
# area and hint
.rv_row <- function(rule, output_id = NA_character_, sheet = "", row = "",
                    field = "", args = character(), level = NULL) {
  cat <- .review_catalog()
  k <- match(rule, cat$rule)
  args <- as.character(args)
  msg <- tryCatch(do.call(sprintf, c(list(cat$message[k]), as.list(args))),
                  error = function(e) paste(args, collapse = " "))
  out <- data.frame(output_id = as.character(output_id),
                    level = level %||% cat$level[k], area = cat$area[k],
                    sheet = sheet, row = row, field = field, message = msg,
                    hint = cat$hint[k], rule = rule, draft = FALSE,
                    stringsAsFactors = FALSE)
  out$args <- list(args)
  out$fix <- list(NULL)
  out
}

.review_catalog <- function() {
  if (is.null(.review_memo$cat)) .review_memo$cat <- tflspec::tfl_review_rules()
  .review_memo$cat
}
.review_memo <- new.env(parent = emptyenv())

.review_bind <- function(parts) {
  parts <- Filter(function(d) is.data.frame(d) && nrow(d), parts)
  if (!length(parts)) {
    return(.review_finish(.rv_row("R01")[0, , drop = FALSE]))
  }
  cols <- names(parts[[1L]])
  .review_finish(do.call(rbind, lapply(parts, function(d) {
    class(d) <- "data.frame"
    d[cols]
  })))
}

# errors, checks, what to set by hand; the study's rows, then each report's
.review_finish <- function(r) {
  key <- paste(r$output_id, r$level, r$sheet, r$row, r$field, r$message, sep = "\r")
  r <- r[!duplicated(key), , drop = FALSE]
  o <- order(match(r$level, c("error", "check", "hand")), !is.na(r$output_id),
             r$output_id, method = "radix")
  r <- r[o, , drop = FALSE]
  rownames(r) <- NULL
  class(r) <- c("tfl_review", "data.frame")
  r
}

.review_narrow <- function(r, output_id) {
  if (is.null(output_id)) return(r)
  .review_finish(r[!is.na(r$output_id) & r$output_id %in% output_id, , drop = FALSE])
}

# Each row's message from its rule's template in `lang` (the English
# template is the key of strings.csv); the English kept as message_en
.review_language <- function(r, lang) {
  r$message_en <- r$message
  if (!nrow(r) || identical(lang, "en")) return(r)
  cat <- .review_catalog()
  k <- match(r$rule, cat$rule)
  for (i in seq_len(nrow(r))) {
    tpl <- cat$message[k[i]]
    if (is.na(tpl)) next
    # a rule whose message is another's words (a figure's advice, the
    # constructors' problems): those words, where the app has them
    if (identical(tpl, "%s")) {
      loc <- tr(r$message[i], lang)
      if (!identical(loc, r$message[i])) r$message[i] <- loc
      h <- cat$hint[k[i]]
      if (!is.na(h) && nzchar(h)) r$hint[i] <- tr(h, lang)
      next
    }
    loc <- tr(tpl, lang)
    a <- r$args[[i]]
    if (!identical(loc, tpl)) {
      m <- tryCatch(do.call(sprintf, c(list(loc), as.list(a))), error = function(e) NA)
      if (!is.na(m)) r$message[i] <- m
    }
    h <- cat$hint[k[i]]
    if (!is.na(h) && nzchar(h)) r$hint[i] <- tr(h, lang)
  }
  r
}

# ---- the report list (R01-R08) ----------------------------------------------

.rules_report_list <- function(p) {
  o <- p$outputs
  if (is.null(o) || !nrow(o)) return(NULL)
  out <- list()
  add <- function(...) out[[length(out) + 1L]] <<- .rv_row(...)
  ids <- o$output_id
  # R02: an id twice; R03: an id a file name cannot hold
  for (id in unique(ids[duplicated(ids)])) {
    add("R02", id, "_tflplanner", id, "output_id", args = id)
  }
  for (i in seq_along(ids)) {
    ok <- tryCatch({
      .check_id(ids[i])
      TRUE
    }, error = function(e) FALSE)
    if (!ok) add("R03", ids[i], "_tflplanner", ids[i] %|% "", "output_id",
                 args = ids[i] %|% "")
  }
  ids <- unique(ids[!is.na(ids) & nzchar(trimws(ids))])
  pops <- p$ard$populations$population_id
  a <- p$ard$analyses
  ad_all <- .adata_rows(p)
  tok <- p$sheets$tokens
  files <- character()
  progs <- character()
  for (id in ids) {
    info <- tryCatch(report_info(p, id), error = function(e) NULL)
    type <- info$type %||% "table"
    files[id] <- info$file %||% NA
    progs[id] <- info$program %||% NA
    # R01: no title -- no line of the titles (its own, the defaults), no
    # OUTPUT_TITLE of its own
    own_title <- tok$value[!is.na(tok$output_id) & tok$output_id == id &
                             tok$name %in% "OUTPUT_TITLE"]
    if (!length(.title_lines(p, id)) && !any(nzchar(trimws(own_title %|% "")))) {
      add("R01", id, "titles", "", "", args = id)
    }
    pop <- report_population(p, id)
    ao <- a[!is.na(a$output_id) & a$output_id == id, , drop = FALSE]
    # R05: an analysis set the populations do not have
    if (!is.na(pop) && !pop %in% pops) {
      add("R05", id, "_tflplanner", id, "population", args = c(pop, id))
    }
    if (identical(type, "table")) {
      # R04: a table with no analysis set, and analyses that name none (own
      # code chooses its data itself)
      own_code <- nrow(ao) && all(ao$method %in% "custom")
      if (is.na(pop) && nrow(ao) && !own_code && !length(.review_pops_of(ad_all, ao))) {
        add("R04", id, "_tflplanner", id, "population", args = id)
      }
      # R06: nothing makes its ARD
      if (!nrow(ao) && .is_blank(o$data_code[match(id, o$output_id)]) &&
          is.null(.ard_import_of(p, id))) {
        add("R06", id, "analyses", "", "", args = c(id, "no analyses and no data code"))
      }
    } else if (identical(type, "listing")) {
      cl <- p$lf$listing_cols
      if (!any(cl$output_id %in% id)) {
        add("R06", id, "listing_cols", "", "", args = c(id, "no listing columns"))
      }
    } else if (identical(type, "figure")) {
      if (is.null(p$fig_designs[[id]])) {
        add("R06", id, "design", "", "", args = c(id, "no figure design"))
      }
    }
    # R08: a dataset the report list names that the catalog does not have
    lst <- .split_bar(o$datasets[match(id, o$output_id)] %||% NA)
    for (d in setdiff(toupper(lst), toupper(p$ard$datasets$dataset))) {
      add("R08", id, "_tflplanner", id, "datasets", args = c(id, d))
    }
  }
  # R07: two reports writing one file or one program
  for (what in c("file", "program")) {
    v <- if (identical(what, "file")) files else progs
    v <- v[!is.na(v)]
    for (x in unique(v[duplicated(v)])) {
      who <- names(v)[v == x]
      add("R07", who[2L], "report", who[2L], what, args = c(who[1L], who[2L], what, x))
    }
  }
  .review_bind(out)
}

`%|%` <- function(a, b) ifelse(is.na(a), b, a)

# The analysis sets a report's analyses read: their own, or their analysis
# data's
.review_pops_of <- function(ad_all, ao) {
  ad <- ad_all[!is.na(ad_all$output_id) & ad_all$output_id %in% unique(ao$output_id), ,
               drop = FALSE]
  v <- ao$population_id[!is.na(ao$population_id)]
  d <- if (!is.null(ao$data)) ao$data[!is.na(ao$data)] else character()
  v <- c(v, vapply(d[d %in% ad$data_id], function(i) .adata_pop(ad, i), ""))
  unique(v[!is.na(v) & nzchar(v)])
}

# ---- the analyses (A11-A13) ---------------------------------------------------

.rules_ard_list <- function(p) {
  a <- p$ard$analyses
  if (is.null(a) || !nrow(a)) return(NULL)
  out <- list()
  add <- function(...) out[[length(out) + 1L]] <<- .rv_row(...)
  ids <- p$outputs$output_id
  ad_all <- .adata_rows(p)
  # A11: analyses of a report the list does not have
  for (id in setdiff(unique(stats::na.omit(a$output_id)), ids)) {
    first <- a$analysis_id[which(a$output_id %in% id)[1L]]
    add("A11", id, "analyses", first %|% "", "output_id", args = id)
  }
  for (id in intersect(unique(stats::na.omit(a$output_id)), ids)) {
    ao <- a[!is.na(a$output_id) & a$output_id == id, , drop = FALSE]
    pop <- report_population(p, id)
    # A12: an analysis reading another analysis set than the report's
    if (!is.na(pop)) {
      ad <- ad_all[!is.na(ad_all$output_id) & ad_all$output_id == id, , drop = FALSE]
      for (i in seq_len(nrow(ao))) {
        own <- ao$population_id[i]
        dat <- if (!is.null(ao$data)) ao$data[i] else NA
        if (is.na(own) && !is.na(dat) && dat %in% ad$data_id) own <- .adata_pop(ad, dat)
        if (!is.na(own) && nzchar(own) && !identical(own, pop)) {
          add("A12", id, "analyses", ao$analysis_id[i] %|% "",
              if (!is.na(ao$population_id[i])) "population_id" else "data",
              args = c(ao$analysis_id[i] %|% "", own, pop))
        }
      }
    }
    # A13: the subjects per group counted twice
    tw <- tryCatch(stack_n_twice(ao), error = function(e) NULL)
    for (i in seq_len(NROW(tw))) {
      add("A13", id, "analyses", tw$analysis_id[i], "", args = tw$analysis_id[i])
    }
  }
  .review_bind(out)
}

# ---- the study folder (D01, D02, P01) ----------------------------------------

# D01: a catalog dataset whose file is not there -- once for the study, and
# under each report that reads it
.rule_d01 <- function(study) {
  p <- study$planner
  d <- p$ard$datasets
  if (is.null(d) || !nrow(d)) return(NULL)
  gone <- d$dataset[!is.na(d$path) &
                      !file.exists(file.path(study$path, d$path))]
  if (!length(gone)) return(NULL)
  out <- lapply(gone, function(g) .rv_row("D01", NA_character_, "datasets", g, "path",
                                          args = g))
  for (id in p$outputs$output_id) {
    used <- tryCatch(toupper(.report_datasets(p, id)), error = function(e) character())
    for (g in intersect(toupper(gone), used)) {
      out[[length(out) + 1L]] <- .rv_row("D01", id, "datasets", g, "path", args = g)
    }
  }
  .review_bind(out)
}

# D02: a dataset whose facts were not made yet (data = "cached")
.rule_d02 <- function(study, not_read) {
  if (!length(not_read)) return(NULL)
  .review_bind(lapply(not_read, function(g)
    .rv_row("D02", NA_character_, "datasets", g, "", args = g)))
}

# P01: the deep check, the definition read back as the programs read it
.rule_p01 <- function(p) {
  ck <- tryCatch(check_planner(p), error = function(e) NULL)
  if (is.null(ck)) return(NULL)
  bad <- ck[!ck$ok, , drop = FALSE]
  .review_bind(lapply(seq_len(nrow(bad)), function(i)
    .rv_row("P01", if (identical(bad$output_id[i], "(workbook)")) NA_character_ else
      bad$output_id[i], "", "", "", args = bad$message[i])))
}

# ---- the facts of the data, kept in the store --------------------------------

#' @rdname study_review
#' @param refresh `TRUE` makes the facts again whatever is kept.
#' @param read `FALSE` reads no file: the facts kept, the datasets with none
#'   listed in `attr(, "not_read")`.
#' @export
review_facts <- function(study, refresh = FALSE, read = TRUE,
                         home = tflplanner_home(), progress = NULL) {
  p <- study$planner
  dir <- file.path(.store_dir(study$meta$study_id, home), "facts")
  if (isTRUE(refresh)) unlink(dir, recursive = TRUE)
  ds <- p$ard$datasets
  ds <- ds[!is.na(ds$dataset) & !is.na(ds$path), , drop = FALSE]
  path_of <- function(d) {
    k <- match(toupper(d), toupper(ds$dataset))
    if (is.na(k)) NA_character_ else file.path(study$path, ds$path[k])
  }
  sig <- function(f) {
    if (is.na(f) || !file.exists(f)) return(NULL)
    i <- file.info(f)
    list(path = normalizePath(f, "/"), mtime = as.numeric(i$mtime), size = i$size)
  }
  pops <- p$ard$populations
  pop_ds <- unique(toupper(stats::na.omit(pops$dataset)))
  units <- list()
  not_read <- character()
  for (d in ds$dataset) {
    f <- path_of(d)
    s <- sig(f)
    if (is.null(s)) next
    key <- list(file = s, pop_files = lapply(stats::setNames(pop_ds, pop_ds),
                                             function(x) sig(path_of(x))),
                populations = pops, datasets = ds, rows = .facts_rows_of(p, d),
                tflspec = as.character(utils::packageVersion("tflspec")))
    cf <- file.path(dir, paste0(.r_file_name(d), ".rds"))
    hit <- if (file.exists(cf)) tryCatch(readRDS(cf), error = function(e) NULL)
    if (!is.null(hit) && identical(hit$key, key)) {
      units[[d]] <- hit$facts
      next
    }
    if (!isTRUE(read)) {
      not_read <- c(not_read, d)
      next
    }
    if (is.function(progress)) progress(d)
    u <- .facts_unit(p, d, path_of, pop_ds)
    if (is.null(u)) next
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(key = key, facts = u), cf)
    units[[d]] <- u
  }
  .facts_assemble(units, not_read)
}

# a dataset's name as a file name
.r_file_name <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)

# The facts of one dataset: its columns, the analysis sets made of it, and
# the conditions on it (the analysis sets' datasets read too, for their
# subjects)
.facts_unit <- function(p, d, path_of, pop_ds) {
  need <- unique(c(toupper(d), pop_ds))
  data <- list()
  for (x in need) {
    f <- path_of(x)
    if (is.na(f) || !file.exists(f)) next
    v <- tryCatch(as.data.frame(read_data_head(f, n = .Machine$integer.max)),
                  error = function(e) NULL)
    if (!is.null(v)) data[[x]] <- v
  }
  if (is.null(data[[toupper(d)]])) return(NULL)
  f <- tflspec::tfl_data_facts(data, populations = p$ard,
                               listings = tryCatch(tflspec::tfl_listing_spec(p$lf, check = FALSE),
                                                   error = function(e) NULL))
  po <- p$ard$populations
  mine <- po$population_id[toupper(po$dataset) %in% toupper(d)]
  cd <- f$conditions
  list(dataset = f$datasets[[toupper(d)]],
       populations = f$populations[intersect(names(f$populations), mine)],
       conditions = cd[toupper(cd$dataset) %in% toupper(d), , drop = FALSE],
       max_levels = f$max_levels, id = f$id, made = f$made)
}

# The definition rows that read dataset `d`: its analysis data (and those
# made from them), the analyses reading it or them, a listing of it -- a
# change elsewhere does not make its facts again
.facts_rows_of <- function(p, d) {
  a <- p$ard$analyses
  ad <- .adata_rows(p)
  D <- toupper(d)
  mine <- toupper(ad$from) %in% D
  repeat {
    more <- !mine & paste(ad$output_id, ad$from) %in%
      paste(ad$output_id[mine], ad$data_id[mine])
    if (!any(more)) break
    mine <- mine | more
  }
  ids <- paste(ad$output_id[mine], ad$data_id[mine])
  po <- p$ard$populations
  pop_d <- po$population_id[toupper(po$dataset) %in% D]
  dat <- if (!is.null(a$data)) a$data else rep(NA_character_, nrow(a))
  ar <- toupper(a$dataset) %in% D | paste(a$output_id, dat) %in% ids |
    (is.na(a$dataset) & is.na(dat) & a$population_id %in% pop_d)
  # those inside a parent that reads it
  par <- a$parent %||% rep(NA_character_, nrow(a))
  ar <- ar | paste(a$output_id, par) %in% paste(a$output_id[ar], a$analysis_id[ar])
  l <- p$lf$listings
  list(analysis_data = ad[mine, , drop = FALSE], analyses = a[ar, , drop = FALSE],
       listings = if (!is.null(l)) l[toupper(l$dataset) %in% D, , drop = FALSE])
}

.facts_assemble <- function(units, not_read) {
  cond <- do.call(rbind, c(list(NULL), lapply(units, `[[`, "conditions")))
  if (is.null(cond)) {
    cond <- data.frame(dataset = character(), population_id = character(),
                       where = character(), n_rows = integer(), n_subjects = integer(),
                       error = character(), stringsAsFactors = FALSE)
    cond$values <- list()
  }
  rownames(cond) <- NULL
  made <- if (length(units)) max(do.call(c, lapply(units, `[[`, "made"))) else NA
  f <- structure(list(datasets = lapply(units, `[[`, "dataset"),
                      populations = do.call(c, c(list(list()), unname(lapply(units, `[[`, "populations")))),
                      conditions = cond, ard = list(),
                      id = if (length(units)) units[[1L]]$id else "USUBJID",
                      max_levels = if (length(units)) units[[1L]]$max_levels else 200L,
                      made = made),
                 class = "tfl_data_facts")
  names(f$datasets) <- toupper(names(f$datasets))
  attr(f, "not_read") <- not_read
  f
}

# What each report's ARD holds (the study ARD's rows of it, an ARD taken
# in), kept in the store while the files do not change
.review_ard_facts <- function(study, home = tflplanner_home()) {
  p <- study$planner
  out <- .ard_study_value(p$ard, "output", "output/ard/ard.rds")
  f <- file.path(study$path, out)
  if (!file.exists(f)) return(list())
  i <- file.info(f)
  key <- list(path = normalizePath(f, "/"), mtime = as.numeric(i$mtime), size = i$size,
              tflspec = as.character(utils::packageVersion("tflspec")))
  cf <- file.path(.store_dir(study$meta$study_id, home), "facts", "_ard.rds")
  hit <- if (file.exists(cf)) tryCatch(readRDS(cf), error = function(e) NULL)
  if (!is.null(hit) && identical(hit$key, key)) return(hit$facts)
  a <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(a) || !"output_id" %in% names(a)) return(list())
  ids <- unique(stats::na.omit(as.character(unlist(a$output_id))))
  oid <- as.character(unlist(lapply(a$output_id, function(x) x[1L])))
  facts <- lapply(stats::setNames(ids, ids), function(id)
    tflspec::tfl_ard_facts(a[oid %in% id, , drop = FALSE]))
  dir.create(dirname(cf), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(key = key, facts = facts), cf)
  facts
}
