# A report's analysis set: one value, the report list's `population`
# (outputs$population, a population_id).  The TOC fills it, the report list
# and step 1 change it; step 1's defaults read it.  The analysis data of the
# set's subjects (adsl_<set>) is made when a report's set is chosen, and a
# report whose set changes has its analyses moved to the new set's data.

# a report's analysis set (NA: none given).  A listing's is its data's: its
# row's `population_id` (what a save writes onto its `data: population`),
# else the report list's -- tflspec's rule for the sheets (the data's own
# set wins, else the report's: .data_population(), convert), so its
# program, step 1 and the TOC read one set (review S5)
report_population <- function(x, output_id) {
  v <- .listing_row_set(x, output_id)
  if (!is.na(v)) return(v)
  o <- x$outputs
  v <- if (is.null(o$population)) NA_character_ else
    o$population[match(output_id, o$output_id)]
  if (length(v) != 1L || .is_blank(v)) NA_character_ else v
}
# a listing's row's own set (NA: none, or no listing row)
.listing_row_set <- function(x, output_id) {
  lr <- if (is.list(x[["lf"]])) x[["lf"]][["listings"]]
  if (length(output_id) != 1L || is.null(lr) || is.null(lr[["population_id"]])) return(NA_character_)
  v <- lr[["population_id"]][match(output_id, lr[["output_id"]])]
  if (length(v) != 1L || .is_blank(v)) NA_character_ else as.character(v)
}
# The listing sheets with a listing's set as report_population() reads it
# (its own, else the report's) on its row: what its program is written
# from, as tflspec writes the saved listing's (review S5: the planner's
# program and tflspec's, of one SPEC, keep the same subjects).  A row that
# reads one of the report's analysis data (`data`) has that data's set
# (.data_population()): left as it is
.lf_with_report_set <- function(x, output_id) {
  lf <- x[["lf"]]
  lr <- if (is.list(lf)) lf[["listings"]]
  pop <- report_population(x, output_id)
  if (is.null(lr) || is.na(pop)) return(lf)
  mine <- !is.na(lr[["output_id"]]) & lr[["output_id"]] == output_id
  if (!is.null(lr[["data"]])) mine <- mine & .is_blank_v(lr[["data"]])
  if (!any(mine)) return(lf)
  if (is.null(lr[["population_id"]])) lr[["population_id"]] <- rep(NA_character_, nrow(lr))
  lr[["population_id"]][mine] <- pop
  lf[["listings"]] <- lr
  lf
}

# The study's analysis sets: what every control that offers a report's set
# offers -- the report list's, step 1's, a figure's, the TOC's, the Create
# dialog's, a template's data card, an analysis data's Population (canon
# 11-15 (2): a set is registered in the study, spec/data.yml `populations:`,
# and a report picks one of them; a flag no set says is no choice).  The
# rows of the populations sheet (data.yml's `populations:` as the planner
# holds it, its edits not saved yet with them) whose id tflspec's
# .study_set_ids() takes as the study's, read from the study's data
# catalog with those rows in it.  attr "none" TRUE when the study registers
# no set (data.yml names none): a control offers none and says so
# (.study_sets_none_label()).  A study with no data catalog (no
# spec/data.yml; a planner with no YAML spec), or a tflspec without the
# helper, has sets that are not known: the sheet's rows, as before.
.study_set_rows <- function(p) {
  po <- p$ard$populations
  if (is.null(po) || !nrow(po)) {
    po <- data.frame(population_id = character(), dataset = character(), where = character(),
                     stringsAsFactors = FALSE)
  }
  po <- po[!.is_blank_v(po$population_id), , drop = FALSE]
  ids <- .study_set_ids_now(p, po$population_id)
  if (!is.null(ids)) po <- po[po$population_id %in% ids, , drop = FALSE]
  rownames(po) <- NULL
  attr(po, "none") <- !is.null(ids) && !length(ids)
  po
}
# tflspec's .study_set_ids() of the study's data catalog with the
# populations sheet's ids `sheet` as its `populations:` (what a save
# writes there); NULL when the sets are not known
.study_set_ids_now <- function(p, sheet) {
  f <- get0(".study_set_ids", envir = asNamespace("tflspec"), mode = "function", inherits = FALSE)
  sp <- p[["spec"]]
  data <- if (is.list(sp) && is.list(sp[["study"]])) sp[["study"]][["data"]]
  if (is.null(f) || !length(data)) return(NULL)
  data[["populations"]] <- lapply(unique(as.character(sheet)), function(id) list(id = id))
  f(data)
}
# the "none" item of a control of a report's set: `none` ("(not chosen)"),
# or, the study registering no set, that it has none to offer
.study_sets_none_label <- function(rows, t = identity, none = t("(not chosen)")) {
  if (isTRUE(attr(rows, "none"))) {
    t("(none: the study has no analysis set; register one in spec/data.yml populations)")
  } else none
}

# An analysis set's label: the populations sheet's `label`, else the company
# standards' (their populations sheet's note), else the label of its flag in `data` (SAFFL: "Safety
# Population Flag"), else its id
.pop_label <- function(x, pop, data = NULL) {
  po <- x$ard$populations
  k <- match(pop, po$population_id)
  if (is.na(k)) return(pop)
  # the sheet's own label first, then the company standards' note
  lab <- if (!is.null(po$label)) po$label[k] else NA_character_
  if (!.is_blank(lab)) return(lab)
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
  nm <- .adata_free_name(x, output_id, .adata_default_name(from, pop))
  x <- set_analysis_data(x, output_id, nm, from = from, population_id = pop)
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
  if (identical(r$population_id, from_pop) && .is_blank(r$subjects_in)) {
    pop <- to_pop
    subj <- NA_character_
  } else if (!.is_blank(r$subjects_in)) {
    x <- .adata_for_pop(x, output_id, r$subjects_in, from_pop, to_pop)
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
      identical(.blank_na(ad$subjects_in[j]), .blank_na(subj)) &&
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
  x <- set_analysis_data(x, output_id, nm, from = r$from, population_id = pop, subjects_in = subj,
                         where = r$where, add = r$add, derive = r$derive, keep = r$keep,
                         distinct = r$distinct)
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
  ds <- x$ard$datasets$dataset
  subs <- as.vector(outer(paste0(rn(ds), "_sub"), c("", 2:9), paste0))
  c(rn(ds), paste0("pop_", rn(x$ard$populations$population_id)), subs,
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
#' none is removed.  The set is written where the report's data keeps it:
#' a listing's row (its `population_id`); in the study's YAML spec, a
#' figure's and a listing's data (`data: population`), a table's `common:`,
#' and the report's TOC line with it.
#'
#' @param x A `tflplanner`.
#' @param output_id The report.
#' @param population A population_id of the study (`NA`: none).
#' @return The `tflplanner`, with attributes `made` (the analysis data
#'   added) and `left`.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (a few seconds: the sample study and its data)
#' s <- create_sample_study(run = FALSE)
#' # the report moves from the safety set to the screened set
#' p <- set_report_population(s$planner, "T-14-1-2", "ENR")
#' p$outputs$population[p$outputs$output_id == "T-14-1-2"]
#' }
#' options(old)
#' @export
set_report_population <- function(x, output_id, population) {
  if (length(output_id) != 1L) stop("One report at a time.", call. = FALSE)
  .write_report_population(x, output_id, population)
}

# The one way a report's analysis set is written: the report list, step 1,
# the TOC taken in, the Add dialog and a template report's writes all come
# here (codex review 2026-11-26, 3B).  `output_id` the reports, `population`
# their sets, one each (recycled; NA none).  The set goes where the
# report's data keeps it:
# - the planner's own (no `output`): the report list's column, a listing's
#   rows (`population_id`: a save writes it onto the listing's data), the
#   set's analysis data made and the report's analyses moved to it
#   (.report_set_view(); attributes `made` and `left`, every report's);
# - a report the YAML spec has: its file's set as .o_set_population() puts
#   it (a figure's and a data map's on the data, else `common:`), its toc
#   line following it (.spec_set_toc_population()), and the view's rows
#   made again from the file (.spec_set_output()), so the save does not
#   take it back.  The planner's own reports the spec has are written after
#   one sync of the view's edits (the analyses moved); a report new to the
#   view reaches the spec by the save, from the column and rows set here.
#   (A report that shares its file with others: the save's, as before.)
# - a template report whose template takes the set as an argument of its
#   own (.tpl_pop_is_arg(): forest_hr, tflspec_hierarchy ...): that
#   argument too, the same value, so the two never disagree (.o_write_set();
#   review H5).
# `output`: one report's tfl_output written with its set (a template
# report's argument or step write, what it writes beside the set as it
# is); `population` NULL keeps the set it has (its argument made the
# same).  Its analyses are its template's: none moved, no data made.
.write_report_population <- function(x, output_id, population = NULL, output = NULL) {
  n <- length(output_id)
  if (!is.null(output) && n != 1L) stop("One report's file is written at a time.", call. = FALSE)
  if (!is.null(output) && is.null(x$spec)) stop("The planner has no YAML spec.", call. = FALSE)
  pops <- NULL
  if (!is.null(population)) {
    pops <- as.character(rep_len(population, n))
    pops[vapply(pops, .is_blank, NA)] <- NA_character_
  }
  if (is.null(output)) {
    if (is.null(pops)) pops <- rep(NA_character_, n)
    made <- left <- character()
    for (k in seq_len(n)) {
      x <- .report_set_view(x, output_id[k], pops[k])
      made <- c(made, attr(x, "made"))
      left <- c(left, attr(x, "left"))
    }
  }
  if (!is.null(x$spec)) {
    ids <- if (!is.null(output)) output_id else intersect(output_id, names(x$spec$outputs))
    if (is.null(output) && length(ids)) {
      # (the view's edits -- the analyses moved, the data made -- in the
      # spec first: the file is written from it)
      x <- .spec_sync(x, x$spec$meta)
      attr(x, "changed") <- NULL
    }
    for (id in ids) {
      o <- output %||% x$spec$outputs[[id]]
      if (is.null(o)) next
      if (!is.null(pops)) {
        o <- .o_write_set(o, pops[match(id, output_id)])
      } else {
        # (the set kept: its argument made the same -- the set where the
        # data keeps it, else, an older file's, the argument's)
        now <- .o_population(o)
        if (is.na(now)) now <- .o_arg_population(o)
        if (!is.na(now)) o <- .o_write_set(o, now)
      }
      if (is.null(output)) {
        w <- x$spec$where
        rel <- if (id %in% names(w)) w[[id]]
        # (a file several reports share is written by the save, as before;
        # its TOC line follows the set now all the same)
        if (!is.null(rel) && sum(unlist(w) == rel) > 1L) {
          x <- .spec_set_toc_population(x, id, .o_population(o))
          next
        }
      }
      x <- .spec_set_output(.spec_set_toc_population(x, id, .o_population(o)), id, o)
    }
  }
  if (is.null(output)) {
    attr(x, "made") <- unique(made)
    attr(x, "left") <- unique(left)
  }
  x
}

# One report's file with its set (NA: none) in every place it lives:
# where its data keeps it (.o_set_population()) and, when its template
# takes the set as an argument of its own (a shown `population`
# parameter, .tpl_pop_is_arg()), that argument -- the same value, or taken
# out with it.  The one rule .write_report_population() writes by (H5).
.o_write_set <- function(o, population) {
  o <- .o_set_population(o, population)
  if (!.o_takes_set_arg(o)) return(o)
  has <- length(population) && !is.na(population[1L]) && nzchar(population[1L])
  t <- o$template
  args <- .tpl_args_of(t)
  if (!has && is.null(args$population)) return(o)
  if (has && identical(as.character(unlist(args$population)), as.character(population[1L]))) return(o)
  if (has) args["population"] <- list(as.character(population[1L])) else args$population <- NULL
  # (in the place the file has it, a new one in its step's: .tpl_write_args())
  o$template <- .tpl_write_args(t, args, by_step = TRUE)
  o
}

# Whether a report's template takes its set as an argument of its own
# (a template SPEC with a shown `population` parameter; FALSE for a report
# of no template, a figure designer's name, a template not found)
.o_takes_set_arg <- function(o) {
  t <- o$template
  if (!is.list(t) || is.null(t$name)) return(FALSE)
  isTRUE(tryCatch(.tpl_pop_is_arg(.tpl_params(as.character(t$name)[1L])), error = function(e) FALSE))
}

# The set a report's template argument says (NA: none, or no such argument)
.o_arg_population <- function(o) {
  if (!.o_takes_set_arg(o)) return(NA_character_)
  v <- as.character(unlist(.tpl_args_of(o$template)$population %||% NA_character_))[1L]
  if (is.na(v) || !nzchar(v)) NA_character_ else v
}

# The planner's own part of .write_report_population() for one report:
# the report list's column, a listing's rows, the set's data made and the
# report's analyses moved (attributes `made`, `left`)
.report_set_view <- function(x, output_id, population) {
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
  # (a listing's row holds its set: the save writes it onto its data)
  lr <- x$lf$listings
  if (!is.null(lr) && "population_id" %in% names(lr)) {
    mine <- !is.na(lr$output_id) & lr$output_id == output_id
    if (any(mine)) {
      lr$population_id[mine] <- new
      x$lf$listings <- lr
    }
  }
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
