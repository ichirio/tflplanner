# The study's model is its YAML spec (#407, step B1 of the "YAML native"
# plan, _archive/tflplanner-yaml-native-plan-2026-10-17.md).  A planner of
# a study carries, in `p$spec`:
#
#   study    the study's spec, tflspec::tfl_read_study_spec() (study.yml,
#            data, defaults, codelists, patterns, toc)
#   outputs  the reports as their files say them (not resolved), a named
#            list of tfl_output, by id
#   text     every file's text (its lines), by its path relative to the
#            study folder: what was read, or what a save writes
#   where    the file each report is in, by id
#   base     the sheets' view (the planner without `spec`) the spec was
#            last in step with
#   meta     the study's fields then
#
# The sheets every screen reads (p$sheets, p$outputs, p$ard, p$lf,
# p$fig_designs, p$study, p$setup) are a view of the spec, made by
# tflspec::tfl_sheets_from_spec() (.planner_from_spec()).  A screen that
# still edits the sheets edits the view only; .spec_sync() takes the edits
# back into the spec through the bridge (tflspec::tfl_yaml_from_sheets(),
# what .planner_sheets() gives it) -- but only the files the edits change:
# the bridge is run on the view as it was (`base`) and as it is, and a file
# whose bridged text is the same both times keeps the spec's own text (and
# whatever of it the sheets cannot hold).  A save writes the files whose
# text differs from the disk's.  A part written straight into the spec
# (.spec_set_output(), the screens to come) has its view made again, for
# that report only.  A planner with no `spec` (a new study, the company
# standards, workbooks) is written whole from the bridge, as before.

# ------------------------------------------------------------- reading

# The spec of a study folder as the model holds it (NULL when the folder
# has no YAML spec)
.spec_load <- function(root) {
  if (is.null(root) || !dir.exists(file.path(root, study_layout()[["spec"]], "outputs"))) {
    return(NULL)
  }
  rels <- .spec_files_yaml(root)
  text <- stats::setNames(lapply(rels, function(r)
    readLines(file.path(root, r), encoding = "UTF-8", warn = FALSE)), rels)
  # the reports, in tfl_read_outputs()'s order (its files' list)
  out_dir <- file.path(root, study_layout()[["spec"]], "outputs")
  files <- list.files(out_dir, "\\.ya?ml$", recursive = TRUE)
  outputs <- list()
  where <- character()
  for (f in files) {
    r <- tflspec::tfl_read_output(file.path(out_dir, f))
    if (inherits(r, "tfl_output")) r <- list(r)
    rel <- file.path(study_layout()[["spec"]], "outputs", f)
    for (o in r) {
      if (o$id %in% names(outputs)) {
        stop("The report ", o$id, " is in two files: ", where[[o$id]], " and ", rel, ".",
             call. = FALSE)
      }
      outputs[[o$id]] <- o
      where[[o$id]] <- rel
    }
  }
  list(study = tflspec::tfl_read_study_spec(root), outputs = outputs, text = text,
       where = where, base = NULL, meta = NULL)
}

# The planner (the sheets' view) made from a spec in memory: the sheets,
# the report list and setup, the ARD definition, the listing and figure
# sheets and the figure designs, as read_planner() and the ARD and listing
# readers give them.  What the spec holds that the sheets cannot is the
# attribute `not_converted`.
.planner_from_spec <- function(study, outputs) {
  back <- tflspec::tfl_sheets_from_spec(study, outputs)
  p <- .planner_from_sheets(back$table, back$planner)
  old <- attr(p, "old_data_code")
  attr(p, "old_data_code") <- NULL
  p$ard <- stats::setNames(lapply(names(.ard_sheets()), function(s)
    .normalize_ard_sheet(back$ard[[s]] %||% data.frame(), s)), names(.ard_sheets()))
  p$lf <- .empty_lf()
  for (sh in intersect(.lf_sheet_names, names(back$listing))) {
    p$lf[[sh]] <- .normalize_lf_sheet(back$listing[[sh]], sh)
  }
  for (id in names(back$figures)) {
    # (a figure with nothing of the designer's -- no template, no layers,
    # no steps -- has no design)
    d <- .fig_norm(back$figures[[id]])
    if (length(Filter(length, d))) p <- set_fig_design(p, id, d)
  }
  p <- .migrate_step_code(p, old)
  p <- .renamed_outputs(p)
  attr(p, "not_converted") <- back$not_converted
  p
}

# The planner without its spec: the view the screens edit (what is compared
# to tell an edit; what the state keeps)
.planner_body <- function(p) {
  if (is.null(p)) return(NULL)
  p$spec <- NULL
  attr(p, "not_converted") <- NULL
  p
}

# Two planners the same, whatever spec they carry
.same_planner <- function(a, b) identical(.planner_body(a), .planner_body(b))

.spec_meta <- function(meta) {
  m <- lapply(stats::setNames(.study_fields, .study_fields), function(k) {
    v <- meta[[k]]
    if (is.null(v) || !length(v)) NA_character_ else as.character(v[1L])
  })
  m
}

# The spec and the view in step: the view as it is now is the one the spec
# was made from (open, register, reload, save)
.spec_rebase <- function(p, meta) {
  if (is.null(p$spec)) return(p)
  p$spec$base <- .planner_body(p)
  p$spec$meta <- .spec_meta(meta)
  p
}

# The study folder's spec put into a study's planner, in step with its
# view (NULL spec when the folder has none or it does not read: the
# planner is then written whole on save)
.spec_attach <- function(s) {
  sp <- tryCatch(.spec_load(s$path), error = function(e) NULL)
  s$planner$spec <- sp
  s$planner <- .spec_rebase(s$planner, s$meta)
  s
}

# ------------------------------------------------------------- the bridge

# The files the bridge writes from a view: their text by path (study.yml
# as tflspec writes it, without the study's own lines), and the folder
# they are in (the caller removes it)
.spec_bridge <- function(body, meta) {
  tmp <- tempfile("tflplanner-yaml")
  tflspec::tfl_yaml_from_sheets(.planner_sheets(body, meta), tmp, overwrite = TRUE)
  rel <- list.files(tmp, recursive = TRUE)
  rel <- setdiff(rel, file.path(study_layout()[["spec"]], "_not_converted.yml"))
  list(dir = tmp, text = stats::setNames(lapply(rel, function(r)
    readLines(file.path(tmp, r), encoding = "UTF-8", warn = FALSE)), rel))
}

# Which part of the spec a file is: study, data, defaults, codelists, toc,
# pattern (its name) or output
.spec_part <- function(rel) {
  sp <- study_layout()[["spec"]]
  if (identical(rel, .study_file)) return(list(kind = "study"))
  if (startsWith(rel, paste0(sp, "/outputs/"))) return(list(kind = "output"))
  if (startsWith(rel, paste0(sp, "/patterns/"))) {
    return(list(kind = "pattern", name = tools::file_path_sans_ext(basename(rel))))
  }
  list(kind = tools::file_path_sans_ext(basename(rel)))
}

# The edits of the view taken into the spec.  `meta`: the study's fields
# (and `created`).  Each file whose bridged text the edits change takes
# the bridge's text and what it reads as; the others keep the spec's.  The
# result is in step (its base the view now).  The files changed are the
# attribute `changed`.
.spec_sync <- function(p, meta) {
  sp <- p$spec
  body <- .planner_body(p)
  m <- .spec_meta(meta)
  if (!is.null(sp) && !is.null(sp$base) && identical(body, sp$base) && identical(m, sp$meta)) {
    attr(p, "changed") <- character()
    return(p)
  }
  now <- .spec_bridge(body, meta)
  on.exit(unlink(now$dir, recursive = TRUE), add = TRUE)
  if (is.null(sp) || is.null(sp$base)) {
    changed <- union(names(sp$text), names(now$text))
  } else {
    was <- .spec_bridge(sp$base, sp$meta)
    unlink(was$dir, recursive = TRUE)
    rels <- union(names(was$text), names(now$text))
    changed <- rels[!vapply(rels, function(r) identical(was$text[[r]], now$text[[r]]), NA)]
  }
  if (length(changed)) {
    # study.yml with the study's own lines: when it was created, when
    # updated, by which tflplanner
    sy <- file.path(now$dir, .study_file)
    if (.study_file %in% changed && file.exists(sy)) {
      more <- list(created = meta$created %||% format(Sys.Date()),
                   updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   tflplanner = as.character(utils::packageVersion("tflplanner")))
      lines <- now$text[[.study_file]]
      # (no blank line between tflspec's part and the study's)
      while (length(lines) && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
      writeLines(c(lines, sub("\n$", "", yaml::as.yaml(more))), sy, useBytes = TRUE)
      now$text[[.study_file]] <- readLines(sy, encoding = "UTF-8", warn = FALSE)
    }
    new <- .spec_load(now$dir)
    dir <- sp$study$dir
    if (is.null(sp)) sp <- list(study = NULL, outputs = list(), text = list(), where = character())
    if (is.null(sp$study)) sp$study <- new$study
    for (r in changed) {
      sp$text[r] <- list(now$text[[r]])
      part <- .spec_part(r)
      switch(part$kind,
        output = {
          gone <- names(sp$where)[sp$where == r]
          sp$outputs[gone] <- NULL
          sp$where <- sp$where[!names(sp$where) %in% gone]
          for (id in names(new$where)[new$where == r]) {
            sp$outputs[[id]] <- new$outputs[[id]]
            sp$where[[id]] <- r
          }
        },
        pattern = sp$study$patterns[part$name] <- list(new$study$patterns[[part$name]]),
        sp$study[part$kind] <- list(new$study[[part$kind]]))
    }
    sp$study$dir <- dir %||% sp$study$dir
    # the files and the reports in the files' order (as read from a folder)
    sp$text <- sp$text[sort(names(sp$text), method = "radix")]
    sp$where <- sp$where[order(sp$where, seq_along(sp$where), method = "radix")]
    sp$outputs <- sp$outputs[names(sp$where)]
  }
  p$spec <- sp
  p <- .spec_rebase(p, meta)
  attr(p, "changed") <- changed
  p
}

# ------------------------------------------------------------- writing

# The spec's files written into the study folder: each whose text differs
# from the file's (study.yml: but for the time it was updated); a file of
# the folder's spec the spec no longer has removed when it is the one the
# last save wrote (`own(rel)`).  The files, as save_study() lists them:
# file, status.
.spec_write <- function(p, root, own = function(rel) FALSE) {
  text <- p$spec$text
  files <- data.frame(file = character(), status = character(), stringsAsFactors = FALSE)
  for (r in names(text)) {
    to <- file.path(root, r)
    drop <- if (identical(r, .study_file)) function(x) x[!startsWith(x, "updated:")] else identity
    same <- file.exists(to) &&
      identical(drop(readLines(to, encoding = "UTF-8", warn = FALSE)), drop(text[[r]]))
    if (!same) {
      dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
      writeLines(text[[r]], to, useBytes = TRUE)
    }
    files[nrow(files) + 1L, ] <- list(to, if (same) "unchanged" else "written")
  }
  # a report's (or a pattern's) file no longer in the definition
  for (r in setdiff(.spec_files_yaml(root), c(names(text), .study_file))) {
    if (isTRUE(own(r))) {
      unlink(file.path(root, r))
      files[nrow(files) + 1L, ] <- list(file.path(root, r), "removed")
    }
  }
  files
}

# ------------------------------------------------------------- accessors
# (for the screens that will read and write the spec's items themselves)

# The spec with the view's edits in it (NULL: the planner has none)
.spec_now <- function(p, meta = p$spec$meta) {
  if (is.null(p$spec)) return(NULL)
  .spec_sync(p, meta)$spec
}

# The study's spec, and a report as its file says it
.spec_study <- function(p) .spec_now(p)$study
.spec_output <- function(p, output_id) .spec_now(p)$outputs[[output_id]]

# A report written straight into the spec (a tfl_output; NULL removes
# it): the view's edits taken in first, the report's text made by
# tflspec::tfl_write_output(), and its rows of the view made again from it
# -- in the view and in the base, so the bridge does not take it back
.spec_set_output <- function(p, output_id, output) {
  if (is.null(p$spec)) stop("The planner has no YAML spec.", call. = FALSE)
  p <- .spec_sync(p, p$spec$meta)
  attr(p, "changed") <- NULL
  sp <- p$spec
  rel <- sp$where[[output_id]] %||%
    file.path(study_layout()[["spec"]], "outputs", paste0(output_id, ".yml"))
  if (sum(sp$where == rel) > 1L) {
    stop("The report ", output_id, " shares its file ", rel, " with others.", call. = FALSE)
  }
  if (is.null(output)) {
    sp$outputs[[output_id]] <- NULL
    sp$where <- sp$where[names(sp$where) != output_id]
    sp$text[[rel]] <- NULL
  } else {
    if (!inherits(output, "tfl_output") || !identical(output$id, output_id)) {
      stop("A report is a tfl_output of the id ", output_id, ".", call. = FALSE)
    }
    f <- tempfile("tflplanner-output", fileext = ".yml")
    on.exit(unlink(f), add = TRUE)
    tflspec::tfl_write_output(output, f)
    sp$outputs[[output_id]] <- output
    sp$where[[output_id]] <- rel
    sp$text[[rel]] <- readLines(f, encoding = "UTF-8", warn = FALSE)
  }
  p$spec <- sp
  .spec_view_output(p, output_id)
}

# One report's rows of the view (and of the base) made again from the spec
.spec_view_output <- function(p, output_id) {
  sp <- p$spec
  v <- .planner_from_spec(sp$study, sp$outputs[intersect(output_id, names(sp$outputs))])
  put <- function(x) {
    nc <- attr(x, "not_converted")
    no_id <- function(d) { d$output_id <- NULL; d }
    for (s in names(x$sheets)) {
      x <- set_sheet_rows(x, s, output_id, no_id(sheet_rows(v, s, output_id)))
    }
    for (s in c("analysis_data", "analyses")) {
      d <- v$ard[[s]]
      x <- set_ard_rows(x, s, output_id,
                        no_id(d[!is.na(d$output_id) & d$output_id == output_id, , drop = FALSE]))
    }
    for (s in names(x$lf %||% .empty_lf())) {
      x <- set_lf_rows(x, s, output_id, no_id(lf_rows(v, s, output_id)))
    }
    x <- set_fig_design(x, output_id, fig_design(v, output_id))
    row <- v$outputs[v$outputs$output_id == output_id, , drop = FALSE]
    i <- match(output_id, x$outputs$output_id)
    if (!nrow(row)) {
      if (!is.na(i)) x$outputs <- x$outputs[-i, , drop = FALSE]
    } else if (is.na(i)) {
      x$outputs <- rbind(x$outputs, row[names(x$outputs)])
    } else {
      x$outputs[i, ] <- row[names(x$outputs)]
    }
    rownames(x$outputs) <- NULL
    attr(x, "not_converted") <- nc
    x
  }
  base <- sp$base
  p <- put(p)
  if (!is.null(base)) p$spec$base <- put(base)
  p
}
