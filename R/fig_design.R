# Figure designs: the Plot Designer's figures.
#
# A figure can be designed instead of written by hand, as tflspec's figure
# design: one ordered list of data steps from ADaM (read, join, keep a
# PARAMCD or a population, derive ...; the steps that make an object of
# their own: a KM fit, summary statistics ...; or code), the figure-wide
# settings and the layers, in order (#293).  A template fills them at once;
# each is then edited on its own.
# The design is kept with the study (its state) and written as
# spec/figures/<output_id>.yml, one file a figure, to read and to diff.  The
# figure's program then has the design's code as its plot part, in place of
# hand-written code; the pieces a design can have are tflspec's
# tfl_fig_parts().

.fig_design_dir <- "figures"

#' A figure's design (the Plot Designer)
#'
#' `fig_design()` gives a figure's design, `NULL` when it has none (its plot
#' is written by hand); `set_fig_design()` sets it, or with `NULL` drops it.
#' A design is tflspec's [tflspec::tfl_fig_design()] (see also
#' [tflspec::tfl_fig_template()]).  With a design, the figure's program
#' makes its plot from it ([program_code()]).
#'
#' @param x An `tflplanner`.
#' @param output_id The figure.
#' @param design A `tfl_fig_design`, or `NULL`.
#' @return `fig_design()`: a `tfl_fig_design` or `NULL`; `set_fig_design()`:
#'   `x`.
#' @export
fig_design <- function(x, output_id) {
  d <- (x$fig_designs %||% list())[[output_id]]
  if (is.null(d)) return(NULL)
  .fig_rebuild(d)
}

# a design as tflspec makes it: every part the design has that this
# tflspec takes (composed figures, the ggplot2 version ... are kept, not
# dropped); a design of before #293 (its `stats` apart) as one data list
.fig_rebuild <- function(d) {
  args <- c(list(data = d$data %||% list(), stats = d$stats %||% list(),
                 plot = d$plot %||% list(), layers = d$layers %||% list()),
            d[setdiff(names(d), c("data", "stats", "plot", "layers"))])
  args <- args[names(args) %in% names(formals(tflspec::tfl_fig_design))]
  do.call(tflspec::tfl_fig_design, args)
}

#' @rdname fig_design
#' @export
set_fig_design <- function(x, output_id, design) {
  if (is.null(x$fig_designs)) x$fig_designs <- list()
  if (is.null(design)) {
    x$fig_designs[[output_id]] <- NULL
  } else {
    # (a list of before #293, its `stats` apart: as one data list)
    if (length(design$stats)) design <- .fig_rebuild(unclass(design))
    x$fig_designs[[output_id]] <- .fig_norm(unclass(design))
  }
  x
}

# a design as kept: nothing unset, numbers as doubles, lists of single
# values as vectors -- the same, whether made here or read back from JSON
# or YAML
.fig_norm <- function(x) {
  if (is.list(x)) {
    x <- x[!vapply(x, function(v) is.null(v) || (is.atomic(v) && length(v) == 1L &&
                                                 (is.na(v) || identical(v, ""))), logical(1))]
    x <- lapply(x, .fig_norm)
    if (!is.null(names(x)) || !length(x)) return(x)
    scalar <- vapply(x, function(v) is.atomic(v) && length(v) == 1L, logical(1))
    if (all(scalar)) return(unlist(x))
    return(x)
  }
  if (is.integer(x)) as.numeric(x) else x
}

# the datasets a design's script reads (tflspec says: its `reads`)
.fig_design_datasets <- function(design, output_id = "fig") {
  if (is.null(design)) return(character())
  code <- tflspec::tfl_fig_design_code(design, output_id, setup = TRUE, save = FALSE)
  unique(toupper(attr(code, "reads") %||% character()))
}

# the design's script, whole (it saves its PNG to `fig_path`)
.fig_design_script <- function(design, output_id, codelists = NULL) {
  .split_lines(as.character(tflspec::tfl_fig_design_code(
    design, output_id, codelists = codelists)))
}

# A designed figure's data and plot sections, as its program has them
# (#293): `# ---- data ----` -- the code lists, the datasets read (the
# data catalog), the steps' pipes -- and `# ---- plot ----` -- the palette,
# the chain into `plot` (the report writes it) -- then the figure checks.
# The study's figure setup (sourced at the program's top) attaches the
# packages and gives the palette.
.fig_design_plot <- function(design, output_id, codelists, x) {
  code <- tflspec::tfl_fig_design_code(
    design, output_id, setup = TRUE, save = FALSE, name = "plot",
    codelists = codelists)
  reads <- attr(code, "reads") %||% character()
  lines <- .split_lines(as.character(code))
  read <- if (length(reads)) c(
    paste0("# the data: ", paste(reads, collapse = ", "), " (data catalog)"),
    unlist(lapply(reads, function(d) .read_dataset_lines(x, d))),
    "")
  # after the section's line and the code lists (when there are)
  at <- 1L
  if (length(lines) > 1L && startsWith(lines[[2L]], "# ---- code lists")) {
    blank <- which(lines == "")
    at <- blank[blank > 2L][1L]
  }
  c(utils::head(lines, at), .fig_ard_lines(x, output_id, design), read, utils::tail(lines, -at),
    "", "# the figure checks: dropped rows, colours against the standard",
    "tfl_check(plot)")
}

# one element a line; a blank line stays (strsplit would drop it)
.split_lines <- function(code) {
  unlist(lapply(code, function(l) {
    if (!nzchar(l)) "" else strsplit(l, "\n", fixed = TRUE)[[1L]]
  }))
}

# the designs as YAML files, written with the study
# `own`: whether a file is the one tflplanner wrote (its fingerprint the
# recorded one); only such a file goes when its figure has no design any
# more -- one made or changed outside tflplanner is kept (#274)
.save_fig_designs <- function(p, root, own = function(f) FALSE) {
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  designs <- p$fig_designs %||% list()
  dir <- file.path(root, study_layout()[["spec"]], .fig_design_dir)
  have <- if (dir.exists(dir)) list.files(dir, "\\.yml$", full.names = TRUE)
  if (!length(designs) && !length(have)) return(out)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (id in names(designs)) {
    f <- file.path(dir, paste0(id, ".yml"))
    tmp <- tempfile(fileext = ".yml")
    tflspec::tfl_write_fig_design(fig_design(p, id), tmp)
    new <- readLines(tmp, encoding = "UTF-8")
    old <- if (file.exists(f)) readLines(f, encoding = "UTF-8")
    if (identical(old, new)) {
      out[nrow(out) + 1L, ] <- list(f, "unchanged")
    } else {
      file.copy(tmp, f, overwrite = TRUE)
      out[nrow(out) + 1L, ] <- list(f, "written")
    }
    unlink(tmp)
  }
  # a figure no longer designed (or gone): its file goes too, when it is
  # the one tflplanner wrote
  gone <- setdiff(have, file.path(dir, paste0(names(designs), ".yml")))
  for (f in gone) {
    if (own(f)) {
      unlink(f)
      out[nrow(out) + 1L, ] <- list(f, "removed")
    } else {
      out[nrow(out) + 1L, ] <- list(f, "kept")
    }
  }
  out
}

# the designs from the study's state (JSON), as fig_designs
.fig_designs_from_state <- function(x) {
  if (!length(x)) return(list())
  # one data list (a state of before #293 kept `stats` apart)
  lapply(x, function(d) {
    d <- .fig_norm(d)
    if (length(d$stats) || any(vapply(d$data %||% list(), function(s)
      identical(s$step, "data_code") || identical(s$step, "stats_code"), NA))) {
      d <- .fig_norm(unclass(.fig_rebuild(d)))
    }
    d
  })
}

# the study's data a design reads, read as its programs read it
.study_data <- function(study, datasets) {
  x <- study$planner
  owd <- setwd(study$path)
  on.exit(setwd(owd), add = TRUE)
  e <- new.env(parent = globalenv())
  out <- list()
  for (d in datasets) {
    code <- .read_dataset_lines(x, d)
    # a dataset that cannot be read (no file yet) is left out -- the
    # design's check says so; readRDS() warns before it fails, quietly here
    r <- tryCatch(suppressWarnings({
      eval(parse(text = code, encoding = "UTF-8"), envir = e)
      get(make.names(tolower(d)), envir = e)
    }), error = function(err) NULL)
    if (is.data.frame(r)) out[[toupper(d)]] <- r
  }
  out
}

#' Draw a figure from its design
#'
#' Runs a design's script on the study's data, as the figure's program will
#' (in a temporary folder, so nothing of the study changes), and gives the
#' PNG it saves -- at the figure's own size -- with the design's problems
#' and the figure checks.  The Plot Designer's preview.
#'
#' @param study An `rtfstudy`.
#' @param output_id The figure.
#' @param design The design (default: the figure's).
#' @param max_px For the screen: at most this many pixels wide (the PNG is
#'   drawn at a lower dpi, same size); `NULL` draws it as saved.
#' @return A list: `png` (the file, `NULL` when it failed), `size` (its
#'   `width`, `height`, `units` and `dpi`), `advice` (what is usually
#'   wanted and missing, see [tflspec::tfl_fig_advice()]), `problems` (the
#'   design against the schema and the data, see
#'   [tflspec::tfl_check_fig_design()]), `warnings` (the figure checks and
#'   the plot's own warnings), `error` (`NULL` or the message), `code`.
#' @export
preview_figure <- function(study, output_id,
                           design = fig_design(study$planner, output_id),
                           max_px = NULL) {
  if (is.null(design)) stop("The figure has no design.", call. = FALSE)
  saved <- design
  # a preview for the screen: the saved size, fewer dots (much faster);
  # the size reported is still the saved one
  if (!is.null(max_px)) {
    w <- as.numeric(design$plot$width %||% 7.5)
    u <- design$plot$units %||% "in"
    inches <- switch(u, cm = w / 2.54, px = w / 300, w)
    dpi <- as.numeric(design$plot$dpi %||% 300)
    if (inches * dpi > max_px) design$plot$dpi <- max(72, floor(max_px / inches))
  }
  code <- .fig_design_script(design, output_id,
                             codelists = .study_codelists(study$planner))
  ds <- .fig_design_datasets(design, output_id)
  # ADSL too: the advice counts the groups, which may be joined from it
  data <- .study_data(study, union(ds, "ADSL"))
  problems <- tflspec::tfl_check_fig_design(saved, data)
  # its ARD (#293): the rows of its source, in `ard`; their problems
  reads_ard <- isTRUE(attr(tflspec::tfl_fig_design_code(saved, output_id, setup = TRUE,
                                                         save = FALSE), "ard"))
  ard <- if (reads_ard) .fig_ard_rows(study, output_id)
  if (reads_ard) {
    ap <- .fig_ard_problems(study, output_id, saved)
    if (nrow(ap)) problems <- rbind(problems, ap[c("part", "field", "problem")])
  }
  advice <- tryCatch(tflspec::tfl_fig_advice(saved, data), error = function(e) NULL)
  tmp <- tempfile("tflplanner-fig-")
  dir.create(tmp)
  owd <- setwd(tmp)
  on.exit(setwd(owd), add = TRUE)
  e <- new.env(parent = .helpers_env(globalenv()))
  for (d in names(data)) assign(make.names(tolower(d)), data[[d]], envir = e)
  if (!is.null(ard)) assign("ard", ard, envir = e)
  warns <- character()
  err <- NULL
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  on.exit(if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev),
          add = TRUE)
  withCallingHandlers(
    tryCatch(suppressMessages(
      eval(parse(text = code, encoding = "UTF-8"), envir = e)),
      error = function(err0) err <<- conditionMessage(err0)),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  png <- if (is.null(err) && !is.null(e$fig_path) &&
             file.exists(file.path(tmp, e$fig_path))) {
    normalizePath(file.path(tmp, e$fig_path), winslash = "/")
  }
  if (is.null(err) && !is.null(e$fig)) {
    chk <- tryCatch({
      w <- character()
      withCallingHandlers(tflspec::tfl_check_fig(e$fig, quiet = TRUE),
                          warning = function(w0) {
                            w <<- c(w, conditionMessage(w0))
                            invokeRestart("muffleWarning")
                          })
      w
    }, error = function(err0) character())
    warns <- c(warns, chk)
  }
  warns <- unique(warns[!grepl("was built under R version", warns)])
  size <- if (!is.null(e$fig_width)) {
    list(width = e$fig_width, height = e$fig_height, units = e$fig_units,
         dpi = as.numeric(saved$plot$dpi %||% 300))
  }
  list(png = png, size = size, problems = problems, advice = advice, warnings = warns,
       error = err, code = code)
}

#' The same figure for other parameters
#'
#' Copies a designed figure once per parameter: each copy is a new figure
#' report ([copy_output()]: layout and design) whose design keeps every
#' piece but the parameter (its `param` step), and whose description says
#' the new parameter where it said the old one.
#'
#' @param x An `tflplanner`.
#' @param output_id The designed figure.
#' @param params The PARAMCDs, one figure each.
#' @param pattern The new IDs: `{id}` = `output_id`, `{param}` = the
#'   parameter.
#' @return `x` with the new figures.
#' @export
copy_fig_to_params <- function(x, output_id, params, pattern = "{id}-{param}") {
  d <- fig_design(x, output_id)
  if (is.null(d)) stop("The figure has no design.", call. = FALSE)
  at <- which(vapply(d$data, function(s) identical(s$step, "param"), logical(1)))
  if (!length(at)) stop("The design has no 'Keep a parameter' step to change.", call. = FALSE)
  old <- trimws(strsplit(paste(d$data[[at[1L]]]$value, collapse = ","), "[|,]")[[1L]])[1L]
  for (prm in params) {
    new_id <- gsub("{param}", prm, gsub("{id}", output_id, pattern, fixed = TRUE), fixed = TRUE)
    x <- copy_output(x, output_id, new_id)
    d2 <- d
    for (i in at) d2$data[[i]]$value <- prm
    x <- set_fig_design(x, new_id, d2)
    o <- x$outputs$output_id == new_id
    if (!is.na(old) && nzchar(old) && any(o) && !is.na(x$outputs$description[o])) {
      x$outputs$description[o] <- gsub(old, prm, x$outputs$description[o], fixed = TRUE)
    }
  }
  x
}

# ---- presets: designs kept with the company standards ----------------------

.fig_preset_dir <- function(home = tflplanner_home()) {
  file.path(home, "standards", "figure-presets")
}

#' Figure presets
#'
#' A preset is a figure design kept with the company standards, to start a
#' figure from as a template is: a KM figure as the company draws it, the
#' mean-over-time figure of a study type ...  `fig_presets()` lists them;
#' `save_fig_preset()` keeps a design as one (a `.yml` in the home's
#' `standards/figure-presets`); `read_fig_preset()` gives it back;
#' `remove_fig_preset()` drops it.
#'
#' @param design A `tfl_fig_design`.
#' @param name The preset's name (its file's).
#' @param description A line on what it is.
#' @param home tflplanner's home.
#' @return `fig_presets()`: a data frame (`name`, `description`, `template`,
#'   `file`); `read_fig_preset()`: a `tfl_fig_design`; the others: the
#'   file, invisibly.
#' @export
fig_presets <- function(home = tflplanner_home()) {
  dir <- .fig_preset_dir(home)
  files <- if (dir.exists(dir)) list.files(dir, "\\.yml$", full.names = TRUE) else character()
  rows <- lapply(files, function(f) {
    x <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (is.null(x)) return(NULL)
    data.frame(name = x$name %||% tools::file_path_sans_ext(basename(f)),
               description = x$description %||% "", template = x$template %||% "",
               file = f, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(rows, list(data.frame(
    name = character(), description = character(), template = character(),
    file = character(), stringsAsFactors = FALSE))))
  rownames(out) <- NULL
  out
}

#' @rdname fig_presets
#' @export
save_fig_preset <- function(design, name, description = "",
                            home = tflplanner_home()) {
  name <- trimws(name)
  if (!nzchar(name) || grepl("[\\\\/:*?\"<>|]", name)) {
    stop("A preset's name is its file's: letters, digits, spaces, - and _.",
         call. = FALSE)
  }
  dir <- .fig_preset_dir(home)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, paste0(name, ".yml"))
  x <- c(list(name = name, description = description), .fig_norm(unclass(design)))
  x <- x[!vapply(x, function(v) is.null(v) || !length(v), logical(1))]
  writeLines(enc2utf8(yaml::as.yaml(x)), f, useBytes = TRUE)
  invisible(f)
}

#' @rdname fig_presets
#' @export
read_fig_preset <- function(name, home = tflplanner_home()) {
  f <- file.path(.fig_preset_dir(home), paste0(name, ".yml"))
  if (!file.exists(f)) stop("No preset '", name, "'.", call. = FALSE)
  tflspec::tfl_read_fig_design(f)
}

#' @rdname fig_presets
#' @export
remove_fig_preset <- function(name, home = tflplanner_home()) {
  f <- file.path(.fig_preset_dir(home), paste0(name, ".yml"))
  if (file.exists(f)) unlink(f)
  invisible(f)
}

# A template's own defaults (its parameter OS, its flag FASFL, its group
# TRT01P) may not be in the study's data.  For each the user left blank,
# the data's own when the default is not there: the first parameter of
# the dataset, a population flag it has, a treatment variable it has.
# `x` is the dataset, `adsl` ADSL (where flags and the group may be);
# `given` the names the user chose (param, pop, group).  Returns the
# arguments to give the template instead, named (empty when none).
.template_fit <- function(d, x, adsl = NULL, given = character()) {
  cols <- unique(c(names(x), names(adsl)))
  out <- character()
  step_of <- function(s) Filter(function(z) identical(z$step, s), d$data)
  pick <- function(cands) {
    hit <- cands[cands %in% cols]
    if (length(hit)) hit[[1L]] else NA_character_
  }
  if (!"param" %in% given && "PARAMCD" %in% names(x)) {
    p <- step_of("param")
    have <- unique(stats::na.omit(as.character(x$PARAMCD)))
    v <- if (length(p)) unlist(strsplit(paste(p[[1L]]$value, collapse = ","),
                                        "\\s*[|,]\\s*")) else character()
    if (length(v) && !any(v %in% have) && length(have)) out[["param"]] <- sort(have)[1L]
  }
  # a variable the template reads: kept when the dataset has it; named
  # when only ADSL has it (so the caller joins ADSL); else the first of
  # `cands` the data have
  fit_var <- function(v, cands) {
    if (!length(v) || is.na(v) || v %in% names(x)) return(NA_character_)
    if (v %in% names(adsl)) return(v)
    # the dataset's own first (no join), then ADSL's
    hit <- c(cands[cands %in% names(x)], cands[cands %in% names(adsl)])
    if (length(hit)) hit[[1L]] else NA_character_
  }
  if (!"pop" %in% given) {
    f <- step_of("flag")
    v <- fit_var(if (length(f)) f[[1L]]$variable,
                 c("FASFL", "ITTFL", "SAFFL", "EFFFL", "RANDFL",
                   sort(grep("FL$", cols, value = TRUE))))
    if (!is.na(v)) out[["pop"]] <- v
  }
  if (!"group" %in% given) {
    g <- d$plot$colour_by %||% unlist(lapply(d$data, `[[`, "by"))[1L]
    v <- fit_var(g, c("TRT01P", "TRT01A", "TRTP", "TRTA", "ARM", "ACTARM"))
    if (!is.na(v)) out[["group"]] <- v
  }
  out
}

# A template joins ADSL for the variables the dataset lacks, but it names
# them all (the group and the flag): one the dataset has already would come
# back twice (SAFFL.x, SAFFL.y) and be found by neither name.  Joins only
# what `x` has not; a join left with nothing is dropped.
.trim_join <- function(d, x) {
  keep <- vapply(seq_along(d$data), function(i) {
    s <- d$data[[i]]
    if (!identical(s$step, "join") || is.null(s$vars)) return(TRUE)
    v <- trimws(strsplit(paste(s$vars, collapse = ","), ",")[[1L]])
    v <- v[nzchar(v) & !v %in% names(x)]
    if (!length(v)) return(FALSE)
    d$data[[i]]$vars <<- paste(v, collapse = ", ")
    TRUE
  }, logical(1))
  d$data <- d$data[keep]
  d
}
