# Figure designs: the Plot Designer's figures.
#
# A figure can be designed instead of written by hand: its type (km,
# waterfall, forest ...), its style and the arguments of tflspec's
# tfl_fig_<type>() -- which dataset and PARAMCD, which variables, the axes,
# the legend, the size.  The design is kept with the study (its state) and
# written as spec/figures/<output_id>.yml, one file a figure, to read and to
# diff.  The figure's program then has the design's code as its plot part,
# in place of hand-written code; what a design can be is tflspec's
# tfl_fig_schema().

.fig_design_dir <- "figures"

#' A figure's design (the Plot Designer)
#'
#' `fig_design()` gives a figure's design, `NULL` when it has none (its plot
#' is written by hand); `set_fig_design()` sets it, or with `NULL` drops it.
#' A design is tflspec's [tflspec::tfl_fig_design()]: the figure type, its
#' style and the arguments of its `tfl_fig_<type>()`.  With a design, the
#' figure's program makes its plot from it ([program_code()]).
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
  tflspec::tfl_fig_design(d$type, d$style, d$args %||% list())
}

#' @rdname fig_design
#' @export
set_fig_design <- function(x, output_id, design) {
  if (is.null(x$fig_designs)) x$fig_designs <- list()
  if (is.null(design)) {
    x$fig_designs[[output_id]] <- NULL
  } else {
    d <- unclass(design)
    d$args <- .fig_args_norm(d$args)
    x$fig_designs[[output_id]] <- d
  }
  x
}

# the arguments as kept: none unset, numbers as doubles (7, read back from
# JSON or YAML as an integer, is the same 7)
.fig_args_norm <- function(args) {
  args <- args[!vapply(args, is.null, logical(1))]
  lapply(args, function(v) if (is.integer(v)) as.numeric(v) else v)
}

# the datasets a design's script reads (its "Input data frames" line)
.fig_design_datasets <- function(design, output_id = "fig") {
  if (is.null(design)) return(character())
  code <- .fig_design_script(design, output_id)
  l <- grep("^# Input data frames:", code, value = TRUE)
  if (!length(l)) return(character())
  ds <- trimws(strsplit(sub("^# Input data frames:", "", l[1L]), ",")[[1L]])
  unique(toupper(ds[nzchar(ds)]))
}

# the design's script, whole (it saves its PNG to `fig_path`)
.fig_design_script <- function(design, output_id) {
  args <- design$args
  args$plot_id <- output_id
  d <- tflspec::tfl_fig_design(design$type, design$style, args)
  unlist(strsplit(as.character(tflspec::tfl_fig_design_code(d)), "
",
                  fixed = TRUE))
}

# the design's plot part of the figure's program: the script up to its
# figure, which the program keeps as `plot` (the report saves it)
.fig_design_plot <- function(design, output_id) {
  code <- .fig_design_script(design, output_id)
  step3 <- grep("^# Step3", code)
  if (length(step3)) code <- code[seq_len(max(0L, step3[1L] - 2L))]
  while (length(code) && code[length(code)] %in% c("", "fig")) {
    code <- code[-length(code)]
  }
  c(paste0("# the plot, from the figure's design (spec/", .fig_design_dir, "/",
           output_id, ".yml): ", design$type, " / ", design$style),
    "#      edit the design in the Plot Designer, not this code", code,
    "plot <- fig")
}

# the designs as YAML files, written with the study
.save_fig_designs <- function(p, root) {
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
  # a figure no longer designed (or gone): its file goes too
  gone <- setdiff(have, file.path(dir, paste0(names(designs), ".yml")))
  for (f in gone) {
    unlink(f)
    out[nrow(out) + 1L, ] <- list(f, "removed")
  }
  out
}

# the designs from the study's state (JSON), as fig_designs
.fig_designs_from_state <- function(x) {
  if (!length(x)) return(list())
  flat <- function(v) {
    if (is.list(v) && !is.null(names(v)) && all(nzchar(names(v)))) {
      unlist(lapply(v, function(e) if (is.null(e)) NA else e))
    } else if (is.list(v)) {
      unlist(lapply(v, function(e) if (is.null(e)) NA else e))
    } else v
  }
  lapply(x, function(d) list(type = as.character(d$type),
                             style = as.character(d$style),
                             args = .fig_args_norm(lapply(d$args %||% list(),
                                                          flat))))
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
    r <- tryCatch({
      eval(parse(text = code, encoding = "UTF-8"), envir = e)
      get(make.names(tolower(d)), envir = e)
    }, error = function(err) NULL)
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
#' @return A list: `png` (the file, `NULL` when it failed), `size` (its
#'   `width`, `height`, `units` and `dpi`), `problems` (the
#'   design against the schema and the data, see
#'   [tflspec::tfl_check_fig_design()]), `warnings` (the figure checks and
#'   the plot's own warnings), `error` (`NULL` or the message), `code`.
#' @export
preview_figure <- function(study, output_id,
                           design = fig_design(study$planner, output_id)) {
  if (is.null(design)) stop("The figure has no design.", call. = FALSE)
  ds <- .fig_design_datasets(design)
  data <- .study_data(study, ds)
  problems <- tflspec::tfl_check_fig_design(design, data)
  code <- .fig_design_script(design, output_id)
  tmp <- tempfile("tflplanner-fig-")
  dir.create(tmp)
  owd <- setwd(tmp)
  on.exit(setwd(owd), add = TRUE)
  e <- new.env(parent = globalenv())
  for (d in names(data)) assign(make.names(tolower(d)), data[[d]], envir = e)
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
         dpi = e$fig_dpi)
  }
  list(png = png, size = size, problems = problems, warnings = warns,
       error = err, code = code)
}
