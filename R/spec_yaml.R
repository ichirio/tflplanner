# The study definition as tflspec's YAML spec (2026-10-15): study.yml and
# spec/ (data.yml, defaults.yml, toc.yml,
# outputs/<id>.yml), one file a report.  The planner holds the spec itself
# and the sheets every screen edits are a view of it (spec_model.R, #407);
# the view's edits go back through tflspec's tfl_yaml_from_sheets().  A study whose
# spec/ still holds the workbooks is of the sheets' format: it opens as
# before until it is converted.

# Whether a study folder has a YAML spec: spec/outputs/, or spec/toc.yml
# (a study saved with no report yet has no outputs folder)
.has_yaml_spec <- function(root) {
  sp <- file.path(root, study_layout()[["spec"]])
  dir.exists(file.path(sp, "outputs")) || file.exists(file.path(sp, "toc.yml"))
}

# The format of a study folder's definition: "yaml" (.has_yaml_spec()),
# "sheets" (the workbooks, the ARD's JSON) or "none" (nothing yet)
.study_format <- function(root) {
  sp <- file.path(root, study_layout()[["spec"]])
  if (.has_yaml_spec(root)) return("yaml")
  old <- c(.table_file, .report_file, .ard_json, .ard_file, .lf_file)
  if (any(file.exists(file.path(sp, old)))) "sheets" else "none"
}

# The YAML spec's files, relative to the study folder (the converter's list
# of what it could not convert and the backups are not the study's)
.spec_files_yaml <- function(root) {
  sp <- study_layout()[["spec"]]
  f <- c(.study_file, file.path(sp, c("data.yml", "defaults.yml", "toc.yml")))
  dd <- file.path(root, sp, "outputs")
  if (dir.exists(dd)) {
    f <- c(f, file.path(sp, "outputs", sort(list.files(dd, "\\.ya?ml$", recursive = TRUE))))
  }
  f[file.exists(file.path(root, f))]
}

# The planner read from a study folder's YAML spec: its view of the spec
# (.planner_from_spec()), the spec in `$spec` (.spec_load(); in step with
# the view once .spec_rebase() is given the study's fields).  What the YAML
# holds that the sheets cannot is the attribute `not_converted`.
.planner_from_yaml <- function(root) {
  sp <- .spec_load(root)
  if (is.null(sp)) {
    # (a spec with no reports' folder: what tflspec reads of it)
    return(.planner_from_spec(tflspec::tfl_read_study_spec(root), tflspec::tfl_read_outputs(root)))
  }
  p <- .planner_from_spec(sp$study, sp$outputs)
  p$spec <- sp
  p
}

# What tflspec's tfl_yaml_from_sheets() writes the YAML spec from: the
# planner's sheets as one definition (with the study keys), the report list
# and setup (the _tflplanner sheet's rows), the ARD definition, the listing
# and figure sheets, the figure designs, the study's fields, and the study's
# code lists (`codelists`, study$codelists: the study_codelists sheet, which
# a report's `list` cell names; NULL -- a planner with no spec -- none given,
# and tflspec reads the defaults.yml it writes over)
.planner_sheets <- function(p, meta, style = p$spec$study$study$settings$style,
                            codelists = if (!is.null(p$spec)) p$spec$study$codelists %||% list()) {
  o <- p$outputs[names(.empty_outputs())]
  o$setup_code <- rep(NA_character_, nrow(o))
  planner <- rbind(
    data.frame(output_id = NA_character_, description = "(every report)",
               setup_code = p$setup, process_code = NA_character_,
               section = NA_character_, population = NA_character_,
               datasets = NA_character_, batches = NA_character_,
               stringsAsFactors = FALSE),
    o)
  study <- .bridge_fields(meta)
  # the study's default page configuration (study.yml settings: style) travels
  # with the sheets: the bridge names the blank rows' configuration by it
  # (else `standard`, and an edit of those rows would go to a set no report uses;
  # `standard` itself is the bridge's own name, left out)
  if (is.character(style) && length(style) == 1L && !is.na(style) && nzchar(style) &&
      !identical(style, "standard")) {
    study$settings <- list(style = style)
  }
  out <- list(table = unclass(.spec_object(p, c(table_sheets(), report_sheets()),
                                           unique(unlist(.study_keys)))),
              planner = planner, ard = p$ard, listing = p$lf,
              figures = p$fig_designs %||% list(), study = study)
  if (!is.null(codelists)) out$study_codelists <- .study_codelists_sheet(codelists)
  out
}

# The study's code lists (study$codelists: by name, each its codes, named by
# them when it has labels) as tflspec's study_codelists sheet: a code a row
# -- the list's name, the code, its label (blank: the list has none), its
# place -- as tfl_sheets_from_spec() writes it
.study_codelists_sheet <- function(cl) {
  rows <- lapply(names(cl), function(nm) {
    x <- cl[[nm]]
    codes <- as.character(names(x) %||% unname(x))
    lab <- if (is.null(names(x))) rep(NA_character_, length(x)) else as.character(unname(x))
    data.frame(name = rep(nm, length(codes)), value = codes, label = lab,
               order = as.character(seq_along(codes)), stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(data.frame(name = character(), value = character(), label = character(),
                                   order = character(), stringsAsFactors = FALSE)), rows))
}

# study.yml's top-level keys the save writes: the study's fields, when it
# was created and updated and by which tflplanner (.spec_sync_run()), and
# what tflspec's tfl_yaml_from_sheets() writes from the sheets
# (spec_version, packages, settings, keys, setup, tokens).  Always written
# from the planner, never kept from the file before: a key the writer
# leaves out (it is empty) is gone.
.study_yml_writer_keys <- function() {
  c(.study_fields, "created", "updated", "tflplanner", "spec_version",
    "packages", "settings", "keys", "setup", "tokens")
}

# The keys of study.yml's `settings:` the save writes: the study sheets'
# (.study_keys), the report sheet's study row (file, program), the study's
# default page configuration (style) and the app's own
# (.study_own_settings).  The same: never kept from the file before.
.study_yml_writer_settings <- function() {
  c(unlist(.study_keys, use.names = FALSE), "file", "program", "style",
    .study_own_settings)
}

# A YAML text's keys at `indent` spaces, each with its lines, by key: from
# the comment lines just above the key to the line before the next key
# (the comment lines just above the next key belong to that one).
.yaml_key_chunks <- function(lines, indent = 0L) {
  lines <- as.character(lines)
  body <- substring(lines, indent + 1L)
  at <- which(startsWith(lines, strrep(" ", indent)) & grepl("^[^[:space:]#-][^:]*:", body))
  if (!length(at)) return(list())
  keys <- gsub("^[\"']|[\"']$", "", sub(":.*$", "", body[at]))
  is_comment <- grepl("^[[:space:]]*#", lines)
  out <- lapply(seq_along(at), function(n) {
    i <- at[n]
    j <- if (n < length(at)) at[n + 1L] - 1L else length(lines)
    while (j > i && is_comment[j]) j <- j - 1L
    from <- i
    while (from > 1L && is_comment[from - 1L]) from <- from - 1L
    lines[from:j]
  })
  stats::setNames(out, keys)
}

# A `settings:` key's lines (.yaml_key_chunks()) as the keys of its map, by
# key, each at 2 spaces as the writer has them.  A flow map
# (`settings: {a: 1}`): its keys as the YAML package writes them.
.study_yml_settings_chunks <- function(chunk) {
  if (is.null(chunk)) return(list())
  k <- grep("^settings:", chunk)[1L]
  if (is.na(k)) return(list())
  rest <- chunk[-seq_len(k)]
  if (!grepl("^settings:[[:space:]]*(#.*)?$", chunk[k])) {
    v <- tryCatch(yaml::yaml.load(paste(chunk[k:length(chunk)], collapse = "\n"))$settings,
                  error = function(e) NULL)
    if (!is.list(v) || is.null(names(v))) return(list())
    return(stats::setNames(lapply(names(v), function(n) {
      paste0("  ", strsplit(sub("\n$", "", yaml::as.yaml(v[n])), "\n", fixed = TRUE)[[1L]])
    }), names(v)))
  }
  vals <- rest[nzchar(trimws(rest)) & !grepl("^[[:space:]]*#", rest)]
  if (!length(vals)) return(list())
  ind <- nchar(sub("^( *).*$", "\\1", vals[1L]))
  out <- .yaml_key_chunks(rest, ind)
  if (ind != 2L) {
    pad <- strrep(" ", ind)
    out <- lapply(out, function(x) ifelse(startsWith(x, pad), paste0("  ", substring(x, ind + 1L)), x))
  }
  out
}

# study.yml's text as the save writes it (`lines`), with what of `old` (the
# file's text before) the writer does not write kept as it was:
# - every top-level key that is not the writer's (.study_yml_writer_keys())
#   and not in `lines` -- the AI screens' `ai:`, a key tflspec or the AI kit
#   writes, a block written by hand -- its lines and the comment lines just
#   above it, in their old order, at the end;
# - every key of the old `settings:` that is not the writer's
#   (.study_yml_writer_settings()) and not in the new `settings:` -- a key
#   no sheet holds -- at the end of the new `settings:` (a `settings:` of
#   its own when `lines` has none).
# A key the writer writes is the writer's line, never the old one.
# list(lines, kept = list(top, settings)): the text and the keys kept.
.keep_study_yml_keys <- function(lines, old) {
  lines <- as.character(lines)
  new_top <- .yaml_key_chunks(lines)
  old_top <- .yaml_key_chunks(old)
  top <- as.character(setdiff(names(old_top), c(names(new_top), .study_yml_writer_keys())))
  old_set <- .study_yml_settings_chunks(old_top[["settings"]])
  new_set <- .study_yml_settings_chunks(new_top[["settings"]])
  set <- as.character(setdiff(names(old_set), c(names(new_set), .study_yml_writer_settings())))
  if (length(set)) {
    add <- unlist(old_set[set], use.names = FALSE)
    at <- grep("^settings:[[:space:]]*(#.*)?$", lines)[1L]
    if (!is.na(at)) {
      end <- at
      while (end < length(lines) && grepl("^[ ]", lines[end + 1L])) end <- end + 1L
      lines <- append(lines, add, after = end)
    } else if (is.null(new_top[["settings"]])) {
      lines <- c(lines, "settings:", add)
    } else {
      # (a flow map the writer wrote: as it is)
      set <- character()
    }
  }
  if (length(top)) lines <- c(lines, unlist(old_top[top], use.names = FALSE))
  list(lines = lines, kept = list(top = top, settings = set))
}

# A study's study.yml as read (`mine`, as the writer wrote it) with the
# keys .keep_study_yml_keys() kept (`kept`) as `old` (the one before) has
# them
.keep_study_yml_values <- function(mine, old, kept) {
  if (!is.list(old)) return(mine)
  for (k in intersect(kept$top, names(old))) mine[k] <- list(old[[k]])
  ks <- intersect(kept$settings, names(old$settings))
  if (length(ks)) {
    st <- if (is.list(mine$settings)) mine$settings else list()
    for (k in ks) st[k] <- list(old$settings[[k]])
    mine$settings <- st
  }
  mine
}

# A planner's spec with what of the study folder's study.yml the writer
# does not write (.keep_study_yml_keys()) kept as the file has it
.spec_keep_file_blocks <- function(p, root) {
  f <- file.path(root, .study_file)
  txt <- p$spec$text[[.study_file]]
  if (is.null(txt) || !file.exists(f)) return(p)
  k <- .keep_study_yml_keys(txt, readLines(f, encoding = "UTF-8", warn = FALSE))
  if (!length(k$kept$top) && !length(k$kept$settings)) return(p)
  p$spec$text[[.study_file]] <- k$lines
  if (!is.null(p$spec$study$study)) {
    y <- tryCatch(tflspec::tfl_read_study_spec(root)$study, error = .old_form_reraise)
    p$spec$study$study <- .keep_study_yml_values(p$spec$study$study, y, k$kept)
  }
  p
}

# The study's definition written as the YAML spec into its folder: the
# view's edits taken into the spec (.spec_sync(): only the files they
# change come from the bridge, the others keep the spec's text), study.yml
# with the study's fields, when it was created and by which tflplanner; each
# file written only when its text changed (a save that changes one report
# writes its file), a report's file gone from the definition removed when
# it is the one the last save wrote (`own(rel)`).  The files, as
# save_study() lists them: file, status; the planner, in step with the
# spec now, is the attribute `planner`.  `before_write`: called with the
# planner in step just before the files are written (what they were is
# read then).
.save_yaml_spec <- function(p, meta, root, own = function(rel) FALSE,
                            before_write = NULL) {
  whole <- is.null(p$spec)
  # (written whole: the study's code lists, which a report's `list` cell
  # names, are the folder's defaults.yml's -- the bridge writes them back)
  scl <- if (whole && .has_yaml_spec(root)) {
    tryCatch(tflspec::tfl_read_study_spec(root)$codelists %||% list(), error = .old_form_reraise)
  }
  p <- .spec_sync(p, meta, codelists = scl)
  attr(p, "changed") <- NULL
  # (a planner with no spec is written whole: what of study.yml the writer
  # does not write -- the AI screens' ai:, a block by hand, a settings:
  # key no sheet holds -- kept from the file as it is)
  if (whole) p <- .spec_keep_file_blocks(p, root)
  p$spec$study$dir <- normalizePath(root, "/", mustWork = FALSE)
  if (is.function(before_write)) before_write(p)
  files <- .spec_write(p, root, own)
  # the series card's unsaved values, into the study's copies of the
  # series (#753); the planner saved has none left
  if (length(p$series_edits)) {
    files <- rbind(files, .series_save_edits(root, p$series_edits))
    p$series_edits <- NULL
  }
  attr(files, "planner") <- p
  files
}

# A study as its YAML spec would read back: its planner written by tflspec
# (a folder of its own) and read again -- the same definition in the way
# the files say it (rows in the TOC's order, the content's own form), to
# compare the files with (outside edits, imports)
.yaml_normal <- function(s) {
  tmp <- tempfile("tflplanner-normal")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  p <- tryCatch({
    # (the study's code lists, which a report's `list` cell names: the
    # spec's, else the folder's -- a study read from its state has no spec yet)
    scl <- if (!is.null(s$planner$spec)) s$planner$spec$study$codelists %||% list() else
      tflspec::tfl_read_study_spec(s$path)$codelists %||% list()
    tflspec::tfl_yaml_from_sheets(.planner_sheets(s$planner, s$meta, codelists = scl), tmp, overwrite = TRUE)
    .study_spec_keys(.planner_from_yaml(tmp))
  }, error = function(e) NULL)
  if (!is.null(p)) {
    attr(p, "not_converted") <- NULL
    # (the study's own spec stays its own)
    p$spec <- s$planner$spec
    # (the app's own settings pass through no sheet: they stay as saved)
    own <- intersect(.study_app_only_settings, names(s$planner$study))
    if (length(own)) p$study[own] <- s$planner$study[own]
    s$planner <- p
  }
  s
}
