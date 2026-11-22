# ---- the study's ADaM levels (#742) -----------------------------------------
#
# Whether study$adam gets levels, and from which file: the user names one,
# a define.xml or a Define Spec workbook, and applies it ([Data] -> ADaM
# levels).  Not set is none: no levels are applied.  Applying reads the file
# with tflspec (tfl_read_define() / tfl_read_define_spec()), keeps what was
# read as spec/adam_meta/<source>.yml, copies it into defaults.yml
# `adam_meta:` (what tflspec reads), and sets study.yml's settings
# adam_levels (the source), adam_levels_file and adam_levels_applied.
# Clearing the choice takes the three settings away and empties
# `adam_meta:`; the files under spec/adam_meta/ stay.  Both go through the
# study's model like any edit: on disk at the next save.

# The sources a study's levels come from (settings.adam_levels)
.adam_levels_sources <- c("define_xml", "define_spec")

# The study's ADaM levels: "define_xml", "define_spec", or "none" (not set,
# and any other value: an old study's "codelists" is none)
.study_adam_levels <- function(p) {
  v <- .study_setting(p, "adam_levels")
  if (!is.na(v) && v %in% .adam_levels_sources) v else "none"
}

# The setting as the screen shows it: the source, the file, when applied
.adam_levels_state <- function(p) {
  src <- .study_adam_levels(p)
  list(source = src,
       file = if (src != "none") .study_setting(p, "adam_levels_file") else NA_character_,
       applied = if (src != "none") .study_setting(p, "adam_levels_applied") else NA_character_)
}

# A file's source by its extension: .xml a define.xml, .xls / .xlsx a
# Define Spec; another, NA
.adam_levels_kind <- function(path) {
  ext <- tolower(tools::file_ext(as.character(path %||% "")[1L] %||% ""))
  if (identical(ext, "xml")) "define_xml" else
    if (ext %in% c("xls", "xlsx")) "define_spec" else NA_character_
}

# A path as typed or kept: quotes and blanks taken off ("" none)
.adam_levels_clean <- function(path) {
  x <- as.character(path %||% "")
  x <- if (length(x)) x[1L] else ""
  if (is.na(x)) "" else gsub('^"|"$', "", trimws(x))
}

# The file's full path: one kept relative is under the study's folder
.adam_levels_path <- function(path, study_dir) {
  path <- .adam_levels_clean(path)
  if (!nzchar(path)) return("")
  full <- grepl("^([A-Za-z]:)?[/\\\\]", path)
  normalizePath(if (full || is.null(study_dir)) path else file.path(study_dir, path),
                "/", mustWork = FALSE)
}

# The path as the setting keeps it: relative to the study's folder when it
# is in it (the study can move), else the full path
.adam_levels_rel <- function(path, study_dir) {
  f <- .adam_levels_path(path, study_dir)
  if (is.null(study_dir)) return(f)
  d <- sub("/+$", "", normalizePath(study_dir, "/", mustWork = FALSE))
  if (startsWith(tolower(f), tolower(paste0(d, "/")))) substring(f, nchar(d) + 2L) else f
}

# The one define.xml of the study's ADaM folder, offered when nothing is
# chosen (its path in the study's folder); none, or more than one: NA
.adam_levels_candidate <- function(study_dir) {
  if (is.null(study_dir)) return(NA_character_)
  d <- file.path(study_dir, study_layout()[["adam"]])
  f <- if (dir.exists(d)) list.files(d, "[.]xml$", ignore.case = TRUE) else character()
  if (length(f) == 1L) file.path(study_layout()[["adam"]], f) else NA_character_
}

# Whether the file chosen (`chosen`, the field) is not what is applied: a
# file other than the applied one, or the applied one changed since
.adam_levels_pending <- function(p, chosen, study_dir) {
  f <- .adam_levels_path(chosen, study_dir)
  if (!nzchar(f)) return(FALSE)
  st <- .adam_levels_state(p)
  if (is.na(st$file) || !identical(tolower(f), tolower(.adam_levels_path(st$file, study_dir)))) {
    return(TRUE)
  }
  at <- as.POSIXct(st$applied %||% NA_character_, format = "%Y-%m-%dT%H:%M:%S")
  mt <- file.mtime(f)
  !is.na(at) && !is.na(mt) && floor(as.numeric(mt)) > as.numeric(at)
}

# The status line's text (`t` translating)
.adam_levels_status_text <- function(st, t = identity) {
  when <- sub("T", " ", substr(st$applied %||% "", 1L, 16L), fixed = TRUE)
  switch(st$source,
         define_xml = sprintf(t("The levels of a define.xml are applied: %s (%s)"), st$file, when),
         define_spec = sprintf(t("The levels of a Define Spec are applied: %s (%s)"), st$file, when),
         t("No levels are applied (none)"))
}

# What a define gives the levels, as defaults.yml `adam_meta:` holds it:
# the variables that name a code list of the define, and those code lists'
# values (a row a value; a column with no value left out)
.adam_meta_from_define <- function(def) {
  cl <- as.data.frame(def$codelists, stringsAsFactors = FALSE)
  v <- as.data.frame(def$variables, stringsAsFactors = FALSE)
  v <- v[!is.na(v$codelist) & v$codelist %in% cl$codelist, c("dataset", "variable", "codelist"),
         drop = FALSE]
  cl <- cl[cl$codelist %in% v$codelist,
           intersect(c("codelist", "name", "value", "decode", "order"), names(cl)), drop = FALSE]
  rows <- function(d) lapply(seq_len(nrow(d)), function(i) {
    r <- lapply(d[i, , drop = FALSE], function(x) if (is.factor(x)) as.character(x) else x)
    r[!vapply(r, function(x) length(x) != 1L || is.na(x), NA)]
  })
  list(variables = rows(v), codelists = rows(cl))
}

# The `adam_meta:` block's lines: a comment saying where it was read from,
# then the block
.adam_meta_block_lines <- function(meta, source, file) {
  c(sprintf("# The ADaM metadata read from the %s %s by tflplanner ([Data] -> ADaM levels):",
            if (identical(source, "define_xml")) "define.xml" else "Define Spec", file),
    "# each variable's code list and the code lists' values. study$adam gets these levels.",
    strsplit(sub("\n$", "", yaml::as.yaml(list(adam_meta = meta))), "\n", fixed = TRUE)[[1L]])
}

# The block as tflspec reads it (its study$adam_meta): checked the way a
# study's defaults.yml is (no lines: a study with none)
.adam_meta_parse <- function(block) {
  d <- tempfile("tflplanner-adam-meta")
  dir.create(file.path(d, "spec"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c("# adam_meta", block), file.path(d, "spec", "defaults.yml"), useBytes = TRUE)
  tflspec::tfl_read_study_spec(d)$adam_meta
}

# `lines` (a YAML file's) with the top-level block `key` (its comment lines
# above it included) taken out, and `block` put in its place: before
# `styles:` (and its comment lines), else at the end.  `block` NULL: the key taken out only.
.set_study_block <- function(lines, key, block = NULL) {
  lines <- as.character(lines %||% character())
  top <- grepl("^[A-Za-z_][A-Za-z0-9_]*:", lines)
  i <- which(grepl(paste0("^", key, ":"), lines))[1L]
  if (!is.na(i)) {
    nxt <- which(top & seq_along(lines) > i)
    j <- if (length(nxt)) nxt[1L] - 1L else length(lines)
    # (the comment lines just above the next key belong to it)
    while (j > i && grepl("^#", lines[j])) j <- j - 1L
    from <- i
    while (from > 1L && grepl("^#", lines[from - 1L])) from <- from - 1L
    lines <- lines[-(from:j)]
  }
  if (length(block)) {
    at <- which(grepl("^styles:", lines))[1L]
    # (above the comment lines of styles:)
    if (!is.na(at)) while (at > 1L && grepl("^#", lines[at - 1L])) at <- at - 1L
    lines <- if (is.na(at)) c(lines, block) else append(lines, block, after = at - 1L)
  }
  lines
}

.adam_levels_keys <- c("adam_levels", "adam_levels_file", "adam_levels_applied")

# The ARD definition's study sheet with the key adam_levels as tflspec's
# tfl_sheets_from_spec() puts it there (the source; none: no row), so the
# sheets do not give study.yml back the value before
.adam_levels_ard_row <- function(p, value) {
  st <- p$ard$study
  if (!is.data.frame(st) || !"key" %in% names(st)) return(p)
  st <- st[!st$key %in% "adam_levels", , drop = FALSE]
  if (length(value) == 1L && !is.na(value) && value %in% .adam_levels_sources) {
    st[nrow(st) + 1L, ] <- NA
    st$key[nrow(st)] <- "adam_levels"
    st$value[nrow(st)] <- value
  }
  rownames(st) <- NULL
  p$ard$study <- st
  p
}

# The planner with the file's levels applied: the file read (a problem
# stops, nothing changed), spec/adam_meta/<source>.yml and defaults.yml
# `adam_meta:` written into the spec, the settings set.  `t` translates
# the messages.
.adam_levels_apply <- function(p, path, study_dir, now = Sys.time(), t = identity) {
  f <- .adam_levels_path(path, study_dir)
  if (!nzchar(f)) stop(t("Choose a define.xml or a Define Spec first."), call. = FALSE)
  kind <- .adam_levels_kind(f)
  if (is.na(kind)) {
    stop(sprintf(t("The file is a define.xml (.xml) or a Define Spec (.xls, .xlsx): %s"),
                 basename(f)), call. = FALSE)
  }
  if (!file.exists(f)) stop(sprintf(t("There is no file %s."), f), call. = FALSE)
  def <- tryCatch(
    if (identical(kind, "define_xml")) tflspec::tfl_read_define(f) else tflspec::tfl_read_define_spec(f),
    error = function(e) stop(sprintf(t("The file could not be read: %s"), conditionMessage(e)),
                             call. = FALSE))
  rel <- .adam_levels_rel(f, study_dir)
  block <- .adam_meta_block_lines(.adam_meta_from_define(def), kind, rel)
  meta <- .adam_meta_parse(block)
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  attr(p, "changed") <- NULL
  sp <- p$spec
  spec_dir <- study_layout()[["spec"]]
  sp$text[[file.path(spec_dir, "adam_meta", paste0(kind, ".yml"))]] <- block
  dflt <- file.path(spec_dir, "defaults.yml")
  sp$text[[dflt]] <- .set_study_block(sp$text[[dflt]], "adam_meta", block)
  sp$text <- sp$text[sort(names(sp$text), method = "radix")]
  sp$study$adam_meta <- meta
  p$spec <- sp
  p$study[["adam_levels"]] <- kind
  p$study[["adam_levels_file"]] <- rel
  p$study[["adam_levels_applied"]] <- format(now, "%Y-%m-%dT%H:%M:%S")
  .adam_levels_ard_row(p, kind)
}

# The planner with the choice cleared: none -- the settings taken away,
# defaults.yml `adam_meta:` emptied (nothing old is read); the files under
# spec/adam_meta/ stay
.adam_levels_clear <- function(p) {
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  attr(p, "changed") <- NULL
  sp <- p$spec
  dflt <- file.path(study_layout()[["spec"]], "defaults.yml")
  if (!is.null(sp$text[[dflt]])) sp$text[[dflt]] <- .set_study_block(sp$text[[dflt]], "adam_meta")
  sp$study$adam_meta <- .adam_meta_parse(character())
  p$spec <- sp
  for (k in .adam_levels_keys) p$study[[k]] <- NA_character_
  .adam_levels_ard_row(p, NA_character_)
}

# An old study's adam_levels: codelists (the default before #742) taken
# away: none, written as not set at the next save (no alias)
.adam_levels_drop_old <- function(p) {
  v <- .study_setting(p, "adam_levels")
  if (is.na(v) || v %in% c(.adam_levels_sources, "none")) return(p)
  for (k in .adam_levels_keys) if (k %in% names(p$study)) p$study[[k]] <- NA_character_
  .adam_levels_ard_row(p, NA_character_)
}
.adam_levels_dropped_msg <- "The ADaM levels setting changed. Choose a file in [Data] -> [ADaM levels] and apply it."

# A study folder copied (the sample's): its applied file given the time it
# was applied, so the copy is not taken for a change of it (the pending
# message compares the file's time with adam_levels_applied)
.adam_levels_settle <- function(path) {
  st <- tryCatch(yaml::read_yaml(file.path(path, .study_file))$settings, error = function(e) NULL)
  f <- st$adam_levels_file
  at <- as.POSIXct(as.character(st$adam_levels_applied %||% NA_character_), format = "%Y-%m-%dT%H:%M:%S")
  if (length(f) != 1L || is.na(at)) return(invisible(FALSE))
  full <- .adam_levels_path(f, path)
  invisible(file.exists(full) && Sys.setFileTime(full, at))
}
