# ---- the study's ADaM levels (#742) -----------------------------------------
#
# Whether study$adam gets levels, and from which file: the user names one,
# a define.xml or a Define Spec workbook, and applies it ([Data] -> ADaM
# levels).  Not set is none: no levels are applied.  Applying reads the file
# with tflspec (tfl_read_define() / tfl_read_define_spec()), keeps what was
# read as spec/adam_meta/<source>.yml (with the file's md5), copies it into
# defaults.yml `adam_meta:` (what tflspec reads), and sets study.yml's
# settings adam_levels (the source), adam_levels_file and
# adam_levels_applied.  Clearing the choice takes the three settings away
# and empties `adam_meta:`; the files under spec/adam_meta/ stay.  Both go
# through the study's model like any edit: on disk at the next save.

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
  # (~ is the home folder, not a folder of the study)
  path <- path.expand(path)
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

# A file's checksum as [Apply] keeps it: its bytes' md5, a define.xml's
# line ends as LF (a checkout may give it CRLF: the same file)
.adam_levels_md5 <- function(f) {
  if (!nzchar(f) || !file.exists(f)) return(NA_character_)
  x <- readBin(f, "raw", file.size(f))
  if (identical(.adam_levels_kind(f), "define_xml") && length(x) > 1L) {
    cr <- which(x[-length(x)] == as.raw(13L) & x[-1L] == as.raw(10L))
    if (length(cr)) x <- x[-cr]
  }
  tmp <- tempfile("tflplanner-md5")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(x, tmp)
  unname(tools::md5sum(tmp))
}

# spec/adam_meta/<source>.yml's path in the study
.adam_meta_kept_rel <- function(source) {
  file.path(study_layout()[["spec"]], "adam_meta", paste0(source, ".yml"))
}

# A kept file's lines: the spec's (applied, not saved yet), else the study
# folder's; none, NULL
.adam_meta_kept_lines <- function(p, study_dir, source) {
  rel <- .adam_meta_kept_rel(source)
  x <- p$spec$text[[rel]]
  if (!is.null(x)) return(as.character(x))
  f <- if (!is.null(study_dir)) file.path(study_dir, rel)
  if (!is.null(f) && file.exists(f)) readLines(f, encoding = "UTF-8", warn = FALSE)
}

# The checksum [Apply] kept of the applied file (its kept file's `# md5:`
# line); none kept, NA
.adam_levels_md5_kept <- function(p, study_dir) {
  st <- .adam_levels_state(p)
  if (st$source == "none") return(NA_character_)
  x <- grep("^# md5: ", .adam_meta_kept_lines(p, study_dir, st$source), value = TRUE)
  if (length(x)) sub("^# md5: *", "", x[1L]) else NA_character_
}

# Whether the file chosen (`chosen`, the field) is not what is applied:
# "other" (a file other than the applied one, or none applied), "changed"
# (the applied file's content changed since [Apply]), or "" (nothing)
.adam_levels_pending <- function(p, chosen, study_dir) {
  f <- .adam_levels_path(chosen, study_dir)
  if (!nzchar(f)) return("")
  st <- .adam_levels_state(p)
  if (is.na(st$file) || !identical(tolower(f), tolower(.adam_levels_path(st$file, study_dir)))) {
    return("other")
  }
  kept <- .adam_levels_md5_kept(p, study_dir)
  now <- .adam_levels_md5(f)
  if (!is.na(kept) && !is.na(now) && !identical(kept, now)) "changed" else ""
}
.adam_levels_pending_msg <- c(
  other = "The file chosen is not applied yet",
  changed = "The file changed after it was applied. Press [Apply] again.")

# What the applied levels hold: variables with a code list, code lists
.adam_levels_counts <- function(p) {
  m <- p$spec$study$adam_meta
  c(variables = NROW(m$variables), codelists = length(unique(m$codelists$codelist)))
}

# The status: a line saying what is applied, and (applied) one with the
# file, when, and how much (`t` translating)
.adam_levels_status_text <- function(st, t = identity, counts = NULL) {
  when <- sub("T", " ", substr(st$applied %||% "", 1L, 16L), fixed = TRUE)
  head <- switch(st$source,
                 define_xml = t("The levels of a define.xml are applied"),
                 define_spec = t("The levels of a Define Spec are applied"),
                 t("No levels are applied"))
  if (st$source == "none") return(head)
  n <- counts %||% c(variables = NA_integer_, codelists = NA_integer_)
  c(head, sprintf(t("%s, applied %s, %s variables, %s code lists"),
                  st$file, when, n[["variables"]], n[["codelists"]]))
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

# The lines of the top-level block `key` of `lines` (a YAML file's), the
# comment lines above it included; none, integer(0)
.study_block_range <- function(lines, key) {
  top <- grepl("^[A-Za-z_][A-Za-z0-9_]*:", lines)
  i <- which(grepl(paste0("^", key, ":"), lines))[1L]
  if (is.na(i)) return(integer())
  nxt <- which(top & seq_along(lines) > i)
  j <- if (length(nxt)) nxt[1L] - 1L else length(lines)
  # (the comment lines just above the next key belong to it)
  while (j > i && grepl("^#", lines[j])) j <- j - 1L
  from <- i
  while (from > 1L && grepl("^#", lines[from - 1L])) from <- from - 1L
  from:j
}

# `lines` with the top-level block `key` (its comment lines above it
# included) taken out, and `block` put in its place: before `styles:` (and
# its comment lines), else at the end.  `block` NULL: the key taken out only.
.set_study_block <- function(lines, key, block = NULL) {
  lines <- as.character(lines %||% character())
  r <- .study_block_range(lines, key)
  if (length(r)) lines <- lines[-r]
  if (length(block)) {
    at <- which(grepl("^styles:", lines))[1L]
    # (above the comment lines of styles:)
    if (!is.na(at)) while (at > 1L && grepl("^#", lines[at - 1L])) at <- at - 1L
    lines <- if (is.na(at)) c(lines, block) else append(lines, block, after = at - 1L)
  }
  lines
}

# Whether the study's adam_meta has rows (defaults.yml `adam_meta:`)
.adam_meta_has_rows <- function(p) {
  m <- p$spec$study$adam_meta
  NROW(m$variables) > 0L || NROW(m$codelists) > 0L
}

# The spec with defaults.yml's `adam_meta:` block kept as
# spec/adam_meta/previous.yml before it is replaced or taken out, when the
# screen did not write it (it is not the kept file of the applied source):
# a block written by hand, or an old study's
.adam_meta_keep_previous <- function(p, study_dir) {
  sp <- p$spec
  dflt <- file.path(study_layout()[["spec"]], "defaults.yml")
  lines <- as.character(sp$text[[dflt]] %||% character())
  r <- .study_block_range(lines, "adam_meta")
  if (!length(r)) return(sp)
  block <- lines[r]
  src <- .study_adam_levels(p)
  kept <- if (src != "none") .adam_meta_kept_lines(p, study_dir, src)
  kept <- kept[!grepl("^# md5: ", kept)]
  if (length(kept) && identical(kept, block)) return(sp)
  sp$text[[.adam_meta_kept_rel("previous")]] <-
    c("# defaults.yml's adam_meta: block before [Data] -> ADaM levels replaced it", block)
  sp$text <- sp$text[sort(names(sp$text), method = "radix")]
  sp
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

# The planner with the file's levels applied: the file read (a problem, or
# a file that maps no variable to a code list, stops: nothing changed),
# spec/adam_meta/<source>.yml (with the file's md5) and defaults.yml
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
  rows <- .adam_meta_from_define(def)
  if (!length(rows$variables)) {
    stop(sprintf(t("%s maps no variable to a code list: there are no levels to apply."), basename(f)),
         call. = FALSE)
  }
  block <- .adam_meta_block_lines(rows, kind, rel)
  meta <- .adam_meta_parse(block)
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  attr(p, "changed") <- NULL
  sp <- .adam_meta_keep_previous(p, study_dir)
  sp$text[[.adam_meta_kept_rel(kind)]] <- c(paste0("# md5: ", .adam_levels_md5(f)), block)
  dflt <- file.path(study_layout()[["spec"]], "defaults.yml")
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
# defaults.yml `adam_meta:` emptied (nothing old is read; a block the screen
# did not write kept as spec/adam_meta/previous.yml); the files under
# spec/adam_meta/ stay
.adam_levels_clear <- function(p, study_dir = NULL) {
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  attr(p, "changed") <- NULL
  sp <- .adam_meta_keep_previous(p, study_dir)
  dflt <- file.path(study_layout()[["spec"]], "defaults.yml")
  if (!is.null(sp$text[[dflt]])) sp$text[[dflt]] <- .set_study_block(sp$text[[dflt]], "adam_meta")
  sp$study$adam_meta <- .adam_meta_parse(character())
  p$spec <- sp
  for (k in .adam_levels_keys) p$study[[k]] <- NA_character_
  .adam_levels_ard_row(p, NA_character_)
}

# Whether [Clear the choice] has anything to clear: levels applied, a file
# in the field, or `adam_meta:` rows left
.adam_levels_clearable <- function(p, field) {
  .study_adam_levels(p) != "none" || nzchar(.adam_levels_clean(field)) || .adam_meta_has_rows(p)
}

# An old study's setting (before #742) on open: a value other than
# define_xml / define_spec -- codelists, the old default, or none -- taken
# away (not set is none; no alias).  `notice` when its levels were, or may
# have been, applied before: the value was codelists (or another), or it is
# not set (or none) and defaults.yml has `adam_meta:` rows (codelists was
# the default and was not written).  list(p, notice).
.adam_levels_drop_old <- function(p) {
  v <- .study_setting(p, "adam_levels")
  if (!is.na(v) && v %in% .adam_levels_sources) return(list(p = p, notice = FALSE))
  notice <- if (is.na(v) || identical(v, "none")) .adam_meta_has_rows(p) else TRUE
  if (!is.na(v)) {
    for (k in .adam_levels_keys) if (k %in% names(p$study)) p$study[[k]] <- NA_character_
    p <- .adam_levels_ard_row(p, NA_character_)
  }
  list(p = p, notice = notice)
}
.adam_levels_dropped_msg <- "The ADaM levels setting changed. Choose a file in [Data] -> [ADaM pre-processing] and apply it."
