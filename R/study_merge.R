# The parts a study is saved in, each merged on its own.
# Split from R/study.R by section; no code changed.

# The parts a study is saved in, each merged on its own.
.study_parts <- function(s) {
  p <- s$planner
  c(stats::setNames(lapply(.study_fields, function(k) s$meta[[k]]),
                    paste0("meta:", .study_fields)),
    list(`study keys` = p$study, setup = p$setup,
         `report list` = p$outputs),
    stats::setNames(p$sheets, paste0("sheet:", names(p$sheets))),
    stats::setNames(p$ard, paste0("ard:", names(p$ard))),
    stats::setNames(p$lf %||% .empty_lf(), paste0("lf:", .lf_sheet_names)),
    # each figure's design (spec/figures/<id>.yml)
    if (length(p$fig_designs))
      stats::setNames(p$fig_designs, paste0("fig:", names(p$fig_designs))))
}

.set_study_part <- function(s, part, value) {
  kind <- sub(":.*$", "", part)
  name <- sub("^[^:]*:", "", part)
  switch(kind,
    meta = s$meta[[name]] <- value,
    sheet = s$planner$sheets[[name]] <- value,
    ard = s$planner$ard[[name]] <- value,
    lf = s$planner$lf[[name]] <- value,
    fig = s$planner <- set_fig_design(s$planner, name, value),
    `study keys` = s$planner$study <- value,
    setup = s$planner$setup <- value,
    `report list` = s$planner$outputs <- value)
  s
}

.merge_saved <- function(study, base, home) {
  # (nobody saved since this session did: the state on disk is the one it
  # wrote, from the planner it is based on -- nothing to merge, #417)
  f <- .state_file(study$meta$study_id, home)
  seen <- .state_seen[[f]]
  if (!is.null(seen) && file.exists(f) && identical(unname(tools::md5sum(f)), seen$md5) &&
      identical(base$planner, seen$planner)) {
    return(study)
  }
  st <- .read_state(study$meta$study_id, home)
  if (is.null(st)) return(study)
  disk <- .study_from_state(st)
  base$planner <- .study_spec_keys(base$planner)
  study$planner <- .study_spec_keys(study$planner)
  mine <- .study_parts(study)
  was <- .study_parts(base)
  now <- .study_parts(disk)
  clash <- character()
  # a figure's design is a part when either side has it
  for (k in union(names(mine), union(names(was), names(now)))) {
    changed_here <- !identical(mine[[k]], was[[k]])
    changed_there <- !identical(now[[k]], was[[k]])
    if (!changed_here && changed_there) {
      study <- .set_study_part(study, k, now[[k]])
    } else if (changed_here && changed_there &&
               !identical(mine[[k]], now[[k]])) {
      clash <- c(clash, k)
    }
  }
  if (length(clash)) {
    cond <- structure(class = c("tflplanner_conflict", "error", "condition"),
      list(message = paste0(
        "Someone else saved changes to the same part(s) since you opened the study: ",
        paste(clash, collapse = ", "),
        ".  Open the study again to see them (your unsaved changes to those parts would be lost)."),
        call = NULL, parts = clash))
    stop(cond)
  }
  study
}
