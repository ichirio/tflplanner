# The days in a week, a month and a year: the figure programs divide a time
# in days by these named constants (tflspec writes
# `mutate(AVAL = AVAL / days_per_month)`; no data column has such a name:
# the review says so, rule D03).  A company sets them once,
# in its standards' setup code (copied into each new study's
# programs/company_setup.R); a study without them gets tflspec's
# values (its figure setup defines a constant only when it is not defined).

# the names of the programs' constants: no data column may have one
# (review rule D03)
.program_constants <- function() names(tflspec::tfl_time_unit_days())

# the setup code's lines, with tflspec's values
.time_unit_lines <- function() {
  d <- tflspec::tfl_time_unit_days()
  # (as.character: not format(), which follows options(digits))
  sprintf("%s <- %s", names(d), as.character(unname(d)))
}

# The values a study's programs use: the lines of its setup files that set
# them, in the order a program runs them (company_setup.R, study_setup.R,
# study_user.R; a study made before #910, its one study_setup.R) -- the
# last one wins, as when they run -- else tflspec's.  Read, not run.
.study_time_units <- function(path) {
  d <- tflspec::tfl_time_unit_days()
  f <- .study_setup_present(path)
  if (!length(f)) return(d)
  lines <- unlist(lapply(f, readLines, warn = FALSE, encoding = "UTF-8"), use.names = FALSE)
  for (k in names(d)) {
    rx <- sprintf("^[[:space:]]*%s[[:space:]]*(<-|=)[[:space:]]*([0-9]+([.][0-9]*)?)[[:space:]]*(#.*)?$", k)
    hit <- regmatches(lines, regexec(rx, lines))
    v <- unlist(lapply(hit, function(m) if (length(m)) m[3L]))
    if (length(v)) d[[k]] <- as.numeric(v[length(v)])
  }
  d
}
