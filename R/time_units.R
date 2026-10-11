# The days in a week, a month and a year: the figure programs divide a time
# in days by these named constants (tflspec writes
# `mutate(AVAL = AVAL / days_per_month)`; no data column has such a name:
# the review says so, rule D03).  A company sets them once,
# in its standards' setup code (copied into each new study's
# programs/study_setup.R, part 1); a study without them gets tflspec's
# values (its figure setup defines a constant only when it is not defined).

# the names of the programs' constants: no data column may have one
# (review rule D03)
.program_constants <- function() names(tflspec::tfl_time_unit_days())

# the setup code's lines, with tflspec's values
.time_unit_lines <- function() {
  d <- tflspec::tfl_time_unit_days()
  sprintf("%s <- %s", names(d), vapply(unname(d), format, ""))
}

# The values a study's programs use: its programs/study_setup.R's lines
# that set them (part 1, the company's; part 3, the user's -- the last one
# wins, as when the file runs), else tflspec's.  Read, not run.
.study_time_units <- function(path) {
  d <- tflspec::tfl_time_unit_days()
  f <- file.path(path, .study_setup_path())
  if (!file.exists(f)) return(d)
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  for (k in names(d)) {
    rx <- sprintf("^[[:space:]]*%s[[:space:]]*(<-|=)[[:space:]]*([0-9]+([.][0-9]*)?)[[:space:]]*(#.*)?$", k)
    hit <- regmatches(lines, regexec(rx, lines))
    v <- unlist(lapply(hit, function(m) if (length(m)) m[3L]))
    if (length(v)) d[[k]] <- as.numeric(v[length(v)])
  }
  d
}
