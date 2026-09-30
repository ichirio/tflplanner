# ============================================================================
#  Problems a person can act on
# ----------------------------------------------------------------------------
#  When a program the app runs for the user fails (an ARD's data code, a
#  report), the screen gets one short message -- what did not work, why,
#  and what to do -- and the full output of the program goes to the R
#  console, where it can be read in full.  A tflplanner_problem carries the
#  two separately; the app's guarded() shows the first and logs the second.
# ============================================================================

.problem <- function(message, detail = NULL) {
  structure(class = c("tflplanner_problem", "error", "condition"),
            list(message = message, call = NULL, detail = detail))
}

# What R's output says went wrong, in one line: the first "Error" line
# without the call it names ("Error in library(x) : there is no package
# called 'x'" -> "there is no package called 'x'").  NA when there is none.
.first_error <- function(output) {
  lines <- unlist(strsplit(paste(output, collapse = "\n"), "\n", fixed = TRUE))
  i <- grep("^Error", lines)[1L]
  if (is.na(i)) return(NA_character_)
  # R writes "Error in <call> : <message>" or "Error: <message>"
  msg <- sub("^Error: ", "", sub("^Error in .*? : ", "", lines[i], perl = TRUE))
  # a long call puts the message on the next line: "Error in <call> :"
  if ((!nzchar(trimws(msg)) || grepl(":\\s*$", msg)) && i < length(lines)) {
    msg <- lines[i + 1L]
  }
  trimws(msg)
}

# The next step for a failure we recognise
.problem_hint <- function(why) {
  if (is.na(why)) return("")
  pkg <- regmatches(why, regexec("there is no package called .([A-Za-z0-9.]+).", why))[[1L]]
  if (length(pkg) == 2L) {
    return(sprintf(" Install it in the R that runs this app: install.packages(\"%s\").",
                   pkg[2L]))
  }
  ""
}
