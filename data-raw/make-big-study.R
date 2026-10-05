# A study of 200 reports for trying the report list (Make a report, on the
# left) and for the screen reviews: the sample study's reports copied into
# 10 sections of a TOC, their states mixed (R/sample_big.R).  It is made
# in a temporary home -- never in a user's own (tools::R_user_dir() too is
# a temporary folder here) -- and the app is started on it.
#
#   Rscript data-raw/make-big-study.R [n] [port]

a <- commandArgs(TRUE)
n <- if (length(a) >= 1L) as.integer(a[1]) else 200L
port <- if (length(a) >= 2L) as.integer(a[2]) else 7480L
base <- file.path(tempdir(), "big-study")
Sys.setenv(R_USER_CONFIG_DIR = file.path(base, "config"),
           R_USER_DATA_DIR = file.path(base, "data"),
           R_USER_CACHE_DIR = file.path(base, "cache"))
devtools::load_all(quiet = TRUE)
home <- file.path(base, "home")
options(tflplanner.home = home)
suppressMessages(setup_tflplanner(studies_root = file.path(base, "studies")))
s <- .make_big_study(n, root = file.path(base, "studies"), home = home)
message("The study: ", s$path, " (", length(output_ids(s$planner)), " reports)")
run_app(s$meta$study_id, port = port)
