# ---- run this batch again ----------------------------------------------------
# (tflplanner writes this file; the same for every study)
#
# Run it from the batch folder (open the .Rproj next to it, or
# Rscript rerun.R there).  It makes rerun_<date>_<time>/ in the batch
# folder: the study as the run had it (code/: its programs, definition
# workbooks and study.yml; data/: the data files its programs read), runs
# the same programs there, and compares what they make with what the run
# kept (output/): compare.csv, one row a file.  The kept results are never
# written over.  The R and package versions the run used are in
# session_info.txt (and renv.lock when the study had one).

local({
  if (!file.exists("batch.txt") || !file.exists("run.csv")) {
    stop("Run it from the batch folder (the one with batch.txt and run.csv).")
  }
  if (!dir.exists("code") || !dir.exists("data")) {
    stop("This batch folder did not keep its code and data: it cannot be run again on its own.")
  }
  here <- getwd()
  root <- file.path(here, paste0("rerun_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(root)
  copy_tree <- function(from, to, skip = character()) {
    files <- list.files(from, recursive = TRUE, all.files = TRUE)
    files <- files[!dirname(files) %in% skip & !files %in% skip]
    for (f in files) {
      dir.create(file.path(to, dirname(f)), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(from, f), file.path(to, f), overwrite = TRUE, copy.date = TRUE)
    }
    files
  }

  # the data: as the run read them (data_manifest.csv), checked first
  man <- utils::read.csv("data_manifest.csv", colClasses = "character")
  now <- unname(tools::md5sum(man$kept))
  bad <- man$kept[is.na(now) | now != man$md5]
  if (length(bad)) {
    stop("These kept data files are not as the run read them (md5): ",
         paste(bad, collapse = ", "))
  }
  ext <- man$path[startsWith(man$kept, "data/external/")]
  if (length(ext)) {
    warning("Data read from outside the study folder are kept in data/external/, ",
            "but the programs read them where they were: ", paste(ext, collapse = ", "),
            call. = FALSE)
  }
  copy_tree("code", root)
  copy_tree("data", root, skip = "external")
  # the study's folders (study_setup.R's path_... <- "..."): those the
  # programs write to (output/tfl ...) are there, empty
  setup <- readLines(file.path(root, "programs", "study_setup.R"), warn = FALSE)
  dirs <- sub('^path_[a-z_]+ *<- *"([^"]*)".*$', "\\1",
              grep('^path_[a-z_]+ *<- *"[^"]*"', setup, value = TRUE))
  for (d in dirs) dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)

  # what ran: the same programs, by part
  run <- utils::read.csv("run.csv", colClasses = "character")
  parts <- unique(run$part)
  # a run of the reports alone read the study ARD it kept
  if (!"ard" %in% parts && dir.exists(file.path("output", "ard"))) {
    copy_tree(file.path("output", "ard"), file.path(root, "output", "ard"))
  }
  setwd(root)
  on.exit(setwd(here), add = TRUE)
  env <- new.env()
  sys.source(file.path("programs", "batch.R"), envir = env)
  old <- options(tflplanner.rerun = TRUE)
  env$run_batch(parts, c("--no-code", sub("[.][Rr]$", "", basename(run$program))))
  options(old)
  setwd(here)

  # what it made against what the run kept: the same bytes; else the same
  # but the date and time a report prints ({DATETIME}) or a status records;
  # else (an .rds) the same R object
  stamp <- c("[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}(:[0-9]{2})?",
             "[0-9]{1,2}[A-Za-z]{3}[0-9]{4} +[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?",
             "\\\\(creatim|revtim|printim)[^}]*")
  masked <- function(f) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    for (p in stamp) x <- gsub(p, "<time>", x, perl = TRUE, useBytes = TRUE)
    x
  }
  kept <- list.files("output", recursive = TRUE)
  res <- vapply(kept, function(f) {
    a <- file.path("output", f)
    b <- file.path(root, "output", f)
    if (!file.exists(b)) return("not made")
    if (identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))) return("identical")
    if (grepl("[.]rds$", f, ignore.case = TRUE)) {
      return(if (identical(readRDS(a), readRDS(b))) "same object" else "different")
    }
    if (grepl("[.](rtf|csv|txt)$", f, ignore.case = TRUE) &&
        identical(masked(a), masked(b))) return("same but the time")
    "different"
  }, "")
  cmp <- data.frame(file = file.path("output", kept), result = unname(res),
                    stringsAsFactors = FALSE)
  utils::write.csv(cmp, file.path(root, "compare.csv"), row.names = FALSE)
  cat("\nMade again in", basename(root), ":\n")
  print(table(cmp$result))
  if (any(cmp$result %in% c("different", "not made"))) {
    print(cmp[cmp$result %in% c("different", "not made"), ], row.names = FALSE)
  } else {
    cat("Every result the run kept is made again the same.\n")
  }
  invisible(cmp)
})
