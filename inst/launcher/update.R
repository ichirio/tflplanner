# Updates rtfreporter -> tflspec -> tflplanner, in that order.
#
# Run by tflplanner::update_tflplanner() and by the "update and launch"
# shortcut -- always in a fresh R process that has not loaded tflplanner: a
# session cannot replace a package it has loaded.  Base R only, so it can
# replace any package it touches.
#
#   Rscript update.R [--channel=release|dev] [--from=<folder>] [--lib=<library>]
#
#   release  CRAN when the package is there, else its latest GitHub release
#   dev      the GitHub `main` branch
#   --from   install the three from package files in a folder instead
#            (rtfreporter_*.tar.gz / .zip / .tgz, ...): no network needed
#
# Exit status 0 = done (or already up to date), 1 = something failed.

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1L]]) else default
}
channel <- arg("channel", "release")
from    <- arg("from")
lib     <- arg("lib", .libPaths()[[1L]])
if (!channel %in% c("release", "dev")) {
  cat("unknown channel: ", channel, "\n", sep = "")
  quit(status = 1L)
}

pkgs <- c("rtfreporter", "tflspec", "tflplanner")
repo <- c(rtfreporter = "ichirio/rtfreporter", tflspec = "ichirio/tflspec",
          tflplanner = "ichirio/tflplanner")
cran <- getOption("repos")
if (is.null(cran) || !length(cran) || identical(unname(cran[["CRAN"]]), "@CRAN@")) {
  cran <- c(CRAN = "https://cloud.r-project.org")
}
options(repos = cran, timeout = max(300, getOption("timeout")))
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
.libPaths(c(lib, .libPaths()))

say <- function(...) cat(..., "\n", sep = "")
# the version R would load: `lib` first, then the other libraries
installed <- function(p) {
  d <- system.file("DESCRIPTION", package = p, lib.loc = .libPaths())
  if (!nzchar(d)) return(NA_character_)
  unname(read.dcf(d, fields = "Version")[1L, 1L])
}
fail <- function(p, e) {
  say("FAILED: ", p, ": ", conditionMessage(e))
  quit(status = 1L)
}

say("tflplanner update: ", if (is.null(from)) channel else paste("from", from),
    " -> ", lib)

if (!is.null(from)) {
  files <- list.files(from, full.names = TRUE)
  for (p in pkgs) {
    pat <- paste0("^", p, "_([0-9][0-9.-]*)\\.(tar\\.gz|zip|tgz)$")
    f <- files[grepl(pat, basename(files))]
    if (!length(f)) {
      say(p, ": no package file in ", from, " -- left as it is")
      next
    }
    v <- numeric_version(sub(pat, "\\1", basename(f)))
    f <- f[order(v, decreasing = TRUE)][[1L]]
    want <- sub(pat, "\\1", basename(f))
    say(p, ": ", installed(p), " -> ", want, " (", basename(f), ")")
    tryCatch(
      utils::install.packages(f, lib = lib, repos = NULL,
                              type = if (grepl("tar\\.gz$", f)) "source" else "binary"),
      error = function(e) fail(p, e))
    if (!identical(installed(p), want)) {
      fail(p, simpleError("the installed version is not the file's"))
    }
  }
  say("done")
  quit(status = 0L)
}

ap <- tryCatch(utils::available.packages(repos = cran),
               error = function(e) NULL)
on_cran <- function(p) {
  if (!is.null(ap) && p %in% rownames(ap)) ap[p, "Version"] else NA_character_
}

for (p in pkgs) {
  have <- installed(p)
  cv <- on_cran(p)
  if (identical(channel, "release") && !is.na(cv)) {
    if (!is.na(have) && package_version(have) >= package_version(cv)) {
      say(p, ": ", have, " is up to date (CRAN)")
      next
    }
    say(p, ": ", have, " -> ", cv, " (CRAN)")
    tryCatch(utils::install.packages(p, lib = lib), error = function(e) fail(p, e))
  } else {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      say("installing remotes (to install from GitHub)")
      tryCatch(utils::install.packages("remotes", lib = lib),
               error = function(e) fail("remotes", e))
    }
    ref <- if (identical(channel, "dev")) repo[[p]] else
      paste0(repo[[p]], "@*release")
    say(p, ": ", have, " -> ", ref, " (GitHub)")
    tryCatch(remotes::install_github(ref, lib = lib, upgrade = "never",
                                     dependencies = NA),
             error = function(e) fail(p, e))
  }
  say(p, ": now ", installed(p))
}
say("done")
quit(status = 0L)
