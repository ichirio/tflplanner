# Updating tflplanner and its upstream, and telling the app's user that a
# newer version is out.
#
# A session cannot replace a package it has loaded (on Windows the files
# are in use, elsewhere the session goes on with the old code), so the
# installing is always done by inst/launcher/update.R in a separate
# Rscript: from update_tflplanner() here, and from the "update and launch"
# shortcut.  The app itself only says that a newer version is out.

.upstream <- c(rtfreporter = "ichirio/rtfreporter",
               tflspec = "ichirio/tflspec",
               tflplanner = "ichirio/tflplanner")

.channels <- c("release", "dev")

.update_channel <- function(home = tflplanner_home()) {
  ch <- tflplanner_config(home)$update_channel
  if (is.null(ch) || !ch %in% .channels) "release" else ch
}

.installed_version <- function(p) {
  d <- system.file("DESCRIPTION", package = p)
  if (!nzchar(d)) NA_character_ else unname(read.dcf(d, fields = "Version")[1L, 1L])
}

.cran_repos <- function() {
  r <- getOption("repos")
  if (is.null(r) || !length(r) || identical(unname(r[["CRAN"]]), "@CRAN@")) {
    r <- c(CRAN = "https://cloud.r-project.org")
  }
  r
}

.read_url <- function(url, timeout = 5) {
  old <- options(timeout = timeout)
  on.exit(options(old))
  tryCatch(suppressWarnings(readLines(url, warn = FALSE, encoding = "UTF-8")),
           error = function(e) NULL)
}

# The newest version of each package on `channel`: release = CRAN when it is
# there, else the latest GitHub release; dev = the DESCRIPTION on GitHub
# main.  NA when it cannot be found (offline, rate limit, no release).
.latest_versions <- function(pkgs = names(.upstream), channel = "release",
                             timeout = 5) {
  channel <- match.arg(channel, .channels)
  ap <- local({
    old <- options(timeout = timeout)
    on.exit(options(old))
    tryCatch(suppressWarnings(utils::available.packages(repos = .cran_repos())),
             error = function(e) NULL)
  })
  vapply(pkgs, function(p) {
    if (identical(channel, "release") && !is.null(ap) && p %in% rownames(ap)) {
      return(unname(ap[p, "Version"]))
    }
    repo <- .upstream[p]
    if (is.na(repo)) return(NA_character_)
    if (identical(channel, "dev")) {
      d <- .read_url(sprintf("https://raw.githubusercontent.com/%s/main/DESCRIPTION",
                             repo), timeout)
      v <- sub("^Version:[[:space:]]*", "", grep("^Version:", d, value = TRUE))
    } else {
      j <- .read_url(sprintf("https://api.github.com/repos/%s/releases/latest",
                             repo), timeout)
      tag <- tryCatch(jsonlite::fromJSON(paste(j, collapse = "\n"))$tag_name,
                      error = function(e) NULL)
      v <- sub("^v", "", tag %||% character())
    }
    if (length(v) == 1L && nzchar(v)) v else NA_character_
  }, "")
}

.newer <- function(latest, installed) {
  ok <- !is.na(latest) & !is.na(installed)
  out <- rep(FALSE, length(latest))
  out[ok] <- mapply(function(a, b) package_version(a) > package_version(b),
                    latest[ok], installed[ok])
  out
}

#' The packages tflplanner needs, and their versions
#'
#' Lists the packages tflplanner needs (rtfreporter, tflspec, tflplanner
#' itself) and the ones its programs use when they are there (cards, cardx,
#' dplyr, ggplot2, ...), with the version installed and -- when `check` --
#' the newest one.
#'
#' @param check Look up the newest versions (needs the internet; a version
#'   that cannot be found is `NA`).
#' @param channel For rtfreporter, tflspec and tflplanner: `"release"`
#'   (CRAN when the package is there, else its latest GitHub release) or
#'   `"dev"` (GitHub main).  `NULL` uses the channel last used by
#'   [update_tflplanner()].
#' @return A data frame: `package`, `role` (`"required"` / `"suggested"`),
#'   `installed`, `latest`, `status` (`"missing"`, `"update"`, `"ok"`, or
#'   `NA` when not checked).
#' @examples
#' tflplanner_packages(check = FALSE)
#' @export
tflplanner_packages <- function(check = TRUE, channel = NULL) {
  channel <- channel %||% .update_channel()
  sug <- .suggested_packages()
  pkgs <- c(names(.upstream), sug)
  out <- data.frame(
    package = pkgs,
    role = c(rep("required", length(.upstream)), rep("suggested", length(sug))),
    installed = vapply(pkgs, .installed_version, ""),
    latest = NA_character_, stringsAsFactors = FALSE, row.names = NULL)
  if (isTRUE(check)) {
    out$latest[out$role == "required"] <- .latest_versions(names(.upstream), channel)
    if (length(sug)) {
      ap <- tryCatch(suppressWarnings(utils::available.packages(repos = .cran_repos())),
                     error = function(e) NULL)
      if (!is.null(ap)) {
        hit <- out$role == "suggested" & out$package %in% rownames(ap)
        out$latest[hit] <- unname(ap[out$package[hit], "Version"])
      }
    }
  }
  out$status <- ifelse(is.na(out$installed), "missing",
                       ifelse(!check, NA_character_,
                              ifelse(.newer(out$latest, out$installed),
                                     "update", "ok")))
  out
}

.suggested_packages <- function() {
  d <- system.file("DESCRIPTION", package = "tflplanner")
  s <- if (nzchar(d)) read.dcf(d, fields = "Suggests")[1L, 1L] else NA
  if (is.na(s)) return(character())
  s <- trimws(sub("\\(.*$", "", strsplit(s, ",")[[1L]]))
  setdiff(s[nzchar(s)], c("testthat", "withr", "knitr", "rmarkdown"))
}

#' Update tflplanner, rtfreporter and tflspec
#'
#' Updates rtfreporter, then tflspec, then tflplanner -- in that order, so
#' each finds the version of the one before it that it needs.  The work is
#' done in a separate R process, since this session cannot replace a
#' package it has loaded; afterwards, restart R (in RStudio: *Session >
#' Restart R*) before using tflplanner again.  The running app does not
#' update itself: it says when a newer version is out.
#'
#' @param channel `"release"`: CRAN when the package is there, else its
#'   latest GitHub release.  `"dev"`: the development version on GitHub
#'   (main).  `NULL` uses the channel last used (`"release"` at first).  The
#'   channel is remembered, and the "update and launch" shortcut uses it.
#' @param from A folder with package files (`rtfreporter_*.tar.gz` /
#'   `.zip` / `.tgz`, and the same for tflspec and tflplanner) to install
#'   from instead, when this computer cannot reach GitHub.  The newest file
#'   of each package is installed.
#' @param ask Show the plan and ask before installing.  Outside an
#'   interactive session nothing is installed unless `ask = FALSE`.
#' @return `TRUE` when the update finished, invisibly.
#' @seealso [tflplanner_packages()], [add_shortcut()].
#' @examples
#' \dontrun{
#' # not run: installs packages
#' update_tflplanner()                    # the released versions
#' update_tflplanner("dev")               # the development versions
#' update_tflplanner(from = "D:/tflplanner-packages")
#' }
#' @export
update_tflplanner <- function(channel = NULL, from = NULL,
                              ask = interactive()) {
  lang <- .console_language()
  if (!is.null(channel)) channel <- match.arg(channel, .channels)
  channel <- channel %||% .update_channel()
  if (!is.null(from) && !dir.exists(from)) {
    stop("`from` must be a folder with the package files.", call. = FALSE)
  }
  lib <- dirname(system.file(package = "tflplanner"))
  if (file.access(lib, 2L) != 0L) {
    stop("Cannot write to the library tflplanner is in (", lib, ").",
         call. = FALSE)
  }
  if (isTRUE(ask)) {
    if (is.null(from)) {
      cur <- vapply(names(.upstream), .installed_version, "")
      new <- .latest_versions(names(.upstream), channel)
      cat(sprintf(tr("Update on the %s channel:", lang), channel), "\n",
          paste0(sprintf("  %-12s %s -> %s", names(.upstream), cur,
                         ifelse(is.na(new), "?", new)), collapse = "\n"),
          "\n", sep = "")
      if (all(is.na(new))) {
        cat(tr("(The newest versions could not be looked up from here -- GitHub's addresses may be closed; the update itself may still get through.)", lang),
            "\n", sep = "")
      }
    } else {
      cat(sprintf(tr("Install from %s:", lang), from), "\n",
          paste0("  ", list.files(from, "^(rtfreporter|tflspec|tflplanner)_"),
                 collapse = "\n"), "\n", sep = "")
    }
    cat(.update_plan_text(channel, from, lib, lang), sep = "\n")
    if (!.ask_yes(tr("Update these packages now?", lang))) {
      message(tr("Nothing was installed.", lang))
      return(invisible(FALSE))
    }
  } else if (!identical(ask, FALSE)) {
    stop("update_tflplanner() asks before it installs anything; call it in ",
         "an interactive session, or with `ask = FALSE`.", call. = FALSE)
  }
  if (is.null(from) && .is_set_up()) .set_config("update_channel", channel)
  # a copy: the package folder that holds update.R is about to be replaced
  script <- tempfile(fileext = ".R")
  file.copy(.launcher_src("update.R"), script)
  on.exit(unlink(script), add = TRUE)
  rscript <- file.path(R.home("bin"),
                       if (identical(.os(), "windows")) "Rscript.exe" else "Rscript")
  res <- tryCatch(
    processx::run(rscript,
                  c(script, paste0("--channel=", channel),
                    paste0("--lib=", lib),
                    if (!is.null(from)) paste0("--from=", from),
                    .update_net_args()),
                  echo = TRUE, error_on_status = FALSE),
    error = function(e) list(status = NA_integer_,
                             stdout = paste("FAILED:", conditionMessage(e))))
  ok <- identical(res$status, 0L)
  if (ok) {
    message(tr("Done. Restart R (in RStudio: Session > Restart R) before using tflplanner again.", lang))
  } else {
    message(paste(.update_failed_text(res$stdout, channel, from, lang),
                  collapse = "\n"))
  }
  invisible(ok)
}

# This session's network settings for the update's own R process, which
# does not have them: the package repositories (RStudio's, an internal
# mirror) and the download method.  Proxies (http_proxy ...) are
# environment variables, inherited as they are.
.update_net_args <- function() {
  r <- .cran_repos()
  r <- r[!is.na(r) & nzchar(r) & r != "@CRAN@"]
  if (is.null(names(r)) || !all(nzchar(names(r)))) {
    names(r) <- paste0("repo", seq_along(r))
  }
  m <- getOption("download.file.method")
  c(if (length(r)) paste0("--repos=", paste(names(r), r, sep = "=", collapse = ";")),
    if (is.character(m) && length(m) == 1L && nzchar(m)) paste0("--method=", m))
}

# What to say when the update did not finish: what could not be reached,
# and the two ways round it.
.update_failed_text <- function(out, channel = "release", from = NULL,
                                lang = .console_language()) {
  lines <- unlist(strsplit(out %||% "", "\n", fixed = TRUE))
  bad <- sub("^UNREACHABLE: ", "", grep("^UNREACHABLE: ", lines, value = TRUE))
  failed <- sub("^FAILED: ", "", grep("^FAILED: ", lines, value = TRUE))
  ref <- if (identical(channel, "dev")) "" else "@*release"
  c(tr("The update did not finish.", lang),
    if (length(failed)) paste0("  ", failed),
    if (length(bad)) c(
      tr("The update's R process could not reach:", lang),
      paste0("  ", bad),
      tr("(A firewall or a proxy: this R session's own settings are passed on, but a network may let RStudio through and not R.)", lang)),
    if (is.null(from)) c(
      tr("To install by hand in this session (then restart R):", lang),
      sprintf("  remotes::install_github(\"%s%s\")", .upstream, ref),
      tr("Or download the three package files where you can and install them from a folder:", lang),
      "  update_tflplanner(from = \"<folder>\")"))
}

# What update_tflplanner() is about to do, in words: how, where, in what
# order, what it leaves, what to do after.
.update_plan_text <- function(channel, from, lib, lang = .console_language()) {
  c(sprintf(tr("They are installed into %s, in a separate R process, in this order: rtfreporter, tflspec, tflplanner.", lang),
            .show_path(lib)),
    if (is.null(from)) c(
      tr("Channels: \"release\" = CRAN, else the latest GitHub release; \"dev\" = the development version on GitHub.", lang),
      sprintf(tr("The other channel: update_tflplanner(\"%s\").", lang),
              setdiff(.channels, channel)[1L])),
    tr("Your studies and tflplanner's settings are not touched.", lang),
    tr("Afterwards, restart R (in RStudio: Session > Restart R) before using tflplanner again.", lang))
}

# ------------------------------------------------------------- the app's notice

# The check runs once per R process, in the background (it may wait on the
# network): a separate Rscript writes the newest versions to a file.
.upd <- new.env()

.start_update_check <- function(home = tflplanner_home()) {
  if (!is.null(.upd$proc) || !is.null(.upd$result)) return(invisible())
  # the setting, and an option for tests and offline sites
  # off unless turned on (the app's settings, setup_tflplanner(check_updates
  # = TRUE)): it looks on the network, and it never installs anything
  if (isFALSE(getOption("tflplanner.check_updates", TRUE)) ||
      !isTRUE(as.logical(tflplanner_config(home)$check_updates %||% "false"))) {
    .upd$result <- list()
    return(invisible())
  }
  out <- tempfile(fileext = ".rds")
  rscript <- file.path(R.home("bin"),
                       if (identical(.os(), "windows")) "Rscript.exe" else "Rscript")
  code <- sprintf(
    "saveRDS(tflplanner:::.latest_versions(channel = '%s'), '%s')",
    .update_channel(home), gsub("\\\\", "/", out))
  .upd$file <- out
  .upd$proc <- tryCatch(
    processx::process$new(rscript, c("-e", code), stdout = NULL,
                          stderr = NULL),
    error = function(e) NULL)
  if (is.null(.upd$proc)) .upd$result <- list()
  invisible()
}

# The packages with a newer version out, once the check is done: a named
# character vector "installed -> latest"; NULL while it runs.
# The packages this R process runs, against the ones installed now: an
# update installed while the app was running (the launcher then opens the
# running app, the old version) leaves this process on the old code.  A
# named list, package = c(running, installed), of those that differ.
.stale_versions <- function(pkgs = c("tflplanner", "tflspec", "rtfreporter"),
                            running = function(p) as.character(getNamespaceVersion(p)),
                            path = function(p) getNamespaceInfo(p, "path")) {
  out <- list()
  for (p in intersect(pkgs, loadedNamespaces())) {
    f <- file.path(path(p), "DESCRIPTION")
    if (!file.exists(f)) next
    inst <- tryCatch(read.dcf(f, fields = "Version")[1L, 1L], error = function(e) NA)
    run <- running(p)
    if (!is.na(inst) && !identical(unname(inst), unname(run))) {
      out[[p]] <- c(running = unname(run), installed = unname(inst))
    }
  }
  out
}

.update_check_result <- function() {
  if (!is.null(.upd$result)) return(.upd$result)
  p <- .upd$proc
  if (is.null(p) || p$is_alive()) return(NULL)
  latest <- tryCatch(readRDS(.upd$file), error = function(e) NULL)
  unlink(.upd$file)
  .upd$proc <- NULL
  if (is.null(latest)) {
    .upd$result <- list()
    return(.upd$result)
  }
  .upd$result <- .newer_than_installed(latest)
  .upd$result
}

# "installed -> latest" for each package with a newer version; empty when
# none (paste() of nothing would give one " -> ").
.newer_than_installed <- function(latest) {
  inst <- vapply(names(latest), .installed_version, "")
  newer <- .newer(latest, inst)
  if (!any(newer)) return(character())
  stats::setNames(paste(inst[newer], "->", latest[newer]),
                  names(latest)[newer])
}
