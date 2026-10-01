# The tflplanner launcher, run by its shortcuts (tflplanner::add_shortcut())
# and by tflplanner::launch_app().  tflplanner keeps a copy of this file in
# tools::R_user_dir("tflplanner", "config")/launcher, beside update.R; the
# shortcuts point at that copy, so they keep working when R or tflplanner
# is updated.
#
#   Rscript launch.R [--update] [--port=N]
#
# 1. --update: update rtfreporter -> tflspec -> tflplanner first, in another
#    R process (update.R), on the channel kept in config.yml.
# 2. If tflplanner already runs on the port, open it in the browser and stop.
# 3. Otherwise start it and open the browser; closing the browser stops it.
#
# Nothing here loads tflplanner before the update is done.

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1L]]) else default
}
`%||%` <- function(a, b) if (is.null(a) || !length(a) || !nzchar(a[[1L]])) b else a
self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
here <- dirname(normalizePath(self, "/", mustWork = FALSE))
is_win <- identical(.Platform$OS.type, "windows")
is_mac <- identical(Sys.info()[["sysname"]], "Darwin")

log_file <- file.path(here, "launcher.log")
if (file.exists(log_file) && file.size(log_file) > 1e6) unlink(log_file)
say <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S "), ..., "\n", sep = "",
      file = log_file, append = TRUE)
}

# tflplanner's home, as tflplanner_home() finds it (without loading it):
# the pointer file setup_tflplanner() writes, the environment variable, or
# the default folder.
home <- {
  ptr <- file.path(dirname(here), "home")
  h <- if (file.exists(ptr)) trimws(readLines(ptr, n = 1L, warn = FALSE)) else ""
  if (!length(h) || !nzchar(h)) h <- Sys.getenv("TFLPLANNER_HOME")
  if (!nzchar(h)) h <- tools::R_user_dir("tflplanner", "data")
  h
}
# config.yml holds plain `key: value` lines
cfg <- local({
  f <- file.path(home, "config.yml")
  if (!file.exists(f)) return(list())
  l <- readLines(f, warn = FALSE, encoding = "UTF-8")
  m <- regmatches(l, regexec("^([A-Za-z_]+):[[:space:]]*(.*)$", l))
  m <- m[lengths(m) == 3L]
  stats::setNames(lapply(m, function(x) gsub("^['\"]|['\"]$", "", x[[3L]])),
                  vapply(m, `[`, "", 2L))
})
port <- as.integer(arg("port", cfg$port %||% "7470"))
ja <- identical(cfg$language, "ja")
msg <- function(en, jp) if (ja) jp else en

# A short message on the desktop: the launcher has no console to write to.
notify <- function(text, wait = FALSE) {
  say(text)
  try(silent = TRUE, {
    if (is_win) {
      f <- tempfile(fileext = ".vbs")
      code <- paste0('CreateObject("WScript.Shell").Popup "',
                     gsub('"', '""', text), '", ', if (wait) 0L else 8L,
                     ', "tflplanner", 64')
      con <- file(f, "wb")
      writeBin(c(as.raw(c(0xFF, 0xFE)),
                 iconv(code, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1L]]), con)
      close(con)
      system2(file.path(Sys.getenv("SystemRoot"), "System32", "wscript.exe"),
              shQuote(f), wait = wait)
    } else if (is_mac) {
      system2("osascript", c("-e", shQuote(sprintf(
        'display notification "%s" with title "tflplanner"',
        gsub('"', '\\\\"', text)))), wait = FALSE)
    } else if (nzchar(Sys.which("notify-send"))) {
      system2("notify-send", c("tflplanner", shQuote(text)), wait = FALSE)
    }
  })
  invisible(NULL)
}

say("launch: port ", port, if ("--update" %in% args) " (update first)")

if ("--update" %in% args) {
  notify(msg("Updating tflplanner... The browser opens when it is done.",
             "tflplanner を更新しています。終わるとブラウザが開きます。"))
  rscript <- file.path(R.home("bin"), if (is_win) "Rscript.exe" else "Rscript")
  pkg_dir <- find.package("tflplanner", quiet = TRUE)
  lib <- if (length(pkg_dir)) dirname(pkg_dir[[1L]]) else .libPaths()[[1L]]
  status <- system2(rscript,
                    c(shQuote(file.path(here, "update.R")),
                      paste0("--channel=", cfg$update_channel %||% "release"),
                      shQuote(paste0("--lib=", lib))),
                    stdout = file.path(here, "update.log"),
                    stderr = file.path(here, "update.log"))
  if (!identical(as.integer(status), 0L)) {
    notify(msg(paste0("The update did not finish; tflplanner starts as it was. See ", file.path(here, "update.log")),
               paste0("更新は完了しませんでした。今の版で起動します。詳しくは ", log_file)))
  }
}

url <- sprintf("http://127.0.0.1:%d/", port)
running <- local({
  old <- options(timeout = 3)
  on.exit(options(old))
  page <- tryCatch(suppressWarnings(readLines(url, warn = FALSE)),
                   error = function(e) character())
  any(grepl("<title>tflplanner</title>", page, fixed = TRUE))
})
if (running) {
  say("already running: ", url)
  utils::browseURL(url)
  quit(save = "no", status = 0L)
}

if (!requireNamespace("tflplanner", quietly = TRUE)) {
  notify(msg("tflplanner is not installed in this R.",
             "この R には tflplanner が入っていません。"),
         wait = TRUE)
  quit(save = "no", status = 1L)
}
ok <- tryCatch({
  run <- tflplanner::run_app
  # an older tflplanner (before 0.0.2.9004) has no stop_on_close
  if ("stop_on_close" %in% names(formals(run))) {
    run(port = port, launch.browser = TRUE, stop_on_close = TRUE)
  } else {
    run(port = port, launch.browser = TRUE)
  }
  TRUE
}, error = function(e) {
  notify(msg(paste0("tflplanner could not start: ", conditionMessage(e)),
             paste0("tflplanner を起動できませんでした: ",
                    conditionMessage(e))), wait = TRUE)
  FALSE
})
say("stopped")
quit(save = "no", status = if (ok) 0L else 1L)
