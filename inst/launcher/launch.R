# The tflplanner launcher, run by its shortcuts (tflplanner::add_shortcut())
# and by tflplanner::launch_app().  tflplanner keeps a copy of this file in
# tools::R_user_dir("tflplanner", "config")/launcher, beside update.R; the
# shortcuts point at that copy, so they keep working when R or tflplanner
# is updated.
#
#   Rscript launch.R [--update] [--port=N]
#
# 1. --update: update rtfreporter -> tflspec -> tflplanner first, in another
#    R process (update.R), on the channel kept in config.yml -- not while an
#    official run goes on (it has loaded the packages).
# 2. The port taken:
#    - by this version of tflplanner, answering: open it in the browser;
#    - by another version, or a tflplanner that does not answer: ask whether
#      to stop it and start again (an official run goes on by itself);
#    - by another program: start on the next free port.
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

# A file wscript.exe runs (UTF-16, so any language shows)
vbs_file <- function(code) {
  f <- tempfile(fileext = ".vbs")
  con <- file(f, "wb")
  writeBin(c(as.raw(c(0xFF, 0xFE)),
             iconv(code, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1L]]), con)
  close(con)
  f
}
vbs_text <- function(text) {
  paste0('"', gsub("\n", '" & vbCrLf & "', gsub('"', '""', text), fixed = TRUE), '"')
}
wscript <- function(f, wait) {
  system2(file.path(Sys.getenv("SystemRoot"), "System32", "wscript.exe"),
          shQuote(f), wait = wait)
}

# A short message on the desktop: the launcher has no console to write to.
notify <- function(text, wait = FALSE) {
  say(text)
  # (the tests: in the log only)
  if (nzchar(Sys.getenv("TFLPLANNER_LAUNCHER_ANSWER"))) return(invisible(NULL))
  try(silent = TRUE, {
    if (is_win) {
      wscript(vbs_file(paste0('CreateObject("WScript.Shell").Popup ', vbs_text(text), ", ",
                              if (wait) 0L else 8L, ', "tflplanner", 64')), wait)
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

# A yes / no question on the desktop; no answer (no way to ask) is no.
ask_yes <- function(text) {
  say("ask: ", gsub("\n", " ", text, fixed = TRUE))
  # (the tests answer without a dialog)
  auto <- Sys.getenv("TFLPLANNER_LAUNCHER_ANSWER")
  yes <- if (nzchar(auto)) identical(auto, "yes") else tryCatch({
    if (is_win) {
      # 4 = Yes and No, 32 = a question mark; Yes is 6
      identical(as.integer(wscript(vbs_file(paste0(
        "WScript.Quit MsgBox(", vbs_text(text), ', 4 + 32, "tflplanner")')), TRUE)), 6L)
    } else if (is_mac) {
      out <- system2("osascript", c("-e", shQuote(sprintf(
        'display dialog "%s" buttons {"%s", "%s"} default button 2 with title "tflplanner"',
        gsub('"', '\\\\"', text), msg("No", "いいえ"), msg("Yes", "はい")))),
        stdout = TRUE, stderr = FALSE)
      any(grepl(msg("Yes", "はい"), out, fixed = TRUE))
    } else if (nzchar(Sys.which("zenity"))) {
      identical(as.integer(system2("zenity", c("--question", "--title=tflplanner",
                                               shQuote(paste0("--text=", text))))), 0L)
    } else {
      FALSE
    }
  }, error = function(e) FALSE)
  say("answer: ", if (yes) "yes" else "no")
  yes
}

# ---- the official runs going on --------------------------------------------
# tflplanner records each official run in the home (R/live_runs.R): its
# process and when that was created (a pid used again is not the run)
live_runs <- function() {
  fs <- list.files(file.path(home, "live_runs"), "[.]dcf$", full.names = TRUE)
  have_ps <- requireNamespace("ps", quietly = TRUE)
  runs <- lapply(fs, function(f) {
    r <- tryCatch(as.list(read.dcf(f)[1L, ]), error = function(e) NULL)
    if (is.null(r) || is.null(r$pid)) return(NULL)
    if (have_ps) {
      now <- tryCatch(as.numeric(ps::ps_create_time(ps::ps_handle(as.integer(r$pid)))),
                      error = function(e) NA_real_)
      if (is.na(now) || abs(now - as.numeric(r$created)) >= 1) return(NULL)
    }
    r
  })
  Filter(Negate(is.null), runs)
}
runs_names <- function(runs) {
  paste(vapply(runs, function(r) sprintf("%s %s", r$study_id, substr(r$started, 12L, 16L)), ""),
        collapse = ", ")
}

say("launch: port ", port, if ("--update" %in% args) " (update first)")

if ("--update" %in% args) {
  runs <- live_runs()
  if (length(runs)) {
    # the packages a run has loaded cannot be replaced (or the run breaks)
    notify(msg(paste0("Not updated: an official run is going on -- ", runs_names(runs),
                      ". Update when it has finished; tflplanner starts as it is."),
               paste0("更新しませんでした。正式実行が動いています: ", runs_names(runs),
                      "。終わってから更新してください。今の版で起動します。")))
  } else {
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
}

# ---- the port ---------------------------------------------------------------
app_url <- function(p) sprintf("http://127.0.0.1:%d/", p)
get_lines <- function(u, wait = 3) {
  old <- options(timeout = wait)
  on.exit(options(old))
  tryCatch(suppressWarnings(readLines(u, warn = FALSE)), error = function(e) character())
}
# tflplanner's page answers (an app that is not stuck): the app makes the
# page for each request, a few seconds while it starts or is busy
page_answers <- function(p) {
  any(grepl("<title>tflplanner</title>", get_lines(app_url(p), wait = 15), fixed = TRUE))
}
# what a tflplanner app says of itself (R/app.R .serve_health(); an app
# older than this launcher says nothing): list(version, pid), or NULL
health <- function(p) {
  h <- get_lines(paste0(app_url(p), "tflplanner-health/health.txt"))
  v <- sub("^tflplanner ", "", grep("^tflplanner ", h, value = TRUE))
  pid <- suppressWarnings(as.integer(sub("^pid ", "", grep("^pid ", h, value = TRUE))))
  if (length(v)) list(version = v[[1L]], pid = if (length(pid)) pid[[1L]] else NA_integer_)
}
# Can an app listen on the port (127.0.0.1, as shiny does)?
port_free <- function(p) {
  if (!requireNamespace("httpuv", quietly = TRUE)) return(!length(get_lines(app_url(p))))
  s <- tryCatch(httpuv::startServer("127.0.0.1", p, list(call = function(req) {
    list(status = 200L, headers = list(), body = "")
  })), error = function(e) NULL)
  if (is.null(s)) return(FALSE)
  httpuv::stopServer(s)
  TRUE
}
# The process listening on the port, NA when it cannot be told
port_owner <- function(p) {
  out <- tryCatch(if (is_win) {
    system2("netstat", c("-ano", "-p", "TCP"), stdout = TRUE, stderr = FALSE)
  } else {
    system2("lsof", c("-nP", paste0("-iTCP:", p), "-sTCP:LISTEN", "-t"), stdout = TRUE, stderr = FALSE)
  }, error = function(e) character())
  pid <- if (is_win) {
    hit <- grep(paste0("^\\s*TCP\\s+\\S+:", p, "\\s+\\S+\\s+LISTENING\\s+[0-9]+\\s*$"), out,
                value = TRUE, perl = TRUE)
    sub("^.*LISTENING\\s+([0-9]+)\\s*$", "\\1", hit, perl = TRUE)
  } else out
  pid <- suppressWarnings(as.integer(pid))
  pid <- pid[!is.na(pid)]
  if (length(pid)) pid[[1L]] else NA_integer_
}
# Is a process tflplanner's (this launcher, or an app started from R)?
# A process's command line
cmdline <- function(pid) {
  if (is.na(pid) || !requireNamespace("ps", quietly = TRUE)) return(character())
  tryCatch(ps::ps_cmdline(ps::ps_handle(pid)), error = function(e) character())
}
# A launcher's app (this script runs the app in its own process): the one
# the launcher may stop.  An app run in an R session (RStudio's, a
# console's) is that session's: it is never stopped from here.
by_launcher <- function(pid) any(grepl("launch.R", cmdline(pid), fixed = TRUE))
stop_app <- function(pid) {
  say("stopping the app on port ", port, " (process ", pid, ")")
  ok <- tryCatch(tools::pskill(pid), error = function(e) FALSE)
  for (i in 1:30) {
    if (port_free(port)) return(TRUE)
    Sys.sleep(0.5)
  }
  say("the port is still taken after stopping process ", pid, " (", ok, ")")
  FALSE
}
# Leave the port to what holds it: start on the next free one
next_port <- function() {
  for (p in port + seq_len(20L)) {
    if (port_free(p)) {
      say("starting on port ", p, " instead")
      return(p)
    }
  }
  notify(msg(paste0("Port ", port, " and the 20 after it are taken: set another port (setup_tflplanner(port = ))."),
             paste0("ポート ", port, " とその後の 20 個が使われています。別のポートを設定してください（setup_tflplanner(port = )）。")),
         wait = TRUE)
  quit(save = "no", status = 1L)
}

installed <- tryCatch(utils::packageDescription("tflplanner")$Version,
                      error = function(e) NA_character_)
if (!port_free(port)) {
  h <- health(port)
  answers <- page_answers(port)
  pid <- if (!is.null(h) && !is.na(h$pid)) h$pid else port_owner(port)
  if (!is.null(h) && identical(h$version, installed) && answers) {
    # 1. this version, answering: open it (the usual double click)
    say("already running (same version ", installed, "): ", app_url(port))
    utils::browseURL(app_url(port))
    quit(save = "no", status = 0L)
  } else if ((!is.null(h) || answers) && !by_launcher(pid)) {
    # tflplanner in an R session of its own (RStudio, a console): not ours
    # to stop -- this one starts beside it
    say("tflplanner on port ", port, " runs in an R session (process ", pid,
        if (!is.null(h)) paste0(", version ", h$version), "): left running")
    port <- next_port()
  } else if (!is.null(h) || answers || by_launcher(pid)) {
    # 2. a launcher's tflplanner, another version or not answering: stop
    # it on yes
    say("tflplanner on port ", port, ": ",
        if (is.null(h)) "an older version (no health)" else paste("version", h$version),
        if (answers) ", answering" else ", not answering", ", process ", pid,
        "; installed ", installed)
    runs <- live_runs()
    text <- paste(c(
      if (!answers && !is.null(h) && identical(h$version, installed))
        msg("tflplanner is running but does not answer. Stop it and start again?",
            "tflplanner が動いていますが応答しません。止めて起動し直しますか？") else
        msg("An older tflplanner is running. Stop it and start again?",
            "古いアプリが動いています。止めて起動し直しますか？"),
      if (length(runs)) msg(paste0("The official run (", runs_names(runs), ") goes on: it is not stopped."),
                            paste0("正式実行（", runs_names(runs), "）は止まらずに続きます。")),
      if (is.null(h)) msg("If an official run is going on in the older app, it stops too.",
                          "古いアプリの中で正式実行が動いていれば、それも止まります。"),
      msg("Unsaved changes in it are lost.", "保存していない変更は失われます。")), collapse = "\n")
    if (!ask_yes(text)) {
      say("left running")
      if (answers) utils::browseURL(app_url(port))
      quit(save = "no", status = 0L)
    }
    if (!stop_app(pid)) {
      notify(msg(paste0("The app on port ", port, " did not stop. Close it, then start again."),
                 paste0("ポート ", port, " のアプリが止まりませんでした。閉じてから起動し直してください。")),
             wait = TRUE)
      quit(save = "no", status = 1L)
    }
  } else {
    # 3. another program: leave it, take the next free port
    say("port ", port, " is taken by another program (process ", pid, ")")
    port <- next_port()
  }
}

if (!requireNamespace("tflplanner", quietly = TRUE)) {
  notify(msg("tflplanner is not installed in this R.",
             "この R には tflplanner が入っていません。"),
         wait = TRUE)
  quit(save = "no", status = 1L)
}
# what the app writes to the console (its errors, with the calls) goes to
# app.log, beside this file: the launcher has no console (the last run's)
app_log <- file(file.path(here, "app.log"), open = "wt")
sink(app_log, type = "message")
say("start: port ", port, ", tflplanner ", installed)
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
sink(type = "message")
close(app_log)
say("stopped")
quit(save = "no", status = if (ok) 0L else 1L)
