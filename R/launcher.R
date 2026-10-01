# Starting tflplanner from a shortcut.
#
# add_shortcut() keeps a copy of the launcher (inst/launcher: launch.R,
# update.R, the OS script and an icon) in
# tools::R_user_dir("tflplanner", "config")/launcher, and points the
# shortcuts at that copy -- not at the package library, which moves with
# every R version.  The OS script finds R at every start, so an R update
# does not break a shortcut either.
#
#   Windows  desktop and Start menu .lnk -> wscript.exe tflplanner.vbs
#            (no console window; the R installer's registry entry, else the
#            newest R-x.y.z folder)
#   macOS    ~/Applications/tflplanner.app and "tflplanner (update).app",
#            each a shell script in an application bundle
#   Linux    ~/.local/share/applications/tflplanner.desktop, with an
#            "Update and launch" action
#
# The app never updates itself: a session cannot replace a package it has
# loaded.  The "update and launch" shortcut updates in another R process
# first (update.R), then starts the app.

.default_port <- 7470L

.launcher_dir <- function() {
  file.path(tools::R_user_dir("tflplanner", "config"), "launcher")
}

.os <- function() {
  if (identical(.Platform$OS.type, "windows")) "windows"
  else if (identical(Sys.info()[["sysname"]], "Darwin")) "mac" else "linux"
}

# The port the launcher and launch_app() use: config.yml, else 7470.
.app_port <- function(home = tflplanner_home()) {
  p <- suppressWarnings(as.integer(tflplanner_config(home)$port))
  if (length(p) != 1L || is.na(p)) .default_port else p
}

.check_port <- function(port) {
  port <- suppressWarnings(as.integer(port))
  if (length(port) != 1L || is.na(port) || port < 1024L || port > 65535L) {
    stop("`port` must be one number from 1024 to 65535.", call. = FALSE)
  }
  port
}

# The language for the launcher's own messages: the app's, else the
# system's (a first setup has no setting yet).
.console_language <- function() {
  l <- tflplanner_config()$language
  if (!is.null(l)) return(l)
  loc <- paste(Sys.getlocale("LC_CTYPE"), Sys.getenv("LANG"))
  if (grepl("Japanese|ja_JP|^ja", loc)) "ja" else "en"
}

.write_utf16 <- function(text, file) {
  con <- file(file, "wb")
  on.exit(close(con))
  writeBin(c(as.raw(c(0xFF, 0xFE)),
             iconv(paste(text, collapse = "\r\n"), "UTF-8", "UTF-16LE",
                   toRaw = TRUE)[[1L]]), con)
  invisible(file)
}

.launcher_src <- function(name) {
  f <- system.file("launcher", name, package = "tflplanner")
  if (!nzchar(f)) stop("tflplanner's launcher file ", name,
                       " is missing: reinstall tflplanner.", call. = FALSE)
  f
}

# Write (or refresh) the launcher's files.  Returns the folder.
.write_launcher <- function(dir = .launcher_dir(),
                            lang = .console_language()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (f in c("launch.R", "update.R", "tflplanner.png", "tflplanner.ico",
              "tflplanner.icns")) {
    file.copy(.launcher_src(f), file.path(dir, f), overwrite = TRUE)
  }
  no_r <- tr("R was not found. Install R, then try again.", lang)
  vbs <- readLines(.launcher_src("tflplanner.vbs"), warn = FALSE)
  .write_utf16(gsub("{{NO_R}}", no_r, vbs, fixed = TRUE),
               file.path(dir, "tflplanner.vbs"))
  sh <- readLines(.launcher_src("tflplanner.sh"), warn = FALSE)
  sh_file <- file.path(dir, "tflplanner.sh")
  con <- file(sh_file, "wb")
  writeLines(enc2utf8(gsub("{{NO_R}}", no_r, sh, fixed = TRUE)), con,
             sep = "\n", useBytes = TRUE)
  close(con)
  Sys.chmod(sh_file, "0755")
  writeLines(as.character(utils::packageVersion("tflplanner")),
             file.path(dir, "VERSION"))
  dir
}

# Keep an existing launcher in step with the installed tflplanner.
.refresh_launcher <- function(dir = .launcher_dir()) {
  v <- file.path(dir, "VERSION")
  if (!file.exists(v)) return(invisible(FALSE))
  now <- as.character(utils::packageVersion("tflplanner"))
  if (identical(readLines(v, n = 1L, warn = FALSE), now)) return(invisible(FALSE))
  try(.write_launcher(dir), silent = TRUE)
  invisible(TRUE)
}

# ------------------------------------------------------------- where

# Where the shortcuts go.  `tflplanner.shortcut_dirs` (a named list:
# desktop, start_menu, applications) overrides, for tests.
.shortcut_dirs <- function(os = .os()) {
  o <- getOption("tflplanner.shortcut_dirs")
  if (!is.null(o)) return(o)
  switch(os,
    windows = {
      sf <- tryCatch(utils::readRegistry(
        "Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders",
        "HCU"), error = function(e) list())
      expand <- function(p) {
        if (is.null(p)) return(NULL)
        for (v in regmatches(p, gregexpr("%[^%]+%", p))[[1L]]) {
          p <- sub(v, Sys.getenv(gsub("%", "", v)), p, fixed = TRUE)
        }
        normalizePath(p, "/", mustWork = FALSE)
      }
      up <- Sys.getenv("USERPROFILE")
      list(desktop = expand(sf$Desktop) %||% file.path(up, "Desktop"),
           start_menu = file.path(
             expand(sf$Programs) %||% file.path(Sys.getenv("APPDATA"),
               "Microsoft", "Windows", "Start Menu", "Programs"),
             "tflplanner"))
    },
    mac = list(applications = path.expand("~/Applications")),
    linux = {
      xdg <- Sys.getenv("XDG_DATA_HOME")
      if (!nzchar(xdg)) xdg <- path.expand("~/.local/share")
      list(applications = file.path(xdg, "applications"))
    })
}

.shortcut_names <- function(lang) {
  c(launch = "tflplanner",
    update = tr("tflplanner (update and launch)", lang))
}

# ------------------------------------------------------------- Windows

# A path with Windows separators.  normalizePath(winslash = "\\") changes
# them only on Windows, and these strings are also built (and tested)
# elsewhere.
.win_path <- function(p) {
  if (identical(.os(), "windows")) p <- normalizePath(p, "/", mustWork = FALSE)
  gsub("/", "\\", p, fixed = TRUE)
}

# The shortcuts as data: one row per .lnk.
.windows_shortcuts <- function(dir, dirs, lang, desktop = TRUE,
                               start_menu = TRUE, update = TRUE,
                               port = NULL) {
  nm <- .shortcut_names(lang)
  extra <- if (!is.null(port)) paste0(" --port=", port) else ""
  wscript <- file.path(Sys.getenv("SystemRoot", "C:/Windows"), "System32",
                       "wscript.exe")
  vbs <- paste0('"', .win_path(file.path(dir, "tflplanner.vbs")), '"')
  rows <- list()
  add <- function(where, name, args, desc) {
    rows[[length(rows) + 1L]] <<- data.frame(
      path = file.path(where, paste0(name, ".lnk")), target = wscript,
      args = paste0(vbs, args, extra),
      icon = .win_path(file.path(dir, "tflplanner.ico")),
      workdir = .win_path(dir),
      description = desc, stringsAsFactors = FALSE)
  }
  d_launch <- tr("Start tflplanner in the browser", lang)
  d_update <- tr("Update tflplanner, then start it", lang)
  if (desktop) add(dirs$desktop, nm[["launch"]], "", d_launch)
  if (start_menu) {
    add(dirs$start_menu, nm[["launch"]], "", d_launch)
    if (update) add(dirs$start_menu, nm[["update"]], " --update", d_update)
  }
  if (update && !start_menu && desktop) {
    add(dirs$desktop, nm[["update"]], " --update", d_update)
  }
  do.call(rbind, rows)
}

# A VBScript that makes the .lnk files of `sc` (WScript.Shell, which every
# Windows has; PowerShell may be blocked by policy).
.lnk_script <- function(sc) {
  q <- function(x) paste0('"', gsub('"', '""', x), '"')
  win <- .win_path
  body <- unlist(lapply(seq_len(nrow(sc)), function(i) {
    r <- sc[i, ]
    c(sprintf("fso_mkdir %s", q(win(dirname(r$path)))),
      sprintf("Set l = sh.CreateShortcut(%s)", q(win(r$path))),
      sprintf("l.TargetPath = %s", q(r$target)),
      sprintf("l.Arguments = %s", q(r$args)),
      sprintf("l.IconLocation = %s", q(paste0(r$icon, ",0"))),
      sprintf("l.WorkingDirectory = %s", q(r$workdir)),
      sprintf("l.Description = %s", q(r$description)),
      "l.WindowStyle = 1",
      "l.Save")
  }))
  c('Set sh = CreateObject("WScript.Shell")',
    'Set fso = CreateObject("Scripting.FileSystemObject")',
    "Sub fso_mkdir(p)",
    "  If Not fso.FolderExists(p) Then",
    "    If Not fso.FolderExists(fso.GetParentFolderName(p)) Then fso_mkdir fso.GetParentFolderName(p)",
    "    fso.CreateFolder p",
    "  End If",
    "End Sub",
    body)
}

.make_lnk <- function(sc) {
  f <- tempfile(fileext = ".vbs")
  on.exit(unlink(f))
  .write_utf16(.lnk_script(sc), f)
  cscript <- file.path(Sys.getenv("SystemRoot", "C:/Windows"), "System32",
                       "cscript.exe")
  out <- suppressWarnings(system2(cscript, c("//nologo", "//B", shQuote(f)),
                                  stdout = TRUE, stderr = TRUE))
  missing <- sc$path[!file.exists(sc$path)]
  if (length(missing)) {
    stop("Could not make the shortcut ", missing[[1L]],
         if (length(out)) paste0(": ", paste(out, collapse = " ")),
         call. = FALSE)
  }
  sc$path
}

# ------------------------------------------------------------- macOS

.mac_app_files <- function(dir, apps, lang, update = TRUE, port = NULL) {
  nm <- .shortcut_names(lang)
  sh <- file.path(dir, "tflplanner.sh")
  one <- function(name, args) {
    app <- file.path(apps, paste0(if (identical(name, nm[["update"]]))
                                    "tflplanner (update)" else name, ".app"))
    exe <- paste0(
      "#!/bin/sh\n",
      "# tflplanner launcher (tflplanner::add_shortcut()); see ", sh, "\n",
      "exec \"", sh, "\"", args,
      if (!is.null(port)) paste0(" --port=", port), "\n")
    plist <- paste0(
      '<?xml version="1.0" encoding="UTF-8"?>\n',
      '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" ',
      '"http://www.apple.com/DTDs/PropertyList-1.0.dtd">\n',
      '<plist version="1.0">\n<dict>\n',
      "  <key>CFBundleExecutable</key><string>tflplanner</string>\n",
      "  <key>CFBundleIconFile</key><string>tflplanner</string>\n",
      "  <key>CFBundleIdentifier</key><string>org.tflplanner.",
      if (nzchar(args)) "update" else "launch", "</string>\n",
      "  <key>CFBundleName</key><string>", .xml_escape(name), "</string>\n",
      "  <key>CFBundlePackageType</key><string>APPL</string>\n",
      "  <key>CFBundleShortVersionString</key><string>",
      as.character(utils::packageVersion("tflplanner")), "</string>\n",
      "  <key>LSUIElement</key><true/>\n",
      "</dict>\n</plist>\n")
    list(app = app,
         files = list(
           list(path = file.path(app, "Contents", "MacOS", "tflplanner"),
                text = exe, mode = "0755"),
           list(path = file.path(app, "Contents", "Info.plist"),
                text = plist, mode = "0644"),
           list(path = file.path(app, "Contents", "Resources",
                                 "tflplanner.icns"),
                copy = file.path(dir, "tflplanner.icns"))))
  }
  out <- list(one(nm[["launch"]], ""))
  if (update) out <- c(out, list(one(nm[["update"]], " --update")))
  out
}

.xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# ------------------------------------------------------------- Linux

.desktop_file <- function(dir, lang, update = TRUE, port = NULL) {
  # Exec: a quoted path needs no other escaping (it has no " ` $ \ in it)
  q <- function(p) paste0('"', p, '"')
  sh <- file.path(dir, "tflplanner.sh")
  extra <- if (!is.null(port)) paste0(" --port=", port) else ""
  c("[Desktop Entry]",
    "Type=Application",
    "Version=1.0",
    "Name=tflplanner",
    paste0("Comment=", tr("Start tflplanner in the browser", lang)),
    paste0("Exec=", q(sh), extra),
    paste0("Icon=", file.path(dir, "tflplanner.png")),
    "Terminal=false",
    "Categories=Office;Science;",
    if (update) c("Actions=update;", "",
                  "[Desktop Action update]",
                  paste0("Name=", tr("Update and launch", lang)),
                  paste0("Exec=", q(sh), " --update", extra)))
}

.write_text <- function(text, path, mode = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, "wb")
  writeLines(enc2utf8(text), con, sep = "\n", useBytes = TRUE)
  close(con)
  if (!is.null(mode)) Sys.chmod(path, mode)
  path
}

# ------------------------------------------------------------- what is said

# A path as the person reads it on their system.
.show_path <- function(p) if (identical(.os(), "windows")) .win_path(p) else p

# The shortcuts add_shortcut() would make, as data: one row per shortcut --
# where it goes (`place`, `folder`), what it is called (`name`), its file
# (`path`), and whether one is there already (it is then replaced).
.shortcut_plan <- function(os = .os(), dirs = .shortcut_dirs(os),
                           lang = .console_language(), desktop = TRUE,
                           start_menu = TRUE, update = TRUE,
                           dir = .launcher_dir()) {
  rows <- switch(os,
    windows = {
      sc <- .windows_shortcuts(dir, dirs, lang, desktop, start_menu, update)
      if (is.null(sc)) return(NULL)
      # compared as normalized paths: dirname() may change the separators
      np <- function(p) normalizePath(p, "/", mustWork = FALSE)
      on_desk <- np(dirname(sc$path)) == np(dirs$desktop %||% "")
      data.frame(place = ifelse(on_desk, tr("Desktop", lang),
                                tr("Start menu", lang)),
                 folder = dirname(sc$path),
                 name = sub("\\.lnk$", "", basename(sc$path)),
                 path = sc$path, stringsAsFactors = FALSE)
    },
    mac = {
      apps <- vapply(.mac_app_files(dir, dirs$applications, lang, update),
                     `[[`, "", "app")
      data.frame(place = tr("Applications", lang), folder = dirname(apps),
                 name = basename(apps), path = apps, stringsAsFactors = FALSE)
    },
    linux = {
      f <- file.path(dirs$applications, "tflplanner.desktop")
      data.frame(place = tr("the application menu", lang),
                 folder = dirs$applications,
                 name = if (update) sprintf(
                   tr("tflplanner (with the action \"%s\")", lang),
                   tr("Update and launch", lang)) else "tflplanner",
                 path = f, stringsAsFactors = FALSE)
    })
  rows$exists <- file.exists(rows$path)
  rows
}

# What add_shortcut() is about to do, in words: what, where, that one
# already there is replaced, where the launcher is kept, how to undo it.
.shortcut_plan_text <- function(plan, lang = .console_language(),
                                dir = .launcher_dir()) {
  out <- tr("tflplanner will make these shortcuts:", lang)
  for (f in unique(plan$folder)) {
    p <- plan[plan$folder == f, , drop = FALSE]
    out <- c(out, sprintf("  %s  (%s)", p$place[1L], .show_path(f)),
             paste0("    - ", p$name, ifelse(p$exists, paste0(
               "  ", tr("[already there: it will be replaced]", lang)), "")))
  }
  c(out,
    tr("A double click starts tflplanner in the browser; \"update and launch\" first updates rtfreporter, tflspec and tflplanner.", lang),
    paste(tr("They run a small launcher that tflplanner keeps in:", lang),
          .show_path(dir)),
    tr("To remove them later: remove_shortcut()", lang))
}

# The question that ends it, with how many.
.shortcut_question <- function(n, lang = .console_language()) {
  sprintf(tr("Make these %d shortcuts?", lang), n)
}

# What to do next, once they are made.
.shortcut_made_text <- function(made, os = .os(), lang = .console_language()) {
  c(sprintf(tr("Made %d shortcuts:", lang), length(made)),
    paste0("  ", .show_path(made)),
    switch(os,
      windows = tr("To pin tflplanner to the taskbar: right-click it in the Start menu and choose \"Pin to taskbar\" (Windows does not let a program do it).", lang),
      mac = tr("Open tflplanner from Finder > Applications. To keep it in the Dock, drag it there.", lang),
      linux = tr("tflplanner is in the application menu (on some desktops after you log in again).", lang)),
    tr("To remove them: remove_shortcut()", lang))
}

# ------------------------------------------------------------- the API

#' Make a shortcut that starts tflplanner
#'
#' Makes the shortcuts that start tflplanner in the browser with a double
#' click, without R or RStudio open:
#'
#' * **Windows**: "tflplanner" on the desktop and in the Start menu, and
#'   "tflplanner (update and launch)" in the Start menu.  To pin tflplanner
#'   to the taskbar, right-click it in the Start menu and choose *Pin to
#'   taskbar* (Windows does not let a program do it).
#' * **macOS**: `tflplanner.app` and `tflplanner (update).app` in
#'   `~/Applications`.
#' * **Linux**: a `tflplanner.desktop` menu entry in
#'   `~/.local/share/applications`, with an *Update and launch* action.
#'
#' A shortcut runs a small launcher that tflplanner keeps in
#' `tools::R_user_dir("tflplanner", "config")`.  The launcher finds R each
#' time it starts (on Windows from the registry entry the R installer
#' writes), so updating R does not break the shortcut, and it shows no
#' console window.  If tflplanner already runs on the port, the shortcut
#' opens it; otherwise it starts it, and closing the browser stops it.
#'
#' "Update and launch" first updates rtfreporter, tflspec and tflplanner
#' ([update_tflplanner()], on the channel last used), then starts the app.
#'
#' @param port The port the app runs on.  `NULL` keeps the setting
#'   (`setup_tflplanner(port = )`, default 7470); a number is saved as the
#'   setting.
#' @param desktop Make the desktop shortcut (Windows).
#' @param start_menu Make the Start menu entries (Windows).
#' @param update Also make "update and launch".
#' @param ask Ask before making them.  `FALSE` makes them without asking
#'   (scripts); outside an interactive session nothing is made unless
#'   `ask = FALSE`.
#' @return The shortcuts made, invisibly.
#' @seealso [remove_shortcut()], [launch_app()], [setup_tflplanner()].
#' @examples
#' \dontrun{
#' add_shortcut()
#' add_shortcut(port = 7480)
#' }
#' @export
add_shortcut <- function(port = NULL, desktop = TRUE, start_menu = TRUE,
                         update = TRUE, ask = interactive()) {
  lang <- .console_language()
  os <- .os()
  if (!is.null(port)) port <- .check_port(port)
  dirs <- .shortcut_dirs(os)
  if (isTRUE(ask)) {
    plan <- .shortcut_plan(os, dirs, lang, desktop, start_menu, update)
    if (is.null(plan) || !nrow(plan)) {
      message(tr("Nothing to make: both desktop and start_menu are FALSE.", lang))
      return(invisible(character()))
    }
    cat(.shortcut_plan_text(plan, lang), sep = "\n")
    if (!.ask_yes(.shortcut_question(nrow(plan), lang))) {
      message(tr("No shortcut was made.", lang))
      return(invisible(character()))
    }
  } else if (!identical(ask, FALSE)) {
    stop("add_shortcut() asks before it makes anything; call it in an ",
         "interactive session, or with `ask = FALSE`.", call. = FALSE)
  }
  if (!is.null(port)) {
    suppressMessages(setup_tflplanner(port = port))
  } else if (!.is_set_up()) {
    suppressMessages(setup_tflplanner(language = lang))
  }
  dir <- .write_launcher(lang = lang)
  made <- switch(os,
    windows = .make_lnk(.windows_shortcuts(dir, dirs, lang, desktop,
                                           start_menu, update)),
    mac = {
      apps <- .mac_app_files(dir, dirs$applications, lang, update)
      for (a in apps) {
        unlink(a$app, recursive = TRUE)
        for (f in a$files) {
          dir.create(dirname(f$path), recursive = TRUE, showWarnings = FALSE)
          if (!is.null(f$copy)) file.copy(f$copy, f$path, overwrite = TRUE)
          else .write_text(f$text, f$path, f$mode)
        }
      }
      vapply(apps, `[[`, "", "app")
    },
    linux = .write_text(.desktop_file(dir, lang, update),
                        file.path(dirs$applications, "tflplanner.desktop"),
                        "0755"))
  writeLines(made, file.path(dir, "shortcuts.txt"))
  message(paste(.shortcut_made_text(made, os, lang), collapse = "\n"))
  invisible(made)
}

#' @rdname add_shortcut
#' @param launcher Also remove the launcher files tflplanner keeps.
#' @export
remove_shortcut <- function(launcher = TRUE, ask = interactive()) {
  lang <- .console_language()
  dir <- .launcher_dir()
  rec <- file.path(dir, "shortcuts.txt")
  made <- if (file.exists(rec)) readLines(rec, warn = FALSE) else character()
  made <- made[file.exists(made)]
  gone <- c(made, if (launcher && dir.exists(dir)) dir)
  if (!length(gone)) {
    message(tr("There is no tflplanner shortcut to remove.", lang))
    return(invisible(character()))
  }
  if (isTRUE(ask)) {
    cat(.remove_plan_text(made, if (launcher && dir.exists(dir)) dir, lang),
        sep = "\n")
    if (!.ask_yes(tr("Remove these?", lang))) {
      message(tr("Nothing was removed.", lang))
      return(invisible(character()))
    }
  } else if (!identical(ask, FALSE)) {
    stop("remove_shortcut() asks before it removes anything; call it in ",
         "an interactive session, or with `ask = FALSE`.", call. = FALSE)
  }
  unlink(made, recursive = TRUE)
  if (identical(.os(), "windows")) {
    sm <- unique(dirname(made[grepl("\\.lnk$", made)]))
    sm <- sm[basename(sm) == "tflplanner" & dir.exists(sm)]
    for (d in sm) if (!length(list.files(d))) unlink(d, recursive = TRUE)
  }
  if (launcher) unlink(dir, recursive = TRUE)
  message(paste(c(tr("Removed:", lang), paste0("  ", .show_path(gone)),
                  tr("To make the shortcuts again: add_shortcut()", lang)),
                collapse = "\n"))
  invisible(gone)
}

# What remove_shortcut() is about to remove, and what it leaves.
.remove_plan_text <- function(made, launcher_dir = NULL,
                              lang = .console_language()) {
  c(tr("tflplanner will remove:", lang),
    if (length(made)) c(paste0("  ", tr("Shortcuts:", lang)),
                        paste0("    - ", .show_path(made))),
    if (length(launcher_dir)) c(
      paste0("  ", tr("The launcher files the shortcuts run:", lang)),
      paste0("    - ", .show_path(launcher_dir))),
    tr("Your studies, their files and tflplanner's settings are not touched.", lang),
    tr("To make the shortcuts again: add_shortcut()", lang))
}

#' Start tflplanner in its own R process
#'
#' Starts the app the way its shortcut does ([add_shortcut()]) -- in a
#' separate R process -- and opens it in the browser, so the R console (or
#' RStudio) stays free.  If the app already runs on the port, it opens that
#' one.  Closing the browser stops it.  This is the RStudio add-in "Launch
#' tflplanner".
#'
#' @param port The port; `NULL` uses the setting (default 7470).
#' @return The app's address, invisibly.
#' @seealso [run_app()] to run the app in this R session.
#' @examples
#' \dontrun{
#' launch_app()
#' }
#' @export
launch_app <- function(port = NULL) {
  port <- if (is.null(port)) .app_port() else .check_port(port)
  if (!.is_set_up()) setup_tflplanner(language = .console_language())
  dir <- .write_launcher()
  rscript <- file.path(R.home("bin"),
                       if (identical(.os(), "windows")) "Rscript.exe" else "Rscript")
  processx::process$new(rscript, c(file.path(dir, "launch.R"),
                                   paste0("--port=", port)),
                        stdout = NULL, stderr = NULL, cleanup = FALSE)
  url <- sprintf("http://127.0.0.1:%d/", port)
  message(tr("tflplanner is starting at", .console_language()), " ", url)
  invisible(url)
}
