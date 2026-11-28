# Starting tflplanner from a shortcut.
#
# add_shortcut() keeps a copy of the launcher (inst/launcher: launch.R,
# update.R, the OS script and an icon) in
# tools::R_user_dir("tflplanner", "config")/launcher-R<x.y>, and points the
# shortcuts at that copy -- not at the package library, which moves with
# every R version.  Each R x.y has its own icon ("tflplanner (R 4.6)") and
# its own folder, and the folder's launcher.conf remembers the R it was made
# in (r_home, r_xy).  The OS script looks for R at every start and never
# falls back to another x.y (.pick_r() is the rule, in R, for the tests;
# tflplanner.vbs and tflplanner.sh do the same):
#   1. the remembered R, when its Rscript is there;
#   2. the newest R of the same x.y in the usual places;
#   3. Rscript on PATH, only when its x.y matches;
#   4. else it stops and says so (.r_not_found_text()).
#
#   Windows  desktop and Start menu .lnk -> wscript.exe tflplanner.vbs
#            (no console window; the usual places: R-x.y.* under
#            Program Files\R and %LOCALAPPDATA%\Programs\R)
#   macOS    ~/Applications/tflplanner (R 4.6).app and "... (update).app",
#            each a shell script in an application bundle
#   Linux    ~/.local/share/applications/tflplanner-R4.6.desktop, with an
#            "Update and launch" action
#
# The app never updates itself: a session cannot replace a package it has
# loaded.  The "update and launch" shortcut updates in another R process
# first (update.R), then starts the app.

.default_port <- 7470L

# The R x.y a launcher is made for: "4.6" for R 4.6.1.
.r_xy <- function(v = R.version) {
  paste0(v$major, ".", sub("\\..*$", "", v$minor))
}

# The icon's name for an R x.y: "tflplanner (R 4.6)".
.shortcut_base <- function(xy = .r_xy()) sprintf("tflplanner (R %s)", xy)

# The launcher folder of an R x.y: next to the home pointer, so launch.R
# finds the home one folder up.
.launcher_dir <- function(xy = .r_xy()) {
  file.path(tools::R_user_dir("tflplanner", "config"), paste0("launcher-R", xy))
}

# The one folder every launcher had before the x.y: replaced by add_shortcut().
.legacy_launcher_dir <- function() {
  file.path(tools::R_user_dir("tflplanner", "config"), "launcher")
}

# ------------------------------------------------------------- which R

# Where the launcher looks for R: ONE rule for the Windows and the mac / linux
# scripts (tflplanner.vbs, tflplanner.sh).  The scripts cannot call R -- R is
# what they look for -- so each does the same in its own language, and the
# tests (tests/testthat/test-launcher.R) run one table of cases through this
# function, the .sh and the VBScript (`--which` prints the pick and starts
# nothing; TFLPLANNER_R_ROOTS / TFLPLANNER_R_SEARCH replace the usual places).
# Never another x.y; every R picked has its x.y checked, the remembered one
# too (macOS R.framework/Resources and Linux /usr/lib/R are the *current* R,
# whichever x.y that is today).
#   1. the remembered R (`remembered`, an R home), when its Rscript is there
#      and its x.y is `xy`;
#   2. the newest R-x.y.* folder of `xy` in `folders` (the usual places and
#      the remembered R's parent folder) that has its Rscript; equal
#      versions: the first found;
#   3. the first Rscript of `path` (every PATH entry, in order) whose x.y is
#      `xy`;
#   4. none: NULL.
# `rscript_of(folder)` is the Rscript inside an R home; `exists()` says
# whether a file is there; `xy_of(rscript)` its x.y (the script takes it from
# the path where the path says, else asks the Rscript for its version).
# Returns list(rscript, from) with from = "remembered", "folder" or "path".
.pick_r <- function(xy, remembered = NULL, folders = character(),
                    path = character(), xy_of = .xy_of_path,
                    rscript_of = function(f) file.path(f, "bin", "Rscript.exe"),
                    exists = file.exists) {
  if (length(remembered) == 1L && !is.na(remembered) && nzchar(remembered)) {
    rs <- rscript_of(remembered)
    if (exists(rs) && identical(unname(xy_of(rs)), xy)) {
      return(list(rscript = rs, from = "remembered"))
    }
  }
  v <- .r_folder_version(folders)
  keep <- !is.na(v) & .version_xy(v) == xy
  keep[keep] <- vapply(folders[keep], function(f) isTRUE(exists(rscript_of(f))), NA)
  if (any(keep)) {
    best <- which(keep)[order(package_version(v[keep]), decreasing = TRUE)[1L]]
    return(list(rscript = rscript_of(folders[[best]]), from = "folder"))
  }
  for (p in path) {
    if (!is.na(p) && nzchar(p) && exists(p) && identical(unname(xy_of(p)), xy)) {
      return(list(rscript = p, from = "path"))
    }
  }
  NULL
}

# The x.y an Rscript's path says: "4.6" for ".../R-4.6.1/bin/Rscript.exe"
# (also R.framework/Versions/4.6[-arm64]/Resources/bin and /opt/R/4.6.1/bin);
# NA when the path does not say (R.framework/Resources, /usr/lib/R).
.xy_of_path <- function(rscript) {
  home <- dirname(dirname(rscript))
  cand <- c(.r_folder_version(home), basename(home),
            if (identical(basename(dirname(dirname(home))), "Versions")) basename(dirname(home)))
  cand <- sub("-.*$", "", cand[!is.na(cand)])
  cand <- cand[grepl("^[0-9]+[.][0-9]+", cand)]
  if (!length(cand)) return(NA_character_)
  .version_xy(cand[[1L]])
}

# "4.6.1" from the folder "R-4.6.1" (also "R-4.6.1patched"); NA when the
# name is not an R folder's.
.r_folder_version <- function(folders) {
  m <- regmatches(basename(folders),
                  regexec("^R-([0-9]+(\\.[0-9]+)+)", basename(folders),
                          ignore.case = TRUE))
  vapply(m, function(x) if (length(x)) x[[2L]] else NA_character_, "")
}

# "4.6" from "4.6.1".
.version_xy <- function(v) {
  vapply(strsplit(v, ".", fixed = TRUE),
         function(p) if (length(p) >= 2L) paste(p[1:2], collapse = ".") else NA_character_,
         "")
}

# What the launcher says when no R of that x.y is found, in the language the
# launcher's messages use.
.r_not_found_text <- function(xy, lang = .console_language()) {
  sprintf(tr("R %s is not found. Install tflplanner in the R you will use, and run add_shortcut() in that R.", lang), xy)
}

# The launcher's settings file: the R it was made in (key=value lines).
.launcher_conf_text <- function(r_home, xy) {
  c(paste0("r_home=", r_home), paste0("r_xy=", xy))
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
             iconv(enc2utf8(paste(text, collapse = "\r\n")), "UTF-8", "UTF-16LE",
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
.write_launcher <- function(dir = .launcher_dir(xy),
                            lang = .console_language(), xy = .r_xy(),
                            r_home = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (f in c("launch.R", "update.R", "tflplanner.png", "tflplanner.ico",
              "tflplanner.icns")) {
    file.copy(.launcher_src(f), file.path(dir, f), overwrite = TRUE)
  }
  # the R this launcher is for: only add_shortcut() says it (the R it runs
  # in); a refresh, or launch_app(), keeps the one on record for this x.y
  # (and records this R when there is none)
  if (is.null(r_home)) {
    old <- .read_launcher_conf(dir)
    r_home <- if (identical(old$r_xy, xy) && length(old$r_home) && nzchar(old$r_home)) {
      old$r_home
    } else R.home()
  }
  # UTF-16 for the VBScript (a path may hold Japanese), UTF-8 for the shell
  conf <- .launcher_conf_text(.show_path(normalizePath(r_home, "/", mustWork = FALSE)), xy)
  if (identical(.os(), "windows")) {
    .write_utf16(conf, file.path(dir, "launcher.conf"))
  } else {
    .write_text(conf, file.path(dir, "launcher.conf"))
  }
  no_r <- .r_not_found_text(xy, lang)
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

# The settings file as a list (r_home, r_xy); empty when there is none.
# UTF-16 (Windows, the VBScript reads it) or UTF-8.
.read_launcher_conf <- function(dir) {
  f <- file.path(dir, "launcher.conf")
  if (!file.exists(f)) return(list())
  raw <- readBin(f, "raw", file.size(f))
  txt <- if (length(raw) >= 2L && identical(raw[1:2], as.raw(c(0xFF, 0xFE)))) {
    iconv(list(raw[-(1:2)]), "UTF-16LE", "UTF-8")
  } else {
    x <- rawToChar(raw)
    Encoding(x) <- "UTF-8"
    x
  }
  l <- strsplit(txt, "\r?\n")[[1L]]
  m <- regmatches(l, regexec("^([a-z_]+)=(.*)$", l))
  m <- m[lengths(m) == 3L]
  stats::setNames(lapply(m, function(x) x[[3L]]), vapply(m, `[`, "", 2L))
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

.shortcut_names <- function(lang, xy = .r_xy()) {
  c(launch = .shortcut_base(xy),
    update = paste0(.shortcut_base(xy), " - ", tr("Update and launch", lang)))
}

# The icons of the version before the R x.y (one "tflplanner", one launcher
# folder), found by name: add_shortcut() removes them -- the new icon
# replaces them.  The folder's shortcuts.txt names any other place.
.legacy_shortcuts <- function(os = .os(), dirs = .shortcut_dirs(os),
                              lang = .console_language(),
                              dir = .legacy_launcher_dir()) {
  found <- switch(os,
    windows = c(file.path(dirs$desktop, "tflplanner.lnk"),
                file.path(dirs$start_menu,
                          paste0(c("tflplanner", "tflplanner (update and launch)",
                                   tr("tflplanner (update and launch)", lang)), ".lnk"))),
    mac = file.path(dirs$applications, c("tflplanner.app", "tflplanner (update).app")),
    linux = file.path(dirs$applications, "tflplanner.desktop"))
  rec <- file.path(dir, "shortcuts.txt")
  if (file.exists(rec)) {
    # only what looks like tflplanner's own icons: a path in the record may
    # have been changed by hand
    r <- readLines(rec, warn = FALSE)
    found <- c(found, r[grepl("^tflplanner.*\\.(lnk|app|desktop)$", basename(r))])
  }
  unique(found[file.exists(found)])
}

# The old launcher folder is replaced by the one of the R x.y.  On Windows a
# pinned old icon (taskbar, Start) still points at its tflplanner.vbs: that
# file stays, as a stub that says which shortcut to use (stub.vbs), with the
# icon file; everything else in the folder goes.  Elsewhere the old icons
# were files that add_shortcut() has removed, so the folder goes.
.replace_legacy_launcher <- function(os = .os(), xy = .r_xy(),
                                     lang = .console_language(),
                                     dir = .legacy_launcher_dir()) {
  if (!dir.exists(dir)) return(invisible(FALSE))
  if (!identical(os, "windows")) {
    unlink(dir, recursive = TRUE)
    return(invisible(TRUE))
  }
  keep <- c("tflplanner.ico", "tflplanner.vbs")
  unlink(setdiff(list.files(dir, all.files = TRUE, no.. = TRUE, full.names = TRUE),
                 file.path(dir, keep)), recursive = TRUE)
  msg <- sprintf(tr("This shortcut is out of date. Use \"%s\", made by add_shortcut().", lang),
                 .shortcut_base(xy))
  vbs <- readLines(.launcher_src("stub.vbs"), warn = FALSE)
  .write_utf16(gsub("{{MSG}}", gsub('"', '""', msg, fixed = TRUE), vbs, fixed = TRUE),
               file.path(dir, "tflplanner.vbs"))
  invisible(TRUE)
}

# The old launcher folder (before the R x.y), when it is all there is: no
# launcher of an x.y beside it.  The app says to run add_shortcut().
.legacy_only <- function(dir = .legacy_launcher_dir(),
                         config = dirname(dir)) {
  file.exists(file.path(dir, "VERSION")) &&
    !length(list.files(config, "^launcher-R"))
}

# The launcher folders of every R x.y: x.y -> folder.
.launcher_dirs <- function(config = tools::R_user_dir("tflplanner", "config")) {
  d <- list.files(config, "^launcher-R[0-9]", full.names = TRUE)
  stats::setNames(d, sub("^launcher-R", "", basename(d)))
}

# The x.y of the R's that are installed (from where they are kept), for the
# launchers whose R has gone.
.installed_xys <- function(os = .os()) {
  homes <- switch(os,
    windows = list.dirs(c(file.path(Sys.getenv("ProgramFiles"), "R"),
                          file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "R")),
                        recursive = FALSE),
    mac = list.dirs("/Library/Frameworks/R.framework/Versions", recursive = FALSE),
    list.dirs("/opt/R", recursive = FALSE))
  nm <- sub("^R-", "", basename(homes))
  nm <- nm[grepl("^[0-9]+[.][0-9]+", nm)]
  unique(.version_xy(sub("-.*$", "", nm)))
}

# The launchers of other R x.y's whose R is not there any more: x.y ->
# their icons.  A launcher is dead when its remembered Rscript is gone and no
# R of its x.y is installed (`installed`, the x.y's that are).  A launcher
# that remembers no R (no launcher.conf, or no r_home in it) has no Rscript:
# on every system, not only where "/bin/Rscript" happens to be there.
.dead_launchers <- function(dirs = .launcher_dirs(), xy = .r_xy(),
                            installed = .installed_xys(), os = .os()) {
  exe <- if (identical(os, "windows")) "Rscript.exe" else "Rscript"
  out <- list()
  for (k in setdiff(names(dirs), xy)) {
    home <- .read_launcher_conf(dirs[[k]])$r_home %||% ""
    gone <- !nzchar(home) || !file.exists(file.path(home, "bin", exe))
    if (gone && !k %in% installed) {
      rec <- file.path(dirs[[k]], "shortcuts.txt")
      out[[k]] <- if (file.exists(rec)) readLines(rec, warn = FALSE) else character()
    }
  }
  out
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
                               start_menu = TRUE, update = FALSE,
                               port = NULL, xy = .r_xy()) {
  nm <- .shortcut_names(lang, xy)
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

# The PowerShell that makes one .lnk (WScript.Shell through COM), for
# when the VBScript could not: security software may stop a script from
# writing a shortcut to the desktop without a word (Trend Micro's, seen in
# 2026-10), while PowerShell gets through -- and PowerShell may be blocked
# by policy, so it is the second way, not the first.
.lnk_ps <- function(r) {
  q <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  win <- .win_path
  c("$ErrorActionPreference = 'Stop'",
    "$sh = New-Object -ComObject WScript.Shell",
    sprintf("New-Item -ItemType Directory -Force -Path %s | Out-Null", q(win(dirname(r$path)))),
    sprintf("$l = $sh.CreateShortcut(%s)", q(win(r$path))),
    sprintf("$l.TargetPath = %s", q(win(r$target))),
    sprintf("$l.Arguments = %s", q(r$args)),
    sprintf("$l.IconLocation = %s", q(paste0(r$icon, ",0"))),
    sprintf("$l.WorkingDirectory = %s", q(r$workdir)),
    sprintf("$l.Description = %s", q(r$description)),
    "$l.WindowStyle = 1",
    "$l.Save()")
}

# One .lnk by the VBScript (cscript), and by PowerShell: what each said
.lnk_by_vbs <- function(r) {
  f <- tempfile(fileext = ".vbs")
  on.exit(unlink(f))
  .write_utf16(.lnk_script(r), f)
  cscript <- file.path(Sys.getenv("SystemRoot", "C:/Windows"), "System32",
                       "cscript.exe")
  suppressWarnings(system2(cscript, c("//nologo", "//B", shQuote(f)),
                           stdout = TRUE, stderr = TRUE))
}
.lnk_by_ps <- function(r) {
  # -EncodedCommand: UTF-16LE in base64, so no quoting and no script file
  # (which an execution policy could refuse)
  cmd <- iconv(paste(.lnk_ps(r), collapse = "\n"), "UTF-8", "UTF-16LE",
               toRaw = TRUE)[[1L]]
  ps <- file.path(Sys.getenv("SystemRoot", "C:/Windows"), "System32",
                  "WindowsPowerShell", "v1.0", "powershell.exe")
  suppressWarnings(system2(ps, c("-NoProfile", "-NonInteractive", "-EncodedCommand",
                                 gsub("[[:space:]]", "", jsonlite::base64_enc(cmd))),
                           stdout = TRUE, stderr = TRUE))
}

# The .lnk files of `sc`, one at a time (one stopped does not stop the
# others): by the VBScript, else by PowerShell; an error names those
# neither could make, and how to make one by hand
.make_lnk <- function(sc, vbs = .lnk_by_vbs, ps = .lnk_by_ps) {
  said <- character()
  for (i in seq_len(nrow(sc))) {
    r <- sc[i, , drop = FALSE]
    out <- tryCatch(vbs(r), error = function(e) conditionMessage(e))
    if (!file.exists(r$path)) {
      out <- c(out, tryCatch(ps(r), error = function(e) conditionMessage(e)))
    }
    if (!file.exists(r$path)) said <- c(said, out[nzchar(out)])
  }
  missing <- sc[!file.exists(sc$path), , drop = FALSE]
  if (nrow(missing)) {
    m <- missing[1L, ]
    stop("Could not make the shortcut ", m$path,
         if (nrow(missing) > 1L) paste0(" (and ", nrow(missing) - 1L, " more)"),
         if (length(said)) paste0(": ", paste(unique(said), collapse = " ")), ".\n",
         "Security software (an antivirus program's folder protection) may have ",
         "stopped the scripts that write it, without a message: allow ",
         "cscript.exe and powershell.exe to write there and try again, or make ",
         "it by hand -- right-click the folder > New > Shortcut, the location ",
         .win_path(m$target), " ", m$args, ", then in its Properties start in ",
         m$workdir, ".", call. = FALSE)
  }
  sc$path
}

# ------------------------------------------------------------- macOS

.mac_app_files <- function(dir, apps, lang, update = FALSE, port = NULL,
                           xy = .r_xy()) {
  nm <- .shortcut_names(lang, xy)
  sh <- file.path(dir, "tflplanner.sh")
  one <- function(name, args) {
    app <- file.path(apps, paste0(if (identical(name, nm[["update"]]))
                                    paste0(nm[["launch"]], " (update)") else name,
                                  ".app"))
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
      if (nzchar(args)) "update" else "launch", ".R",
      gsub(".", "-", xy, fixed = TRUE), "</string>\n",
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

# The menu entry's file name: one per R x.y.
.desktop_name <- function(xy = .r_xy()) paste0("tflplanner-R", xy, ".desktop")

.desktop_file <- function(dir, lang, update = FALSE, port = NULL,
                          xy = .r_xy()) {
  # Exec: a quoted path needs no other escaping (it has no " ` $ \ in it)
  q <- function(p) paste0('"', p, '"')
  sh <- file.path(dir, "tflplanner.sh")
  extra <- if (!is.null(port)) paste0(" --port=", port) else ""
  c("[Desktop Entry]",
    "Type=Application",
    "Version=1.0",
    paste0("Name=", .shortcut_base(xy)),
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
                           start_menu = TRUE, update = FALSE,
                           xy = .r_xy(), dir = .launcher_dir(xy)) {
  rows <- switch(os,
    windows = {
      sc <- .windows_shortcuts(dir, dirs, lang, desktop, start_menu, update,
                               xy = xy)
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
      apps <- vapply(.mac_app_files(dir, dirs$applications, lang, update, xy = xy),
                     `[[`, "", "app")
      data.frame(place = tr("Applications", lang), folder = dirname(apps),
                 name = basename(apps), path = apps, stringsAsFactors = FALSE)
    },
    linux = {
      f <- file.path(dirs$applications, .desktop_name(xy))
      data.frame(place = tr("the application menu", lang),
                 folder = dirs$applications,
                 name = if (update) sub("^tflplanner", .shortcut_base(xy), sprintf(
                   tr("tflplanner (with the action \"%s\")", lang),
                   tr("Update and launch", lang))) else .shortcut_base(xy),
                 path = f, stringsAsFactors = FALSE)
    })
  rows$exists <- file.exists(rows$path)
  rows
}

# What add_shortcut() is about to do, in words: what, where, that one
# already there is replaced, where the launcher is kept, how to undo it.
.shortcut_plan_text <- function(plan, lang = .console_language(),
                                dir = .launcher_dir(), legacy = character(),
                                dead = list()) {
  out <- sprintf(tr("tflplanner will make these shortcuts, for R %s:", lang),
                 .r_xy())
  for (f in unique(plan$folder)) {
    p <- plan[plan$folder == f, , drop = FALSE]
    out <- c(out, sprintf("  %s  (%s)", p$place[1L], .show_path(f)),
             paste0("    - ", p$name, ifelse(p$exists, paste0(
               "  ", tr("[already there: it will be replaced]", lang)), "")))
  }
  if (length(legacy)) {
    out <- c(out, tr("The shortcuts made before the R version was in the name are removed:", lang),
             paste0("    - ", .show_path(legacy)))
  }
  for (k in names(dead)) {
    out <- c(out, sprintf(tr("The shortcuts of R %s: that R is not installed any more (remove_shortcut(xy = \"%s\") removes them):", lang), k, k),
             if (length(dead[[k]])) paste0("    - ", .show_path(dead[[k]])))
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
#' The shortcut is named for the R it is made in: in R 4.6.1 it is
#' "tflplanner (R 4.6)".  Each R `x.y` has its own shortcut and launcher
#' folder, so several can stand side by side; running `add_shortcut()` again
#' in the same `x.y` replaces that one.  The shortcut named just "tflplanner"
#' (made before the R version was in the name) is replaced.
#'
#' * **Windows**: "tflplanner (R 4.6)" on the desktop and in the Start menu,
#'   and "tflplanner (R 4.6) - Update and launch" in the Start menu.  To pin
#'   tflplanner to the taskbar, right-click it in the Start menu and choose
#'   *Pin to taskbar* (Windows does not let a program do it).
#' * **macOS**: `tflplanner (R 4.6).app` and `tflplanner (R 4.6) (update).app`
#'   in `~/Applications`.
#' * **Linux**: a `tflplanner-R4.6.desktop` menu entry in
#'   `~/.local/share/applications`, with an *Update and launch* action.
#'
#' A shortcut runs a small launcher that tflplanner keeps in
#' `tools::R_user_dir("tflplanner", "config")`, in a folder for the R `x.y`
#' (`launcher-R4.6`), with a `launcher.conf` that remembers the R it was made
#' in.  The launcher looks for R each time it starts, and never uses another
#' `x.y`: first the remembered R (when it is that `x.y`); then the newest R of the same `x.y` (on
#' Windows `R-x.y.*` under `C:\Program Files\R`, `%LOCALAPPDATA%\Programs\R` and beside the remembered R;
#' on macOS and Linux the usual install places);
#' then the first `Rscript` on the `PATH` of that `x.y`.  When there is none,
#' it says so: install tflplanner in the R you will use and run
#' `add_shortcut()` in that R.  So updating R within an `x.y`, or removing
#' an older R, does not break the shortcut, and it shows no console window.
#' It starts the app, and closing the browser stops it.
#' When the port is taken:
#'
#' * by this version of tflplanner, it opens that one;
#' * by another version, or a tflplanner that does not answer, it asks
#'   whether to stop that one and start again (an official run goes on by
#'   itself: [run_batch()]);
#' * by another program, it starts on the next free port.
#'
#' Each start is noted in the launcher's `launcher.log`.
#' `remove_shortcut()` removes the shortcut of the R it runs in.
#'
#' "Update and launch" first updates rtfreporter, tflspec and tflplanner
#' ([update_tflplanner()], on the channel last used), then starts the app;
#' while an official run goes on it does not update (the run has loaded
#' the packages) and starts the app as it is.
#'
#' On Windows the shortcuts are written by a VBScript, and one it could not
#' write again by PowerShell: security software may stop a script from
#' writing to the desktop without a message.  If neither can, the error
#' says how to allow it or to make the shortcut by hand.
#'
#' @param port The port the app runs on.  `NULL` keeps the setting
#'   (`setup_tflplanner(port = )`, default 7470); a number is saved as the
#'   setting.
#' @param desktop Make the desktop shortcut (Windows).
#' @param start_menu Make the Start menu entries (Windows).
#' @param update Also make "update and launch" (`FALSE` by default:
#'   tflplanner is never updated unless you ask -- with this shortcut, made
#'   when `update = TRUE`, or [update_tflplanner()]).
#' @param ask Ask before making them.  `FALSE` makes them without asking
#'   (scripts); outside an interactive session nothing is made unless
#'   `ask = FALSE`.
#' @return The shortcuts made, invisibly.
#' @seealso [remove_shortcut()], [launch_app()], [setup_tflplanner()].
#' @examples
#' \dontrun{
#' # not run: writes a shortcut on your desktop and in the start menu
#' add_shortcut()
#' add_shortcut(port = 7480)
#' }
#' @export
add_shortcut <- function(port = NULL, desktop = TRUE, start_menu = TRUE,
                         update = FALSE, ask = interactive()) {
  lang <- .console_language()
  os <- .os()
  if (!is.null(port)) port <- .check_port(port)
  dirs <- .shortcut_dirs(os)
  xy <- .r_xy()
  legacy <- .legacy_shortcuts(os, dirs, lang)
  if (isTRUE(ask)) {
    plan <- .shortcut_plan(os, dirs, lang, desktop, start_menu, update, xy)
    if (is.null(plan) || !nrow(plan)) {
      message(tr("Nothing to make: both desktop and start_menu are FALSE.", lang))
      return(invisible(character()))
    }
    cat(.shortcut_plan_text(plan, lang, legacy = legacy, dead = .dead_launchers(xy = xy)),
        sep = "\n")
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
  # the R this runs in is the one the launcher remembers
  dir <- .write_launcher(lang = lang, xy = xy, r_home = R.home())
  made <- switch(os,
    windows = .make_lnk(.windows_shortcuts(dir, dirs, lang, desktop,
                                           start_menu, update, xy = xy)),
    mac = {
      apps <- .mac_app_files(dir, dirs$applications, lang, update, xy = xy)
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
    linux = .write_text(.desktop_file(dir, lang, update, xy = xy),
                        file.path(dirs$applications, .desktop_name(xy)),
                        "0755"))
  writeLines(made, file.path(dir, "shortcuts.txt"))
  # the icon made before the R version was in the name is replaced: removed,
  # and said (even when nothing was asked)
  gone <- setdiff(legacy, made)
  if (length(gone)) {
    unlink(gone, recursive = TRUE)
    message(paste(c(tr("Removed the shortcuts made before the R version was in the name:", lang),
                    paste0("  ", .show_path(gone))), collapse = "\n"))
  }
  .replace_legacy_launcher(os, xy, lang)
  message(paste(.shortcut_made_text(made, os, lang), collapse = "\n"))
  invisible(made)
}

#' @rdname add_shortcut
#' @param launcher Also remove the launcher files tflplanner keeps.
#' @param xy The R version whose shortcut to remove, like `"4.4"` (for an R
#'   that is not installed any more); `NULL` is the R this runs in, and
#'   `"all"` every R's.
#' @export
remove_shortcut <- function(launcher = TRUE, ask = interactive(), xy = NULL) {
  lang <- .console_language()
  dirs <- if (identical(xy, "all")) {
    unique(c(.launcher_dir(), unname(.launcher_dirs())))
  } else .launcher_dir(xy %||% .r_xy())
  made <- as.character(unlist(lapply(file.path(dirs, "shortcuts.txt"), function(rec) {
    if (file.exists(rec)) readLines(rec, warn = FALSE)
  })))
  made <- made[file.exists(made)]
  # the icons of before the R version was in the name go with it
  made <- unique(c(made, .legacy_shortcuts(lang = lang)))
  dirs <- c(dirs, .legacy_launcher_dir())
  dirs <- dirs[dir.exists(dirs)]
  dir <- dirs
  gone <- c(made, if (launcher) dir)
  if (!length(gone)) {
    message(tr("There is no tflplanner shortcut to remove.", lang))
    return(invisible(character()))
  }
  if (isTRUE(ask)) {
    cat(.remove_plan_text(made, if (launcher && length(dir)) dir, lang),
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
#' one (another version: it asks whether to stop it; another program: the
#' next free port, see [add_shortcut()]).  Closing the browser stops it.
#' This is the RStudio add-in "Launch tflplanner".
#'
#' @param port The port; `NULL` uses the setting (default 7470).
#' @return The app's address, invisibly.
#' @seealso [run_app()] to run the app in this R session.
#' @examples
#' if (interactive()) {
#'   launch_app()
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
