# Setup, update and launch (#40).  Everything is written to temporary
# folders: the launcher's config folder (R_USER_CONFIG_DIR) and the
# shortcut folders (option tflplanner.shortcut_dirs).

local_launcher <- function(env = parent.frame()) {
  home <- local_home(env)
  # the messages in English whatever the machine's locale
  suppressMessages(setup_tflplanner(language = "en"))
  cfg <- withr_tempdir(env)
  old_env <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
  Sys.setenv(R_USER_CONFIG_DIR = cfg)
  sc <- file.path(withr_tempdir(env), "shortcuts")
  old_opt <- options(tflplanner.shortcut_dirs = list(
    desktop = file.path(sc, "Desktop"),
    start_menu = file.path(sc, "Programs", "tflplanner"),
    applications = file.path(sc, "applications")))
  do.call(on.exit, list(substitute({
    if (is.na(old_env)) Sys.unsetenv("R_USER_CONFIG_DIR")
    else Sys.setenv(R_USER_CONFIG_DIR = old_env)
    options(old_opt)
  }), add = TRUE), envir = env)
  list(home = home, shortcuts = sc)
}

# ------------------------------------------------------------- the scripts

test_that("the launcher scripts parse and use base R only before the update", {
  for (f in c("launch.R", "update.R")) {
    expect_no_error(parse(system.file("launcher", f, package = "tflplanner"),
                          encoding = "UTF-8"))
  }
  # nothing in launch.R loads tflplanner before the update has run
  l <- readLines(system.file("launcher", "launch.R", package = "tflplanner"),
                 encoding = "UTF-8")
  l[grepl("^[[:space:]]*#", l)] <- ""         # code only
  first <- grep("tflplanner::", l)[1L]
  upd <- grep("update.R", l, fixed = TRUE)
  expect_true(all(upd < first))
  expect_false(any(grepl("library\\(tflplanner\\)", l)))
})

test_that(".write_launcher() writes the launcher, localized, beside the home pointer", {
  local_launcher()
  dir <- .write_launcher(lang = "ja")
  expect_identical(dir, .launcher_dir())
  expect_true(all(file.exists(file.path(dir, c(
    "launch.R", "update.R", "tflplanner.vbs", "tflplanner.sh",
    "tflplanner.ico", "tflplanner.icns", "tflplanner.png", "VERSION")))))
  # launch.R finds the home through the pointer file one folder up -- the
  # one setup_tflplanner() writes
  expect_identical(dirname(dir), dirname(.pointer_file()))
  # the VBScript is UTF-16 with a BOM (wscript reads that), message filled in
  raw <- readBin(file.path(dir, "tflplanner.vbs"), "raw", 1e5)
  expect_identical(raw[1:2], as.raw(c(0xFF, 0xFE)))
  vbs <- iconv(list(raw[-(1:2)]), "UTF-16LE", "UTF-8")
  expect_false(grepl("{{NO_R}}", vbs, fixed = TRUE))
  expect_match(vbs, tr("R was not found. Install R, then try again.", "ja"),
               fixed = TRUE)
  expect_match(vbs, "SOFTWARE\\R-core\\R\\InstallPath", fixed = TRUE)
  expect_match(vbs, 'Q(here & "\\launch.R") & args, 0, False', fixed = TRUE)
  sh <- readLines(file.path(dir, "tflplanner.sh"), encoding = "UTF-8")
  expect_identical(sh[[1L]], "#!/bin/sh")
  expect_false(any(grepl("{{NO_R}}", sh, fixed = TRUE)))
  expect_true(any(grepl("R.framework/Resources/bin/Rscript", sh, fixed = TRUE)))
  if (.Platform$OS.type != "windows") {
    expect_true(file.access(file.path(dir, "tflplanner.sh"), 1L) == 0L)
  }
  # the icons carry the PNG
  ico <- readBin(file.path(dir, "tflplanner.ico"), "raw", 1e5)
  expect_identical(ico[1:4], as.raw(c(0, 0, 1, 0)))
  icns <- readBin(file.path(dir, "tflplanner.icns"), "raw", 1e5)
  expect_identical(rawToChar(icns[1:4]), "icns")
  expect_identical(rawToChar(icns[9:12]), "ic08")
})

test_that("the launcher is refreshed when tflplanner's version changes", {
  local_launcher()
  dir <- .write_launcher()
  expect_false(.refresh_launcher(dir))
  writeLines("0.0.0", file.path(dir, "VERSION"))
  unlink(file.path(dir, "launch.R"))
  expect_true(.refresh_launcher(dir))
  expect_true(file.exists(file.path(dir, "launch.R")))
  # no launcher, nothing written
  expect_false(.refresh_launcher(file.path(tempdir(), "no-launcher")))
})

# ------------------------------------------------------------- Windows

test_that("the Windows shortcuts: desktop and Start menu, wscript + the VBScript", {
  dirs <- list(desktop = "C:/Users/u/Desktop",
               start_menu = "C:/Users/u/Start Menu/Programs/tflplanner")
  sc <- .windows_shortcuts("C:/cfg/launcher", dirs, "en")
  expect_identical(basename(sc$path),
                   c("tflplanner.lnk", "tflplanner.lnk",
                     "tflplanner (update and launch).lnk"))
  expect_identical(dirname(sc$path), c(dirs$desktop, dirs$start_menu,
                                       dirs$start_menu))
  expect_true(all(grepl("wscript.exe$", sc$target)))
  expect_true(all(startsWith(sc$args, '"C:\\cfg\\launcher\\tflplanner.vbs"')))
  expect_identical(endsWith(sc$args, "--update"), c(FALSE, FALSE, TRUE))
  expect_true(all(grepl("tflplanner.ico$", sc$icon)))
  # Japanese names
  sc_ja <- .windows_shortcuts("C:/cfg/launcher", dirs, "ja", desktop = FALSE)
  expect_identical(basename(sc_ja$path),
                   c("tflplanner.lnk", paste0(tr("tflplanner (update and launch)", "ja"), ".lnk")))
  # only the desktop, without update
  one <- .windows_shortcuts("C:/x", dirs, "en", start_menu = FALSE, update = FALSE)
  expect_identical(nrow(one), 1L)
  # the script that makes them
  s <- .lnk_script(sc)
  expect_true(any(grepl('CreateObject("WScript.Shell")', s, fixed = TRUE)))
  expect_identical(sum(grepl("^l.Save$", s)), 3L)
  expect_true(any(grepl('l.Arguments = """C:\\cfg\\launcher\\tflplanner.vbs"" --update"',
                        s, fixed = TRUE)))
})

test_that("add_shortcut() / remove_shortcut() on Windows make and remove real .lnk files", {
  skip_on_cran()
  skip_if_not(.Platform$OS.type == "windows")
  p <- local_launcher()
  made <- suppressMessages(add_shortcut(ask = FALSE))
  expect_length(made, 3L)
  expect_true(all(file.exists(made)))
  expect_true(all(file.size(made) > 0))
  expect_identical(readLines(file.path(.launcher_dir(), "shortcuts.txt")), made)
  gone <- remove_shortcut(ask = FALSE)
  expect_false(any(file.exists(made)))
  expect_false(dir.exists(.launcher_dir()))
  expect_false(dir.exists(file.path(p$shortcuts, "Programs", "tflplanner")))
  expect_message(remove_shortcut(ask = FALSE), "no tflplanner shortcut")
})

test_that("the Windows launcher finds R and runs launch.R hidden", {
  skip_on_cran()
  skip_if_not(.Platform$OS.type == "windows")
  # the VBScript's own way of finding R, run for real: print what it found
  local_launcher()
  dir <- .write_launcher()
  vbs <- iconv(list(readBin(file.path(dir, "tflplanner.vbs"), "raw", 1e5)[-(1:2)]),
               "UTF-16LE", "UTF-8")
  probe <- sub("(?s)rscript = FindRscript\\(\\).*?WScript.Quit 1\\r?\\nEnd If",
               "WScript.Echo FindRscript()\nWScript.Quit 0", vbs, perl = TRUE)
  probe <- sub("sh.Run Q\\(rscript\\)[^\n]*", "", probe)
  f <- tempfile(fileext = ".vbs")
  .write_utf16(probe, f)
  out <- system2(file.path(Sys.getenv("SystemRoot"), "System32", "cscript.exe"),
                 c("//nologo", shQuote(f)), stdout = TRUE)
  expect_match(out[[length(out)]], "Rscript.exe$")
  expect_true(file.exists(out[[length(out)]]))
})

# ------------------------------------------------------------- macOS

test_that("the macOS apps: a bundle whose executable runs the launcher", {
  apps <- .mac_app_files("/cfg/launcher", "/Users/u/Applications", "en")
  expect_identical(basename(vapply(apps, `[[`, "", "app")),
                   c("tflplanner.app", "tflplanner (update).app"))
  a <- apps[[2L]]
  paths <- vapply(a$files, `[[`, "", "path")
  expect_true(all(c("Contents/MacOS/tflplanner", "Contents/Info.plist",
                    "Contents/Resources/tflplanner.icns") %in%
                    substring(paths, nchar(a$app) + 2L)))
  exe <- a$files[[1L]]
  expect_identical(exe$mode, "0755")
  expect_match(exe$text, '^#!/bin/sh\n')
  expect_match(exe$text, 'exec "/cfg/launcher/tflplanner.sh" --update', fixed = TRUE)
  plist <- a$files[[2L]]$text
  for (k in c("CFBundleExecutable", "CFBundleIconFile", "CFBundleIdentifier",
              "CFBundlePackageType")) {
    expect_match(plist, paste0("<key>", k, "</key>"), fixed = TRUE)
  }
  expect_match(plist, "<string>APPL</string>", fixed = TRUE)
  expect_match(plist, "org.tflplanner.update", fixed = TRUE)
  expect_match(apps[[1L]]$files[[1L]]$text, 'tflplanner.sh"\n$')
})

test_that("add_shortcut() on macOS writes valid application bundles", {
  skip_on_cran()
  skip_if_not(identical(Sys.info()[["sysname"]], "Darwin"))
  p <- local_launcher()
  made <- suppressMessages(add_shortcut(ask = FALSE))
  expect_length(made, 2L)
  for (app in made) {
    exe <- file.path(app, "Contents", "MacOS", "tflplanner")
    expect_true(file.access(exe, 1L) == 0L)
    lint <- system2("plutil", c("-lint", shQuote(file.path(app, "Contents", "Info.plist"))),
                    stdout = TRUE)
    expect_match(lint, "OK$")
  }
  remove_shortcut(ask = FALSE)
  expect_false(any(dir.exists(made)))
})

# ------------------------------------------------------------- Linux

test_that("the Linux menu entry: one .desktop with an update action", {
  d <- .desktop_file("/home/u/.config/R/tflplanner/launcher", "en")
  expect_identical(d[[1L]], "[Desktop Entry]")
  expect_true("Type=Application" %in% d)
  expect_true("Terminal=false" %in% d)
  expect_true('Exec="/home/u/.config/R/tflplanner/launcher/tflplanner.sh"' %in% d)
  expect_true("Actions=update;" %in% d)
  expect_true("[Desktop Action update]" %in% d)
  expect_true('Exec="/home/u/.config/R/tflplanner/launcher/tflplanner.sh" --update' %in% d)
  expect_true(any(startsWith(d, "Icon=/home/u/.config/R/tflplanner/launcher/tflplanner.png")))
  expect_false(any(grepl("Actions", .desktop_file("/x", "en", update = FALSE))))
  expect_true(paste0("Name=", tr("Update and launch", "ja")) %in% .desktop_file("/x", "ja"))
})

test_that("add_shortcut() on Linux writes the menu entry", {
  skip_on_cran()
  skip_if_not(.Platform$OS.type == "unix" &&
                !identical(Sys.info()[["sysname"]], "Darwin"))
  p <- local_launcher()
  made <- suppressMessages(add_shortcut(ask = FALSE))
  expect_identical(basename(made), "tflplanner.desktop")
  if (nzchar(Sys.which("desktop-file-validate"))) {
    expect_identical(system2("desktop-file-validate", shQuote(made)), 0L)
  }
  remove_shortcut(ask = FALSE)
  expect_false(file.exists(made))
})

# ------------------------------------------------------------- settings

test_that("the port and the update check are settings", {
  local_launcher()
  expect_identical(.app_port(), 7470L)
  suppressMessages(setup_tflplanner(port = 7480, check_updates = FALSE))
  expect_identical(.app_port(), 7480L)
  expect_false(as.logical(tflplanner_config()$check_updates))
  expect_error(setup_tflplanner(port = 80), "1024 to 65535")
  expect_error(add_shortcut(port = "x", ask = FALSE), "1024 to 65535")
  # add_shortcut(port =) saves it
  skip_if_not(.Platform$OS.type != "windows")
  suppressMessages(add_shortcut(port = 7490, ask = FALSE))
  expect_identical(.app_port(), 7490L)
})

test_that("asking first: nothing is made without a yes", {
  local_launcher()
  local_mocked_bindings(.ask_yes = function(question) FALSE)
  expect_identical(suppressMessages(add_shortcut(ask = TRUE)), character())
  expect_false(dir.exists(.launcher_dir()))
  expect_false(update_tflplanner(from = tempdir(), ask = TRUE))
})

# ------------------------------------------------------------- packages, updates

test_that("tflplanner_packages() lists the required and suggested packages", {
  pk <- tflplanner_packages(check = FALSE)
  expect_identical(names(pk), c("package", "role", "installed", "latest", "status"))
  expect_identical(pk$package[pk$role == "required"],
                   c("rtfreporter", "tflspec", "tflplanner"))
  expect_true(all(c("cards", "cardx", "dplyr") %in% pk$package[pk$role == "suggested"]))
  expect_false("testthat" %in% pk$package)
  expect_false(is.na(pk$installed[pk$package == "tflplanner"]))
  expect_true(all(pk$status[!is.na(pk$installed)] %in% NA))
  expect_true(all(pk$status[is.na(pk$installed)] == "missing"))
})

test_that(".newer() compares versions, NA-safe", {
  expect_identical(.newer(c("0.0.3", "1.0", NA, "0.8.2"),
                          c("0.0.2.9004", "1.0", "1.0", NA)),
                   c(TRUE, FALSE, FALSE, FALSE))
})

test_that("update.R installs the newest package file of a folder, in order", {
  skip_on_cran()
  # a stand-in package named like the first of the three, installed into a
  # library of its own: the real one is not touched
  src <- withr_tempdir()
  for (v in c("9.9.1", "9.9.2")) {
    pkg <- file.path(src, "build", v, "rtfreporter")
    dir.create(file.path(pkg, "R"), recursive = TRUE)
    writeLines(c("Package: rtfreporter", paste("Version:", v),
                 "Title: Stand-in", "Description: A stand-in for a test.",
                 "License: MIT", "Encoding: UTF-8",
                 "Authors@R: person('A', 'B', email = 'a@b.c', role = c('aut', 'cre'))"),
               file.path(pkg, "DESCRIPTION"))
    writeLines("f <- function() 1", file.path(pkg, "R", "f.R"))
    writeLines("export(f)", file.path(pkg, "NAMESPACE"))
    old <- setwd(src)
    system2(file.path(R.home("bin"), "R"), c("CMD", "build", "--no-manual",
                                             shQuote(pkg)), stdout = FALSE,
            stderr = FALSE)
    setwd(old)
  }
  expect_true(file.exists(file.path(src, "rtfreporter_9.9.2.tar.gz")))
  lib <- withr_tempdir()
  res <- processx::run(
    file.path(R.home("bin"), "Rscript"),
    c(system.file("launcher", "update.R", package = "tflplanner"),
      paste0("--from=", src), paste0("--lib=", lib)),
    error_on_status = FALSE)
  expect_identical(res$status, 0L)
  expect_match(res$stdout, "rtfreporter: [0-9.NA]+ -> 9[.]9[.]2")
  expect_match(res$stdout, "tflspec: no package file", fixed = TRUE)
  expect_identical(unname(read.dcf(file.path(lib, "rtfreporter", "DESCRIPTION"),
                                   "Version")[1, 1]), "9.9.2")
  # a bad channel is refused
  bad <- processx::run(file.path(R.home("bin"), "Rscript"),
                       c(system.file("launcher", "update.R", package = "tflplanner"),
                         "--channel=nightly"), error_on_status = FALSE)
  expect_identical(bad$status, 1L)
})

test_that("update_tflplanner() checks its arguments", {
  expect_error(update_tflplanner("nightly", ask = FALSE), "should be one of")
  expect_error(update_tflplanner(from = file.path(tempdir(), "nope"), ask = FALSE),
               "must be a folder")
})

test_that("the update check: off by the option or the setting; newer versions named", {
  local_launcher()
  old <- list(proc = .upd$proc, result = .upd$result, file = .upd$file)
  on.exit(for (k in names(old)) assign(k, old[[k]], envir = .upd), add = TRUE)
  .upd$proc <- NULL
  .upd$result <- NULL
  .start_update_check()                       # the tests' option is off
  expect_identical(.update_check_result(), list())
  # a finished check: its file is read once
  .upd$result <- NULL
  f <- tempfile(fileext = ".rds")
  saveRDS(c(tflplanner = "99.0.0", tflspec = "0.0.1"), f)
  .upd$file <- f
  .upd$proc <- list(is_alive = function() FALSE)
  r <- .update_check_result()
  expect_identical(names(r), "tflplanner")
  expect_match(r[["tflplanner"]], "-> 99.0.0$")
  expect_false(file.exists(f))
  # nothing newer (or nothing found): no notice at all
  expect_identical(.newer_than_installed(c(tflplanner = "0.0.1", tflspec = NA)),
                   character())
})

# ------------------------------------------------------------- the wizard

test_that("the setup wizard: home, packages, shortcut -- each asked", {
  p <- local_launcher()
  new_home <- file.path(withr_tempdir(), "home")
  new_root <- file.path(withr_tempdir(), "studies")
  answers <- c(new_home, new_root, "ja")
  i <- 0L
  local_mocked_bindings(
    .ask_line = function(prompt) { i <<- i + 1L; answers[[i]] },
    .ask_yes = function(question) grepl("Save|保存", question))
  out <- capture.output(cfg <- .setup_wizard())
  expect_identical(cfg$home, normalizePath(new_home, "/", mustWork = FALSE))
  expect_true(dir.exists(new_root))
  expect_identical(tflplanner_config(cfg$home)$language, "ja")
  expect_true(any(grepl("3/3", out)))
  expect_false(dir.exists(.launcher_dir()))      # the shortcut was declined
  # declining the settings changes nothing
  i <- 0L
  local_mocked_bindings(.ask_yes = function(question) FALSE)
  out <- capture.output(res <- .setup_wizard())
  expect_null(res)
})

# ------------------------------------------------------------- the app

test_that("stop_on_close: the app stops once its last tab has been gone a while", {
  stopped <- 0L
  local_mocked_bindings(stopApp = function(...) stopped <<- stopped + 1L,
                        .package = "shiny")
  .open_tabs$n <- 0L
  ended <- list()
  fake <- function() list(onSessionEnded = function(f) ended[[length(ended) + 1L]] <<- f)
  .stop_when_closed(fake(), wait = 0)
  .stop_when_closed(fake(), wait = 0)
  expect_identical(.open_tabs$n, 2L)
  ended[[1L]]()
  later::run_now(0.2)
  expect_identical(stopped, 0L)               # one tab still open
  ended[[2L]]()
  later::run_now(0.2)
  expect_identical(stopped, 1L)
  .open_tabs$n <- 0L
})

test_that("RStudio add-in: Launch tflplanner -> launch_app()", {
  dcf <- read.dcf(system.file("rstudio", "addins.dcf", package = "tflplanner"))
  expect_identical(unname(dcf[1, "Binding"]), "launch_app")
  expect_true(is.function(launch_app))
})
