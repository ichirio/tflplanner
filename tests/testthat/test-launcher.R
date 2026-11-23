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
    "tflplanner.ico", "tflplanner.icns", "tflplanner.png", "VERSION",
    "launcher.conf")))))
  # launch.R finds the home through the pointer file one folder up -- the
  # one setup_tflplanner() writes
  expect_identical(dirname(dir), dirname(.pointer_file()))
  # the VBScript is UTF-16 with a BOM (wscript reads that), message filled in
  raw <- readBin(file.path(dir, "tflplanner.vbs"), "raw", 1e5)
  expect_identical(raw[1:2], as.raw(c(0xFF, 0xFE)))
  vbs <- iconv(list(raw[-(1:2)]), "UTF-16LE", "UTF-8")
  expect_false(grepl("{{NO_R}}", vbs, fixed = TRUE))
  expect_match(vbs, .r_not_found_text(.r_xy(), "ja"), fixed = TRUE)
  expect_match(vbs, 'ConfValue("r_xy")', fixed = TRUE)
  expect_match(vbs, 'Q(here & "\\launch.R") & args, 0, False', fixed = TRUE)
  sh <- readLines(file.path(dir, "tflplanner.sh"), encoding = "UTF-8")
  expect_identical(sh[[1L]], "#!/bin/sh")
  expect_false(any(grepl("{{NO_R}}", sh, fixed = TRUE)))
  expect_true(any(grepl(.r_not_found_text(.r_xy(), "ja"), sh, fixed = TRUE)))
  # the settings file remembers the R it was made in (UTF-16 on Windows)
  craw <- readBin(file.path(dir, "launcher.conf"), "raw", 1e4)
  conf <- if (identical(.os(), "windows")) {
    iconv(list(craw[-(1:2)]), "UTF-16LE", "UTF-8")
  } else {
    rawToChar(craw)
  }
  expect_match(conf, paste0("r_xy=", .r_xy()), fixed = TRUE)
  expect_match(conf, "r_home=", fixed = TRUE)
  expect_match(conf, .show_path(normalizePath(R.home(), "/", mustWork = FALSE)), fixed = TRUE)
  expect_true(any(grepl("R.framework/Versions", sh, fixed = TRUE)))
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
  sc <- .windows_shortcuts("C:/cfg/launcher", dirs, "en", update = TRUE, xy = "4.6")
  expect_identical(basename(sc$path),
                   c("tflplanner (R 4.6).lnk", "tflplanner (R 4.6).lnk",
                     "tflplanner (R 4.6) - Update and launch.lnk"))
  expect_identical(dirname(sc$path), c(dirs$desktop, dirs$start_menu,
                                       dirs$start_menu))
  expect_true(all(grepl("wscript.exe$", sc$target)))
  expect_true(all(startsWith(sc$args, '"C:\\cfg\\launcher\\tflplanner.vbs"')))
  expect_identical(endsWith(sc$args, "--update"), c(FALSE, FALSE, TRUE))
  expect_true(all(grepl("tflplanner.ico$", sc$icon)))
  # Japanese names
  sc_ja <- .windows_shortcuts("C:/cfg/launcher", dirs, "ja", desktop = FALSE,
                              update = TRUE, xy = "4.6")
  expect_identical(basename(sc_ja$path),
                   c("tflplanner (R 4.6).lnk",
                     paste0("tflplanner (R 4.6) - ", tr("Update and launch", "ja"), ".lnk")))
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

test_that("a shortcut the VBScript could not make is made by PowerShell, else said why", {
  base <- withr::local_tempdir()
  dirs <- list(desktop = file.path(base, "Desktop"),
               start_menu = file.path(base, "Programs", "tflplanner"))
  sc <- .windows_shortcuts(file.path(base, "launcher"), dirs, "en", update = TRUE)
  make <- function(r) {
    dir.create(dirname(r$path), recursive = TRUE, showWarnings = FALSE)
    writeLines("lnk", r$path)
    character()
  }
  nothing <- function(r) character()
  calls <- character()
  log_ps <- function(r) {
    calls <<- c(calls, basename(r$path))
    make(r)
  }
  # the VBScript makes them: PowerShell is not asked
  expect_identical(.make_lnk(sc, vbs = make, ps = log_ps), sc$path)
  expect_identical(calls, character())
  unlink(sc$path)
  # the VBScript stopped (security software, without a word) for the
  # desktop only: PowerShell makes that one, the others are the VBScript's
  vbs_no_desktop <- function(r) if (basename(dirname(r$path)) == "Desktop") character() else make(r)
  expect_identical(.make_lnk(sc, vbs = vbs_no_desktop, ps = log_ps), sc$path)
  expect_identical(calls, paste0(.shortcut_base(), ".lnk"))
  expect_true(all(file.exists(sc$path)))
  unlink(sc$path)
  # neither: the error names it and says what to do
  err <- expect_error(.make_lnk(sc, vbs = nothing, ps = function(r) "blocked by policy"),
                      "Could not make the shortcut")
  expect_match(conditionMessage(err), "(and 2 more)", fixed = TRUE)
  expect_match(conditionMessage(err), "blocked by policy", fixed = TRUE)
  expect_match(conditionMessage(err), "Security software", fixed = TRUE)
  expect_match(conditionMessage(err), "New > Shortcut", fixed = TRUE)
  expect_match(conditionMessage(err), "wscript.exe", fixed = TRUE)
  # an error from one way is a message, not a stop
  expect_identical(.make_lnk(sc, vbs = function(r) stop("cscript missing"), ps = make), sc$path)
  # the PowerShell: WScript.Shell, the values quoted for PowerShell
  r <- sc[3, ]
  r$description <- "it's"
  ps <- .lnk_ps(r)
  expect_true(any(grepl("New-Object -ComObject WScript.Shell", ps, fixed = TRUE)))
  expect_true(any(grepl("$l.Description = 'it''s'", ps, fixed = TRUE)))
  expect_true(any(grepl("--update'$", ps)))
  expect_identical(ps[length(ps)], "$l.Save()")
})

test_that("the PowerShell way makes a real .lnk", {
  skip_on_cran()
  skip_if_not(.Platform$OS.type == "windows")
  base <- withr::local_tempdir()
  dirs <- list(desktop = file.path(base, "D"), start_menu = file.path(base, "P"))
  sc <- .windows_shortcuts(file.path(base, "launcher"), dirs, "en",
                           start_menu = FALSE, update = FALSE)
  .lnk_by_ps(sc)
  expect_true(file.exists(sc$path))
})

test_that("add_shortcut() / remove_shortcut() on Windows make and remove real .lnk files", {
  skip_on_cran()
  skip_if_not(.Platform$OS.type == "windows")
  p <- local_launcher()
  made <- suppressMessages(add_shortcut(ask = FALSE))
  expect_length(made, 2L)
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
  # the VBScript's own way of finding R, run for real: --which prints the
  # Rscript it would use and starts nothing
  local_launcher()
  dir <- .write_launcher()
  out <- system2(file.path(Sys.getenv("SystemRoot"), "System32", "cscript.exe"),
                 c("//nologo", shQuote(file.path(dir, "tflplanner.vbs")), "--which"),
                 stdout = TRUE)
  expect_match(out[[length(out)]], "Rscript.exe$")
  expect_true(file.exists(out[[length(out)]]))
})

# ------------------------------------------------------------- macOS

test_that("the macOS apps: a bundle whose executable runs the launcher", {
  apps <- .mac_app_files("/cfg/launcher", "/Users/u/Applications", "en", update = TRUE,
                         xy = "4.6")
  expect_identical(basename(vapply(apps, `[[`, "", "app")),
                   c("tflplanner (R 4.6).app", "tflplanner (R 4.6) (update).app"))
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
  expect_match(plist, "org.tflplanner.update.R4-6", fixed = TRUE)
  expect_match(apps[[1L]]$files[[1L]]$text, 'tflplanner.sh"\n$')
})

test_that("add_shortcut() on macOS writes valid application bundles", {
  skip_on_cran()
  skip_if_not(identical(Sys.info()[["sysname"]], "Darwin"))
  p <- local_launcher()
  made <- suppressMessages(add_shortcut(ask = FALSE))
  expect_length(made, 1L)
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
  d <- .desktop_file("/home/u/.config/R/tflplanner/launcher", "en", update = TRUE)
  expect_identical(d[[1L]], "[Desktop Entry]")
  expect_true("Type=Application" %in% d)
  expect_true("Terminal=false" %in% d)
  expect_true('Exec="/home/u/.config/R/tflplanner/launcher/tflplanner.sh"' %in% d)
  expect_true("Actions=update;" %in% d)
  expect_true("[Desktop Action update]" %in% d)
  expect_true('Exec="/home/u/.config/R/tflplanner/launcher/tflplanner.sh" --update' %in% d)
  expect_true(any(startsWith(d, "Icon=/home/u/.config/R/tflplanner/launcher/tflplanner.png")))
  expect_false(any(grepl("Actions", .desktop_file("/x", "en", update = FALSE))))
  # one entry per R x.y: its name and file say which
  expect_true("Name=tflplanner (R 4.6)" %in% .desktop_file("/x", "en", xy = "4.6"))
  expect_identical(.desktop_name("4.6"), "tflplanner-R4.6.desktop")
  expect_true(paste0("Name=", tr("Update and launch", "ja")) %in% .desktop_file("/x", "ja", update = TRUE))
})

test_that("add_shortcut() on Linux writes the menu entry", {
  skip_on_cran()
  skip_if_not(.Platform$OS.type == "unix" &&
                !identical(Sys.info()[["sysname"]], "Darwin"))
  p <- local_launcher()
  made <- suppressMessages(add_shortcut(ask = FALSE))
  expect_identical(basename(made), .desktop_name())
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

test_that("add_shortcut() says what it makes, where, and how to undo it", {
  p <- local_launcher()
  local_mocked_bindings(.os = function() "windows")
  dirs <- .shortcut_dirs()
  plan <- .shortcut_plan(lang = "en", update = TRUE)
  expect_identical(plan$place, c("Desktop", "Start menu", "Start menu"))
  base <- .shortcut_base()
  expect_identical(plan$name, c(base, base, paste0(base, " - Update and launch")))
  expect_false(any(plan$exists))
  txt <- paste(.shortcut_plan_text(plan, "en"), collapse = "\n")
  expect_match(txt, paste0("tflplanner will make these shortcuts, for R ", .r_xy(), ":"),
               fixed = TRUE)
  expect_match(txt, .show_path(dirs$desktop), fixed = TRUE)
  expect_match(txt, .show_path(dirs$start_menu), fixed = TRUE)
  expect_match(txt, .show_path(.launcher_dir()), fixed = TRUE)
  expect_match(txt, "remove_shortcut()", fixed = TRUE)
  expect_false(grepl("replaced", txt))
  expect_identical(.shortcut_question(3L, "en"), "Make these 3 shortcuts?")
  # one already there is said to be replaced
  dir.create(dirs$desktop, recursive = TRUE)
  file.create(file.path(dirs$desktop, paste0(base, ".lnk")))
  plan <- .shortcut_plan(lang = "en", update = TRUE)
  expect_identical(plan$exists, c(TRUE, FALSE, FALSE))
  expect_match(paste(.shortcut_plan_text(plan, "en"), collapse = "\n"),
               "already there: it will be replaced", fixed = TRUE)
  # in Japanese: where and what, and a question naming how many
  ja <- paste(.shortcut_plan_text(.shortcut_plan(lang = "ja", update = TRUE), "ja"),
              collapse = "\n")
  expect_match(ja, "デスクトップ", fixed = TRUE)
  expect_match(ja, "スタートメニュー", fixed = TRUE)
  expect_match(ja, paste0(base, " - 更新して起動"), fixed = TRUE)
  expect_match(.shortcut_question(3L, "ja"), "3 個のショートカットを作成します。よろしいですか", fixed = TRUE)
  # afterwards: the files, how to pin, how to remove
  done <- paste(.shortcut_made_text(plan$path, "windows", "en"), collapse = "\n")
  expect_match(done, "Made 3 shortcuts:", fixed = TRUE)
  expect_match(done, "Pin to taskbar", fixed = TRUE)
  expect_match(done, "remove_shortcut()", fixed = TRUE)
  # the asking path shows it all before the question
  local_mocked_bindings(.ask_yes = function(question) {
    expect_identical(question, "Make these 3 shortcuts?")
    FALSE
  })
  out <- capture.output(res <- suppressMessages(add_shortcut(ask = TRUE, update = TRUE)))
  expect_identical(res, character())
  expect_true(any(grepl("Start menu", out)))
})

test_that("macOS and Linux name their places", {
  local_launcher()
  local_mocked_bindings(.os = function() "mac")
  plan <- .shortcut_plan(lang = "en", update = TRUE)
  expect_identical(plan$name, c(paste0(.shortcut_base(), ".app"),
                                paste0(.shortcut_base(), " (update).app")))
  expect_identical(unique(plan$place), "Applications")
  expect_match(paste(.shortcut_made_text(plan$path, "mac", "en"), collapse = " "),
               "Finder > Applications", fixed = TRUE)
  local_mocked_bindings(.os = function() "linux")
  plan <- .shortcut_plan(lang = "en", update = TRUE)
  expect_identical(plan$place, "the application menu")
  expect_match(plan$name, "Update and launch", fixed = TRUE)
})

test_that("remove_shortcut() and update_tflplanner() say what they do and leave", {
  local_launcher()
  txt <- paste(.remove_plan_text("C:/x/Desktop/tflplanner.lnk", "C:/cfg/launcher",
                                 "en"), collapse = "\n")
  expect_match(txt, "Shortcuts:", fixed = TRUE)
  expect_match(txt, "The launcher files the shortcuts run:", fixed = TRUE)
  expect_match(txt, "studies, their files and tflplanner's settings are not touched", fixed = TRUE)
  expect_match(txt, "add_shortcut()", fixed = TRUE)
  up <- paste(.update_plan_text("release", NULL, "C:/lib", "en"), collapse = "\n")
  expect_match(up, "rtfreporter, tflspec, tflplanner", fixed = TRUE)
  expect_match(up, 'update_tflplanner("dev")', fixed = TRUE)
  expect_match(up, "Restart R", fixed = TRUE)
  expect_false(grepl("channel", .update_plan_text("dev", "D:/pk", "C:/lib", "en")[2]))
})

test_that("every sentence of the setup, the shortcuts and the update has its Japanese", {
  src <- test_path("..", "..", "R", c("launcher.R", "setup-wizard.R", "update.R"))
  skip_if_not(all(file.exists(src)), "source not available (installed package)")
  code <- unlist(lapply(src, readLines, encoding = "UTF-8", warn = FALSE))
  lits <- unlist(regmatches(code, gregexpr('\\b(tr|t)\\("((?:[^"\\\\]|\\\\.)*)"', code, perl = TRUE)))
  lits <- unique(eval(parse(text = paste0("c(", paste(sub("^(tr|t)\\(", "", lits), collapse = ","), ")"))))
  st <- .strings()
  expect_identical(setdiff(lits, st$en), character(0))
  expect_true(all(nzchar(st$ja[match(lits, st$en)])))
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
  expect_true("dplyr" %in% pk$package[pk$role == "suggested"])
  # cards and cardx are imported (required) now, so the wizard does not offer them
  expect_false(any(c("cards", "cardx") %in% pk$package))
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
  # compared once it exists: Windows may give a short (8.3) name or a long one
  expect_identical(normalizePath(cfg$home, "/"), normalizePath(new_home, "/"))
  expect_true(dir.exists(new_root))
  expect_identical(tflplanner_config(cfg$home)$language, "ja")
  expect_true(any(grepl("3/3", out)))
  # each step says what it will write, and where
  expect_true(any(grepl("config.yml", out, fixed = TRUE)))
  # (not the whole path: on Windows a folder made since may then read with
  # its long name instead of the short one, RUNNER~1)
  expect_true(any(grepl("remember the home folder in .*tflplanner.home$", out)))
  expect_true(any(grepl("remove_shortcut()", out, fixed = TRUE)))
  expect_true(any(grepl("add_shortcut()", out, fixed = TRUE)))
  expect_false(dir.exists(.launcher_dir()))      # the shortcut was declined
  # declining the settings changes nothing
  i <- 0L
  local_mocked_bindings(.ask_yes = function(question) FALSE)
  out <- capture.output(res <- .setup_wizard())
  expect_null(res)
})

test_that("the setup wizard asks again for a home inside the study folders", {
  p <- local_launcher()
  root <- file.path(withr_tempdir(), "studies")
  bad <- file.path(root, "tflplanner_home")
  good <- file.path(withr_tempdir(), "home")
  answers <- c(bad, root, good, root, "en")
  i <- 0L
  local_mocked_bindings(
    .ask_line = function(prompt) { i <<- i + 1L; answers[[i]] },
    .ask_yes = function(question) grepl("Save|保存", question))
  out <- capture.output(cfg <- .setup_wizard())
  expect_true(any(grepl("where new study folders go", out, fixed = TRUE)))
  expect_identical(normalizePath(cfg$home, "/"), normalizePath(good, "/"))
  expect_false(dir.exists(bad))
})

# ------------------------------------------------------------- the app

test_that("stop_on_close: the app stops once its last tab has been gone a while", {
  stopped <- 0L
  local_mocked_bindings(stopApp = function(...) stopped <<- stopped + 1L,
                        .package = "shiny")
  .open_tabs$n <- 0L
  ended <- list()
  fake <- function() list(onSessionEnded = function(f) ended[[length(ended) + 1L]] <<- f,
                          onUnhandledError = function(f) NULL)
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

test_that("stop_on_close: a session an error ended leaves time for the page's Reload", {
  stopped <- 0L
  local_mocked_bindings(stopApp = function(...) stopped <<- stopped + 1L,
                        .package = "shiny")
  .open_tabs$n <- 0L
  ended <- NULL
  failed <- NULL
  .stop_when_closed(list(onSessionEnded = function(f) ended <<- f,
                         onUnhandledError = function(f) failed <<- f),
                    wait = 0, wait_failed = 1)
  expect_message(failed(simpleError("boom")), "stopped on an error: boom")
  ended()
  later::run_now(0.2)
  expect_identical(stopped, 0L)               # not at the short wait
  # the Reload: a new session within the wait keeps the app
  .open_tabs$n <- 1L
  later::run_now(1.5)
  expect_identical(stopped, 0L)
  .open_tabs$n <- 0L
})

test_that("the launcher keeps the app's console in app.log", {
  src <- readLines(system.file("launcher", "launch.R", package = "tflplanner"))
  expect_true(any(grepl('file.path(here, "app.log")', src, fixed = TRUE)))
  expect_true(any(grepl('sink(app_log, type = "message")', src, fixed = TRUE)))
})

test_that("RStudio add-in: Launch tflplanner -> launch_app()", {
  dcf <- read.dcf(system.file("rstudio", "addins.dcf", package = "tflplanner"))
  expect_identical(unname(dcf[1, "Binding"]), "launch_app")
  expect_true(is.function(launch_app))
})

test_that("the update passes on this session's repositories and download method", {
  withr::local_options(repos = c(CRAN = "https://mirror.example/cran",
                                 INT = "https://int.example/r"),
                       download.file.method = "wininet")
  a <- .update_net_args()
  expect_true("--repos=CRAN=https://mirror.example/cran;INT=https://int.example/r" %in% a)
  expect_true("--method=wininet" %in% a)
})

test_that("a failed update says what was not reached, and the ways round it", {
  out <- paste("tflplanner update: dev -> lib",
               "UNREACHABLE: https://api.github.com/repos/ichirio/tflplanner",
               "FAILED: rtfreporter: cannot open URL", sep = "\n")
  txt <- paste(.update_failed_text(out, "dev", NULL, "en"), collapse = "\n")
  expect_match(txt, "could not reach", fixed = TRUE)
  expect_match(txt, "https://api.github.com/repos/ichirio/tflplanner", fixed = TRUE)
  expect_match(txt, "rtfreporter: cannot open URL", fixed = TRUE)
  expect_match(txt, 'remotes::install_github("ichirio/tflspec")', fixed = TRUE)
  expect_match(txt, 'update_tflplanner(from = "<folder>")', fixed = TRUE)
  rel <- paste(.update_failed_text("", "release", NULL, "en"), collapse = "\n")
  expect_match(rel, 'install_github("ichirio/rtfreporter@*release")', fixed = TRUE)
  ja <- paste(.update_failed_text(out, "dev", NULL, "ja"), collapse = "\n")
  expect_false(grepl("could not reach", ja, fixed = TRUE))
})

test_that("update.R with the network closed says which addresses, and stops", {
  skip_on_cran()
  lib <- withr_tempdir()
  # every address through a proxy nobody listens on: nothing is reachable
  res <- withr::with_envvar(
    c(https_proxy = "http://127.0.0.1:9", http_proxy = "http://127.0.0.1:9",
      HTTPS_PROXY = "http://127.0.0.1:9", HTTP_PROXY = "http://127.0.0.1:9",
      no_proxy = "", NO_PROXY = ""),
    processx::run(file.path(R.home("bin"), "Rscript"),
                  c(system.file("launcher", "update.R", package = "tflplanner"),
                    "--channel=dev", paste0("--lib=", lib),
                    "--repos=CRAN=https://cloud.r-project.org"),
                  error_on_status = FALSE, timeout = 120))
  expect_identical(res$status, 1L)
  expect_match(res$stdout, "UNREACHABLE: https://api.github.com", fixed = TRUE)
  expect_match(res$stdout, "package repositories: https://cloud.r-project.org", fixed = TRUE)
  expect_match(res$stdout, "FAILED: network", fixed = TRUE)
  txt <- paste(.update_failed_text(res$stdout, "dev", NULL, "en"), collapse = "\n")
  expect_match(txt, "install_github", fixed = TRUE)
})

test_that("a newer version installed while the app runs is said on its page", {
  # the version on disk against the one this process runs
  d <- withr_tempdir()
  writeLines(c("Package: tflplanner", "Version: 9.9.9"), file.path(d, "DESCRIPTION"))
  s <- .stale_versions("tflplanner", running = function(p) "0.0.1", path = function(p) d)
  expect_identical(s$tflplanner, c(running = "0.0.1", installed = "9.9.9"))
  expect_length(.stale_versions("tflplanner", running = function(p) "9.9.9",
                                path = function(p) d), 0L)
  # the page says it, above everything
  local_home()
  two_studies()
  testthat::local_mocked_bindings(
    .stale_versions = function(...) list(tflplanner = c(running = "0.0.2.9070",
                                                        installed = "0.0.2.9088")))
  shiny::testServer(server_for("S1"), {
    h <- output$update_note$html
    expect_match(h, "A newer version is installed", fixed = TRUE)
    expect_match(h, "tflplanner 0.0.2.9088", fixed = TRUE)
    expect_match(h, "0.0.2.9070", fixed = TRUE)
  })
})

test_that("nothing updates on its own: no update shortcut, no update check unless asked", {
  local_launcher()
  local_mocked_bindings(.os = function() "windows")
  expect_identical(.shortcut_plan(lang = "en")$name, rep(.shortcut_base(), 2L))
  expect_false(formals(add_shortcut)$update)
  dirs <- list(desktop = "C:/d", start_menu = "C:/s")
  expect_false(any(endsWith(.windows_shortcuts("C:/x", dirs, "en")$args, "--update")))
  expect_false(any(grepl("Actions", .desktop_file("/x", "en"))))
  # the check: off with no setting; a setting given is kept
  old <- options(tflplanner.check_updates = NULL)
  on.exit(options(old), add = TRUE)
  saved <- list(proc = .upd$proc, result = .upd$result, file = .upd$file)
  on.exit(for (k in names(saved)) assign(k, saved[[k]], envir = .upd), add = TRUE)
  cfg <- tflplanner_config()
  expect_null(cfg$check_updates)
  .upd$proc <- NULL
  .upd$result <- NULL
  .start_update_check()
  expect_null(.upd$proc)
  expect_identical(.update_check_result(), list())
})

# ------------------------------------------------------------- the R x.y (#784)

test_that("the R x.y, the icon's name and the launcher folder", {
  expect_identical(.r_xy(list(major = "4", minor = "6.1")), "4.6")
  expect_identical(.r_xy(list(major = "4", minor = "10.0")), "4.10")
  expect_match(.r_xy(), "^[0-9]+[.][0-9]+$")
  expect_identical(.shortcut_base("4.6"), "tflplanner (R 4.6)")
  expect_identical(unname(.shortcut_names("en", "4.5")),
                   c("tflplanner (R 4.5)", "tflplanner (R 4.5) - Update and launch"))
  # each x.y has its own folder, beside the home pointer; the old one is another
  d46 <- .launcher_dir("4.6")
  expect_identical(basename(d46), "launcher-R4.6")
  expect_false(identical(d46, .launcher_dir("4.5")))
  expect_identical(dirname(d46), dirname(.pointer_file()))
  expect_identical(basename(.legacy_launcher_dir()), "launcher")
  expect_identical(.launcher_conf_text("C:\\R\\R-4.6.1", "4.6"),
                   c("r_home=C:\\R\\R-4.6.1", "r_xy=4.6"))
})

test_that("an Rscript's x.y from its path", {
  expect_identical(.xy_of_path("C:/Program Files/R/R-4.6.1/bin/Rscript.exe"), "4.6")
  expect_identical(.xy_of_path("C:/R/R-4.10.0patched/bin/Rscript.exe"), "4.10")
  expect_identical(.xy_of_path("/opt/R/4.6.1/bin/Rscript"), "4.6")
  expect_identical(.xy_of_path("/Library/Frameworks/R.framework/Versions/4.6-arm64/Resources/bin/Rscript"), "4.6")
  expect_identical(.xy_of_path("/Library/Frameworks/R.framework/Versions/4.6/Resources/bin/Rscript"), "4.6")
  # the paths that do not say: the Current R, a system R
  expect_true(is.na(.xy_of_path("/Library/Frameworks/R.framework/Resources/bin/Rscript")))
  expect_true(is.na(.xy_of_path("/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript")))
  expect_true(is.na(.xy_of_path("/usr/lib/R/bin/Rscript")))
})

test_that(".pick_r(): remembered (of its x.y), else the newest of the x.y, else PATH -- never another x.y", {
  have <- c("C:/R/R-4.6.1/bin/Rscript.exe", "C:/R/R-4.6.2/bin/Rscript.exe",
            "C:/R/R-4.6.10/bin/Rscript.exe",
            "C:/R/R-4.10.0/bin/Rscript.exe", "C:/R/R-4.5.3/bin/Rscript.exe")
  folders <- c("C:/R/R-4.6.1", "C:/R/R-4.6.2", "C:/R/R-4.6.10", "C:/R/R-4.10.0",
               "C:/R/R-4.5.3", "C:/R/notR")
  ex <- function(f) f %in% have
  # 1. the remembered R, when its Rscript is there and is this x.y
  r <- .pick_r("4.6", "C:/R/R-4.6.1", folders, exists = ex)
  expect_identical(r, list(rscript = "C:/R/R-4.6.1/bin/Rscript.exe", from = "remembered"))
  # 2. it is gone: the newest of the same x.y -- 4.6.10, not 4.6.2 (a number,
  # not a string), not 4.10.0 or 4.5.3
  r <- .pick_r("4.6", "C:/R/R-4.6.0", folders, exists = ex)
  expect_identical(r, list(rscript = "C:/R/R-4.6.10/bin/Rscript.exe", from = "folder"))
  expect_identical(.pick_r("4.10", NULL, folders, exists = ex)$rscript,
                   "C:/R/R-4.10.0/bin/Rscript.exe")
  # a folder without its Rscript is not a candidate
  expect_identical(.pick_r("4.6", NULL, folders, exists = function(f) f == have[[1L]])$rscript,
                   have[[1L]])
  # no R of 4.9 is there, and 4.10 or 4.5 are not taken for it
  expect_null(.pick_r("4.9", NULL, folders, exists = ex))
  # the remembered R of another x.y is not used (the Current R moved on)
  cur <- "C:/R/Current"
  r <- .pick_r("4.6", cur, folders, exists = function(f) ex(f) || f == "C:/R/Current/bin/Rscript.exe",
               xy_of = function(f) if (grepl("Current", f)) "4.7" else .xy_of_path(f))
  expect_identical(r$from, "folder")
  expect_identical(.pick_r("4.7", cur, character(),
                           exists = function(f) f == "C:/R/Current/bin/Rscript.exe",
                           xy_of = function(f) "4.7")$from, "remembered")
  # 3. PATH: the first entry of this x.y, in order
  px <- function(f) if (grepl("^D:", f)) sub("^D:/(.*)/bin/.*$", "\\1", f) else .xy_of_path(f)
  path <- c("D:/9.6/bin/Rscript.exe", "D:/4.7/bin/Rscript.exe", "D:/4.7/bin/Rscript.exe")
  r <- .pick_r("4.7", NULL, folders, path, xy_of = px, exists = function(f) TRUE)
  expect_identical(r, list(rscript = "D:/4.7/bin/Rscript.exe", from = "path"))
  expect_null(.pick_r("4.8", NULL, folders, path, xy_of = px, exists = function(f) TRUE))
  expect_null(.pick_r("4.7", NULL, folders, path, xy_of = function(f) NA_character_,
                      exists = function(f) TRUE))
  # 4. none
  expect_null(.pick_r("4.6", NULL, character(), exists = ex))
  expect_null(.pick_r("4.6", "C:/R/R-4.6.9", character(), exists = ex))
  expect_identical(.r_folder_version(c("C:/R/R-4.6.1patched", "x/R-4.7.0beta", "x/other")),
                   c("4.6.1", "4.7.0", NA))
  expect_identical(.version_xy(c("4.6.1", "4.10.0", NA)), c("4.6", "4.10", NA))
})

test_that("no R of the x.y: the message names the x.y, in the launcher's language", {
  expect_identical(
    .r_not_found_text("4.6", "en"),
    "R 4.6 is not found. Install tflplanner in the R you will use, and run add_shortcut() in that R.")
  expect_identical(
    .r_not_found_text("4.6", "ja"),
    "R 4.6 \u304c\u898b\u3064\u304b\u308a\u307e\u305b\u3093\u3002\u4f7f\u3046 R \u3067 tflplanner \u3092\u5165\u308c\u3001\u305d\u306e R \u3067 add_shortcut() \u3092\u6d41\u3057\u3066\u304f\u3060\u3055\u3044")
})

# One table of cases, run through .pick_r(), tflplanner.sh and tflplanner.vbs
# (their `--which`: print the pick, start nothing).  x.y 9.9, so a real R never
# matches.  Each R is <root>/R-<version>/bin/Rscript[.exe]; the .sh's fake
# Rscript says its version, the VBScript reads it from the folder's name.
.fake_r_tree <- function(base) {
  win <- identical(.Platform$OS.type, "windows")
  exe <- if (win) "Rscript.exe" else "Rscript"
  make <- function(root, v, rscript = TRUE) {
    d <- file.path(root, paste0("R-", v), "bin")
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(d, exe)
    if (!rscript) return(invisible(f))
    if (win) file.create(f) else {
      writeLines(c("#!/bin/sh", sprintf("echo 'Rscript (R) version %s (2099-01-01)'", v)), f)
      Sys.chmod(f, "0755")
    }
    invisible(f)
  }
  root <- file.path(base, "roots")
  for (v in c("9.9.1", "9.9.10", "9.10.0", "9.8.5")) make(root, v)
  make(root, "9.9.2", rscript = FALSE)                  # a folder without its Rscript
  p1 <- file.path(base, "p1")
  make(p1, "9.8.7")                                      # PATH: another x.y first
  p2 <- file.path(base, "p2")
  make(p2, "9.9.3")
  p3 <- file.path(base, "p3")
  make(p3, "9.9.4")                                      # a later PATH entry of 9.9
  list(root = root, exe = exe,
       path = file.path(c(p1, p2, p3), c("R-9.8.7", "R-9.9.3", "R-9.9.4"), "bin"),
       nowhere = file.path(base, "empty", "R-9.9.0"))
}

.which_cases <- function(t) {
  r <- function(v) file.path(t$root, paste0("R-", v))
  list(
    list(name = "remembered wins over newer", remembered = r("9.9.1"), path = FALSE,
         expect = "roots/R-9.9.1/bin"),
    list(name = "remembered gone: newest of the x.y", remembered = r("9.9.0"), path = FALSE,
         expect = "roots/R-9.9.10/bin"),
    list(name = "remembered of another x.y", remembered = r("9.8.5"), path = FALSE,
         expect = "roots/R-9.9.10/bin"),
    list(name = "no folder of the x.y: PATH, first of the x.y", remembered = t$nowhere,
         roots = FALSE, path = TRUE, expect = "p2/R-9.9.3/bin"),
    list(name = "nothing", remembered = t$nowhere, roots = FALSE, path = FALSE,
         expect = NA_character_))
}

test_that("the R rule: one table through .pick_r(), tflplanner.sh and tflplanner.vbs", {
  base <- withr_tempdir()
  t <- .fake_r_tree(base)
  cases <- .which_cases(t)
  norm <- function(x) gsub("\\", "/", x, fixed = TRUE)
  has <- function(got, expect) {
    if (is.na(expect)) length(got) == 0L else endsWith(norm(got), paste0(expect, "/", t$exe))
  }
  # the rule in R
  for (cs in cases) {
    folders <- if (isFALSE(cs$roots)) character() else
      list.dirs(t$root, recursive = FALSE)
    path <- if (isTRUE(cs$path)) file.path(t$path, t$exe) else character()
    pick <- .pick_r("9.9", cs$remembered, folders, path,
                    rscript_of = function(f) file.path(f, "bin", t$exe))
    expect_true(has(pick$rscript, cs$expect), info = cs$name)
  }
  # the .sh
  if (.Platform$OS.type != "windows" && nzchar(Sys.which("sh"))) {
    for (cs in cases) {
      dir <- withr_tempdir()
      .write_launcher(dir, "en", "9.9", cs$remembered)
      search <- if (isFALSE(cs$roots)) file.path(base, "none", "*", "bin", "Rscript") else
        file.path(t$root, "R-*", "bin", "Rscript")
      path <- paste(c(if (isTRUE(cs$path)) t$path, Sys.getenv("PATH")), collapse = ":")
      out <- withr::with_envvar(c(TFLPLANNER_R_SEARCH = search, PATH = path),
        suppressWarnings(system2("sh", c(shQuote(file.path(dir, "tflplanner.sh")), "--which"),
                                 stdout = TRUE, stderr = FALSE)))
      expect_true(has(out, cs$expect), info = paste(".sh:", cs$name))
    }
    # the remembered R is the Current one, now of another x.y: not used
    dir <- withr_tempdir()
    cur <- file.path(base, "current")
    dir.create(file.path(cur, "bin"), recursive = TRUE)
    writeLines(c("#!/bin/sh", "echo 'Rscript (R) version 9.7.0 (2099-01-01)'"),
               file.path(cur, "bin", "Rscript"))
    Sys.chmod(file.path(cur, "bin", "Rscript"), "0755")
    .write_launcher(dir, "en", "9.9", cur)
    out <- withr::with_envvar(
      c(TFLPLANNER_R_SEARCH = file.path(t$root, "R-*", "bin", "Rscript")),
      suppressWarnings(system2("sh", c(shQuote(file.path(dir, "tflplanner.sh")), "--which"),
                               stdout = TRUE, stderr = FALSE)))
    expect_true(endsWith(out, "R-9.9.10/bin/Rscript"))
    # none: the message of step 4 is in launcher.log, and it stops
    dir <- withr_tempdir()
    .write_launcher(dir, "en", "9.9", t$nowhere)
    st <- withr::with_envvar(
      c(TFLPLANNER_R_SEARCH = file.path(base, "none", "*", "bin", "Rscript")),
      suppressWarnings(system2("sh", shQuote(file.path(dir, "tflplanner.sh")),
                               stdout = FALSE, stderr = FALSE)))
    expect_identical(st, 1L)
    expect_match(paste(readLines(file.path(dir, "launcher.log")), collapse = "\n"),
                 .r_not_found_text("9.9", "en"), fixed = TRUE)
  }
  # the VBScript (Windows)
  if (identical(.Platform$OS.type, "windows")) {
    cscript <- file.path(Sys.getenv("SystemRoot"), "System32", "cscript.exe")
    for (cs in cases) {
      dir <- withr_tempdir()
      .write_launcher(dir, "en", "9.9", cs$remembered)
      roots <- if (isFALSE(cs$roots)) file.path(base, "none") else t$root
      path <- paste(c(if (isTRUE(cs$path)) .win_path(t$path), Sys.getenv("PATH")),
                    collapse = ";")
      out <- withr::with_envvar(
        c(TFLPLANNER_R_ROOTS = .win_path(roots), PATH = path),
        suppressWarnings(system2(cscript, c("//nologo", shQuote(file.path(dir, "tflplanner.vbs")),
                                            "--which"), stdout = TRUE, stderr = FALSE)))
      expect_true(has(out, cs$expect), info = paste(".vbs:", cs$name))
    }
  }
})

test_that("launcher.conf holds the R it was made in, whatever the characters of the path", {
  dir <- withr_tempdir()
  home <- "C:/Program Files/R/\u65e5\u672c\u8a9e/R-9.9.1"
  .write_launcher(dir, "en", "9.9", home)
  cf <- .read_launcher_conf(dir)
  expect_identical(cf$r_xy, "9.9")
  expect_identical(cf$r_home, .show_path(normalizePath(home, "/", mustWork = FALSE)))
  expect_match(cf$r_home, "\u65e5\u672c\u8a9e", fixed = TRUE)
  # UTF-8 as well (the shell script reads it)
  local_mocked_bindings(.os = function() "linux")
  .write_launcher(dir, "en", "9.9", home)
  expect_match(.read_launcher_conf(dir)$r_home, "\u65e5\u672c\u8a9e", fixed = TRUE)
  # .write_utf16() takes text in any encoding
  f <- tempfile()
  x <- iconv("\u00e9", "UTF-8", "latin1")                 # marked latin1
  .write_utf16(x, f)
  expect_identical(iconv(list(readBin(f, "raw", 100)[-(1:2)]), "UTF-16LE", "UTF-8"), "\u00e9")
})

test_that("only add_shortcut() says the R: a refresh and launch_app() keep it", {
  local_launcher()
  dir <- .write_launcher(r_home = "C:/R/R-4.6.1")
  before <- .read_launcher_conf(dir)
  writeLines("0.0.0", file.path(dir, "VERSION"))
  expect_true(.refresh_launcher(dir))
  expect_identical(.read_launcher_conf(dir), before)
  .write_launcher()                                     # launch_app()
  expect_identical(.read_launcher_conf(dir), before)
  # a launcher with no record gets this R's
  unlink(file.path(dir, "launcher.conf"))
  .write_launcher()
  expect_identical(.read_launcher_conf(dir)$r_home,
                   .show_path(normalizePath(R.home(), "/", mustWork = FALSE)))
})

test_that("two R x.y: two launcher folders, each with its own R; removing one leaves the other", {
  p <- local_launcher()
  local_mocked_bindings(.os = function() "linux")
  d44 <- .write_launcher(xy = "4.4", r_home = "/opt/R/4.4.3")
  d46 <- .write_launcher(xy = "4.6", r_home = "/opt/R/4.6.1")
  expect_identical(.read_launcher_conf(d44)$r_xy, "4.4")
  expect_match(.read_launcher_conf(d46)$r_home, "opt/R/4.6.1$")
  expect_identical(names(.launcher_dirs()), c("4.4", "4.6"))
  # their icons
  apps <- .shortcut_dirs()$applications
  dir.create(apps, recursive = TRUE)
  i44 <- file.path(apps, .desktop_name("4.4"))
  i46 <- file.path(apps, .desktop_name("4.6"))
  file.create(c(i44, i46))
  writeLines(i44, file.path(d44, "shortcuts.txt"))
  writeLines(i46, file.path(d46, "shortcuts.txt"))
  # R 4.4 is gone (its R home, and no R 4.4 installed): the plan says so
  dead <- .dead_launchers(.launcher_dirs(), "4.6", installed = "4.6", os = "linux")
  expect_identical(names(dead), "4.4")
  expect_identical(dead[["4.4"]], i44)
  expect_length(.dead_launchers(.launcher_dirs(), "4.6", installed = c("4.4", "4.6"),
                                os = "linux"), 0L)
  txt <- paste(.shortcut_plan_text(data.frame(place = "x", folder = apps, name = "n",
                                              path = i46, exists = FALSE,
                                              stringsAsFactors = FALSE),
                                   "en", dead = dead), collapse = "\n")
  expect_match(txt, 'remove_shortcut(xy = "4.4")', fixed = TRUE)
  # remove_shortcut(xy = ) removes that one only
  suppressMessages(remove_shortcut(ask = FALSE, xy = "4.4"))
  expect_false(file.exists(i44))
  expect_false(dir.exists(d44))
  expect_true(file.exists(i46) && dir.exists(d46))
  # "all" removes every x.y
  suppressMessages(remove_shortcut(ask = FALSE, xy = "all"))
  expect_false(file.exists(i46))
  expect_false(dir.exists(d46))
})

test_that("add_shortcut() replaces the old icons and launcher, only its own, and says so", {
  p <- local_launcher()
  local_mocked_bindings(.os = function() "linux")
  apps <- .shortcut_dirs()$applications
  dir.create(apps, recursive = TRUE)
  old <- file.path(apps, "tflplanner.desktop")
  foreign <- file.path(apps, "notes.txt")
  file.create(c(old, foreign))
  ldir <- .legacy_launcher_dir()
  dir.create(ldir, recursive = TRUE)
  writeLines("0.0.1", file.path(ldir, "VERSION"))
  writeLines(c(old, foreign), file.path(ldir, "shortcuts.txt"))
  expect_true(.legacy_only())
  expect_message(made <- add_shortcut(ask = FALSE), "Removed the shortcuts made before")
  expect_identical(basename(made), .desktop_name())
  expect_true(file.exists(made))
  expect_false(file.exists(old))                       # the old icon is gone
  expect_true(file.exists(foreign))                    # what is not tflplanner's stays
  expect_false(dir.exists(ldir))
  expect_false(.legacy_only())
  # again in the same x.y: one icon, replaced
  suppressMessages(again <- add_shortcut(ask = FALSE))
  expect_identical(again, made)
  expect_identical(length(list.files(apps, "^tflplanner")), 1L)
  expect_identical(.read_launcher_conf(.launcher_dir())$r_home,
                   normalizePath(R.home(), "/", mustWork = FALSE))
})

test_that("the old launcher folder on Windows stays as a stub that names the new icon", {
  d <- withr_tempdir()
  ldir <- file.path(d, "launcher")
  dir.create(ldir)
  for (f in c("tflplanner.ico", "tflplanner.vbs", "launch.R", "VERSION", "shortcuts.txt")) {
    writeLines("x", file.path(ldir, f))
  }
  expect_true(.replace_legacy_launcher("windows", "4.6", "en", ldir))
  expect_setequal(list.files(ldir), c("tflplanner.ico", "tflplanner.vbs"))
  raw <- readBin(file.path(ldir, "tflplanner.vbs"), "raw", 1e5)
  expect_identical(raw[1:2], as.raw(c(0xFF, 0xFE)))
  vbs <- iconv(list(raw[-(1:2)]), "UTF-16LE", "UTF-8")
  expect_match(vbs, "tflplanner (R 4.6)", fixed = TRUE)
  expect_false(grepl("{{MSG}}", vbs, fixed = TRUE))
  # it is not the old launcher any more: the app does not call for add_shortcut()
  expect_false(.legacy_only(ldir, d))
  # elsewhere the folder goes
  expect_true(.replace_legacy_launcher("linux", "4.6", "en", ldir))
  expect_false(dir.exists(ldir))
  expect_false(.replace_legacy_launcher("linux", "4.6", "en", ldir))
  # only the old launcher folder, with no x.y beside it: the app says to run add_shortcut()
  dir.create(ldir)
  writeLines("0.0.1", file.path(ldir, "VERSION"))
  expect_true(.legacy_only(ldir, d))
  dir.create(file.path(d, "launcher-R4.6"))
  expect_false(.legacy_only(ldir, d))
})

test_that("the icons made before the R version was in the name are found, only tflplanner's own", {
  d <- withr_tempdir()
  dirs <- list(desktop = file.path(d, "Desktop"),
               start_menu = file.path(d, "Programs", "tflplanner"),
               applications = file.path(d, "apps"))
  for (x in dirs) dir.create(x, recursive = TRUE)
  old <- file.path(dirs$desktop, "tflplanner.lnk")
  new <- file.path(dirs$desktop, "tflplanner (R 4.6).lnk")
  file.create(c(old, new))
  expect_identical(.legacy_shortcuts("windows", dirs, "en", file.path(d, "none")), old)
  # a shortcuts.txt of the old launcher folder names the others -- the ones
  # that look like tflplanner's own icons
  ldir <- file.path(d, "launcher")
  dir.create(ldir)
  other <- file.path(d, "tflplanner-extra.lnk")
  foreign <- file.path(d, "report.docx")
  file.create(c(other, foreign))
  writeLines(c(other, foreign, file.path(d, "gone.lnk")), file.path(ldir, "shortcuts.txt"))
  expect_setequal(.legacy_shortcuts("windows", dirs, "en", ldir), c(old, other))
  expect_identical(.legacy_shortcuts("linux", dirs, "en", file.path(d, "none")), character())
  file.create(file.path(dirs$applications, "tflplanner.desktop"))
  expect_identical(basename(.legacy_shortcuts("linux", dirs, "en", file.path(d, "none"))),
                   "tflplanner.desktop")
  # the plan says they are removed
  plan <- data.frame(place = "Desktop", folder = dirs$desktop, name = "x",
                     path = new, exists = TRUE, stringsAsFactors = FALSE)
  txt <- paste(.shortcut_plan_text(plan, "en", legacy = old), collapse = "\n")
  expect_match(txt, "are removed", fixed = TRUE)
})

test_that("health carries the R x.y, and the launcher takes another x.y for another version", {
  d <- .serve_health()
  expect_true(paste("r", .r_xy()) %in% readLines(file.path(d, "health.txt")))
  src <- readLines(system.file("launcher", "launch.R", package = "tflplanner"))
  expect_true(any(grepl("same_build(h)", src, fixed = TRUE)))
  expect_true(any(grepl("identical(h$xy, my_xy)", src, fixed = TRUE)))
  expect_true(any(grepl('sub("^r ", "", grep("^r ", h', src, fixed = TRUE)))
})

# ------------------------------------------------------------- the running versions (#785)

test_that(".running_versions(): the loaded version; dev, tag, local and release installs told apart", {
  fake <- list(
    rtfreporter = list(Version = "0.9.0"),                          # CRAN: no mark
    tflspec = list(Version = "0.9.1", RemoteType = "github", RemoteRef = "main",
                   RemoteSha = "0123456789abcdef0123456789abcdef01234567"),
    tflplanner = list(Version = "0.9.2", RemoteType = "local",
                      Built = "R 4.6.1; ; 2026-11-20 12:00:00 UTC; windows"))
  run <- function(p) c(rtfreporter = "0.9.0", tflspec = "0.9.1", tflplanner = "0.9.3")[[p]]
  v <- .running_versions(desc = function(p) fake[[p]], running = run, r = "4.6.1")
  p <- v$packages
  expect_identical(p$package, c("rtfreporter", "tflspec", "tflplanner"))
  # the loaded version, not the DESCRIPTION on disk
  expect_identical(p$version, c("0.9.0", "0.9.1", "0.9.3"))
  expect_identical(p$kind, c(NA, "dev", "local"))
  expect_identical(p$ref, c(NA, "main", NA))
  expect_identical(p$sha, c(NA, "01234567", NA))
  expect_identical(p$built, c(NA, NA, "2026-11-20"))
  expect_identical(v$r, "4.6.1")
  expect_identical(
    .running_versions_text(v),
    paste0("rtfreporter 0.9.0, tflspec 0.9.1 (development version: main 01234567), ",
           "tflplanner 0.9.3 (local build, 2026-11-20), R 4.6.1"))
  expect_match(.running_versions_text(v, dev = "d", local = "l"), "(d: main 01234567)", fixed = TRUE)
  expect_match(.running_versions_text(v, dev = "d", local = "l"), "(l, 2026-11-20)", fixed = TRUE)
  # an @*release install (a tag), pak's standard install (the version as RemoteSha),
  # and an install of the default branch (HEAD)
  one <- function(d) .running_versions("x", desc = function(p) d, running = function(p) "1.0.0")$packages
  tag <- one(list(Version = "1.0.0", RemoteType = "github", RemoteRef = "v1.0.0",
                  RemoteSha = "89abcdef0123456789abcdef0123456789abcdef"))
  expect_identical(tag$kind, "tag")
  expect_identical(.running_versions_text(list(packages = tag, r = "4.6.1")), "x 1.0.0 (v1.0.0), R 4.6.1")
  std <- one(list(Version = "1.0.0", RemoteType = "standard", RemoteSha = "1.0.0"))
  expect_true(is.na(std$kind))
  expect_identical(.running_versions_text(list(packages = std, r = "4.6.1")), "x 1.0.0, R 4.6.1")
  head <- one(list(Version = "1.0.0", RemoteType = "github", RemoteRef = "HEAD",
                   RemoteSha = "89abcdef0123456789abcdef0123456789abcdef"))
  expect_identical(c(head$kind, head$ref, head$sha), c("dev", "HEAD", "89abcdef"))
  # a package that is not there: a "?"
  v2 <- .running_versions("tflspec", desc = function(p) NULL, running = function(p) NA_character_,
                          r = "4.5.0")
  expect_identical(.running_versions_text(v2), "tflspec ?, R 4.5.0")
})

test_that(".running_versions() of this process: the three packages and this R, read once", {
  real <- .running_versions()
  expect_identical(real$packages$package, c("rtfreporter", "tflspec", "tflplanner"))
  expect_identical(real$r, as.character(getRversion()))
  expect_false(anyNA(real$packages$version))
  expect_identical(real$packages$version[3L], as.character(getNamespaceVersion("tflplanner")))
  # the DESCRIPTION is read where the namespace is (a load_all folder too)
  d <- .running_description("tflplanner")
  expect_identical(d$Package, "tflplanner")
  # once: a later call gives the first answer
  old <- .run$v
  on.exit(.run$v <- old, add = TRUE)
  .run$v <- NULL
  n <- 0L
  local_mocked_bindings(.running_versions = function(...) { n <<- n + 1L; list(packages = n, r = "x") })
  a <- .running_versions_once()
  b <- .running_versions_once()
  expect_identical(a, b)
  expect_identical(n, 1L)
})

test_that("the page has an About button for the versions; the old-build note stays as it was", {
  html <- as.character(app_ui("en"))
  expect_match(html, 'id="about_app"', fixed = TRUE)
  expect_match(as.character(app_ui("ja")), "\u3053\u306e\u30a2\u30d7\u30ea\u306b\u3064\u3044\u3066", fixed = TRUE)
  local_home()
  two_studies()
  testthat::local_mocked_bindings(
    .stale_versions = function(...) list(tflplanner = c(running = "0.0.2.9070",
                                                        installed = "0.0.2.9088")))
  shiny::testServer(server_for("S1"), {
    h <- output$update_note$html
    expect_match(h, "A newer version is installed", fixed = TRUE)
    # the versions are in the About, not on every page
    expect_false(grepl("Versions this window runs", h, fixed = TRUE))
  })
})
