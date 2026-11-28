# The branches of R/launcher.R no other test reaches: the shortcut folders
# of each system, the R versions installed, launchers whose R has gone, the
# port in each kind of shortcut, add_shortcut() / remove_shortcut() asked
# and refused, and launch_app().  Everything is written to temporary
# folders (R_USER_CONFIG_DIR, option tflplanner.shortcut_dirs).

cov_launcher <- function(env = parent.frame()) {
  home <- local_home(env)
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
  list(home = home, shortcuts = sc, config = cfg)
}

test_that("the console language is Japanese when the settings say none and the locale is Japanese", {
  local_mocked_bindings(tflplanner_config = function(...) list())
  withr::local_envvar(LANG = "ja_JP.UTF-8")
  expect_identical(.console_language(), "ja")
})

test_that("a launcher file missing from the installation is an error that says to reinstall", {
  expect_error(.launcher_src("no-such-launcher-file.txt"),
               "no-such-launcher-file.txt is missing: reinstall tflplanner", fixed = TRUE)
})

test_that("on Windows launcher.conf is UTF-16 with a byte-order mark, and a non-ASCII R home reads back", {
  cov_launcher()
  local_mocked_bindings(.os = function() "windows")
  dir <- file.path(withr_tempdir(), "launcher")
  r_home <- file.path(withr_tempdir(), "R-\u30c6\u30b9\u30c8")
  .write_launcher(dir, lang = "en", xy = "4.6", r_home = r_home)
  raw <- readBin(file.path(dir, "launcher.conf"), "raw", 2L)
  expect_identical(raw, as.raw(c(0xFF, 0xFE)))
  cf <- .read_launcher_conf(dir)
  expect_identical(cf$r_xy, "4.6")
  expect_match(cf$r_home, "R-\u30c6\u30b9\u30c8", fixed = TRUE)
  # (Windows separators: the path is said as Windows writes it)
  expect_false(grepl("/", cf$r_home, fixed = TRUE))
})

test_that("the shortcut folders of macOS: ~/Applications", {
  withr::local_options(tflplanner.shortcut_dirs = NULL)
  expect_identical(.shortcut_dirs("mac"),
                   list(applications = path.expand("~/Applications")))
})

test_that("the shortcut folders of Linux: XDG_DATA_HOME's applications, else ~/.local/share's", {
  withr::local_options(tflplanner.shortcut_dirs = NULL)
  xdg <- withr_tempdir()
  withr::local_envvar(XDG_DATA_HOME = xdg)
  expect_identical(.shortcut_dirs("linux")$applications, file.path(xdg, "applications"))
  withr::local_envvar(XDG_DATA_HOME = "")
  expect_identical(.shortcut_dirs("linux")$applications,
                   file.path(path.expand("~/.local/share"), "applications"))
})

test_that("the shortcut folders of Windows without the registry: the user profile's Desktop and APPDATA's Start menu", {
  # (on Windows the registry is read: what it says depends on the machine)
  skip_on_os("windows")
  withr::local_options(tflplanner.shortcut_dirs = NULL)
  withr::local_envvar(USERPROFILE = "C:/Users/taro", APPDATA = "C:/Users/taro/AppData/Roaming")
  d <- .shortcut_dirs("windows")
  expect_identical(d$desktop, "C:/Users/taro/Desktop")
  expect_identical(d$start_menu, file.path("C:/Users/taro/AppData/Roaming", "Microsoft",
                                           "Windows", "Start Menu", "Programs", "tflplanner"))
})

test_that("the old macOS icons are found by name in Applications", {
  apps <- withr_tempdir()
  dir.create(file.path(apps, "tflplanner.app"))
  dir.create(file.path(apps, "tflplanner (update).app"))
  dir.create(file.path(apps, "tflplanner (R 4.6).app"))
  found <- .legacy_shortcuts("mac", list(applications = apps), "en",
                             dir = file.path(withr_tempdir(), "no-launcher"))
  expect_identical(basename(found), c("tflplanner.app", "tflplanner (update).app"))
})

test_that("the R versions installed on Windows: Program Files and the user's programs, one x.y each", {
  pf <- withr_tempdir()
  la <- withr_tempdir()
  dir.create(file.path(pf, "R", "R-4.5.2"), recursive = TRUE)
  dir.create(file.path(pf, "R", "R-4.5.1"), recursive = TRUE)
  dir.create(file.path(pf, "R", "Rtools"), recursive = TRUE)
  dir.create(file.path(la, "Programs", "R", "R-4.6.1"), recursive = TRUE)
  withr::local_envvar(ProgramFiles = pf, LOCALAPPDATA = la)
  expect_setequal(.installed_xys("windows"), c("4.5", "4.6"))
})

test_that("the R versions installed on macOS: an empty list when the framework is not there", {
  skip_if(dir.exists("/Library/Frameworks/R.framework/Versions"))
  expect_length(.installed_xys("mac"), 0L)
})

test_that("a launcher whose R has gone is dead: its recorded icons, or none when it kept no record", {
  cfg <- withr_tempdir()
  a <- file.path(cfg, "launcher-R3.9")
  b <- file.path(cfg, "launcher-R3.8")
  dir.create(a)
  dir.create(b)
  .write_text(.launcher_conf_text(file.path(cfg, "gone-R"), "3.9"), file.path(a, "launcher.conf"))
  .write_text(.launcher_conf_text(file.path(cfg, "gone-R"), "3.8"), file.path(b, "launcher.conf"))
  writeLines(file.path(cfg, "Desktop", "tflplanner (R 3.9).lnk"), file.path(a, "shortcuts.txt"))
  dead <- .dead_launchers(c(`3.9` = a, `3.8` = b), xy = "4.6", installed = character(), os = "linux")
  expect_setequal(names(dead), c("3.9", "3.8"))
  expect_identical(basename(dead[["3.9"]]), "tflplanner (R 3.9).lnk")
  expect_identical(dead[["3.8"]], character())
  # an x.y still installed is not dead, whatever its record says
  expect_length(.dead_launchers(c(`3.9` = a), xy = "4.6", installed = "3.9", os = "linux"), 0L)
})

test_that("a launcher that remembers no R is dead on every system, though /bin/Rscript be there (#900)", {
  cfg <- withr_tempdir()
  a <- file.path(cfg, "launcher-R3.8")
  b <- file.path(cfg, "launcher-R3.7")
  dir.create(a)
  dir.create(b)
  # (a: no launcher.conf at all; b: a launcher.conf without its r_home)
  .write_text("r_xy=3.7", file.path(b, "launcher.conf"))
  for (os in c("linux", "mac", "windows")) {
    dead <- .dead_launchers(c(`3.8` = a, `3.7` = b), xy = "4.6",
                            installed = character(), os = os)
    expect_setequal(names(dead), c("3.8", "3.7"))
    expect_identical(dead[["3.8"]], character())
  }
  # an x.y still installed is not dead, as for a launcher with its conf
  expect_length(.dead_launchers(c(`3.8` = a), xy = "4.6", installed = "3.8", os = "linux"), 0L)
  # a launcher whose R is there is not dead
  r <- file.path(cfg, "R-3.6")
  dir.create(file.path(r, "bin"), recursive = TRUE)
  file.create(file.path(r, "bin", "Rscript"))
  c36 <- file.path(cfg, "launcher-R3.6")
  dir.create(c36)
  .write_text(.launcher_conf_text(r, "3.6"), file.path(c36, "launcher.conf"))
  expect_length(.dead_launchers(c(`3.6` = c36), xy = "4.6", installed = character(), os = "linux"), 0L)
})

test_that("the port is in every kind of shortcut: Windows, macOS and Linux", {
  dirs <- list(desktop = "C:/u/Desktop", start_menu = "C:/u/Programs/tflplanner")
  sc <- .windows_shortcuts("C:/cfg/launcher", dirs, "en", port = 7480L, xy = "4.6")
  expect_true(all(endsWith(sc$args, " --port=7480")))
  apps <- .mac_app_files("/cfg/launcher", "/Users/u/Applications", "en", port = 7480L, xy = "4.6")
  exe <- unlist(lapply(apps, function(a) vapply(a$files, function(f) f$text %||% "", "")))
  expect_true(any(grepl("--port=7480", exe, fixed = TRUE)))
  de <- .desktop_file("/cfg/launcher", "en", port = 7480L, xy = "4.6")
  expect_match(de[startsWith(de, "Exec=")], " --port=7480$")
})

test_that("Windows: an update shortcut with no Start menu goes on the desktop", {
  dirs <- list(desktop = "C:/u/Desktop", start_menu = "C:/u/Programs/tflplanner")
  sc <- .windows_shortcuts("C:/cfg/launcher", dirs, "en", desktop = TRUE,
                           start_menu = FALSE, update = TRUE, xy = "4.6")
  expect_identical(nrow(sc), 2L)
  expect_true(all(dirname(sc$path) == "C:/u/Desktop"))
  expect_identical(sc$args[2], paste0('"', .win_path("C:/cfg/launcher/tflplanner.vbs"), '" --update'))
})

test_that("Windows: no desktop and no Start menu is no plan", {
  dirs <- list(desktop = "C:/u/Desktop", start_menu = "C:/u/Programs/tflplanner")
  expect_null(.shortcut_plan("windows", dirs, "en", desktop = FALSE, start_menu = FALSE,
                             xy = "4.6", dir = "C:/cfg/launcher"))
})

test_that("add_shortcut() asked with nothing to make says so and makes nothing", {
  cov_launcher()
  local_mocked_bindings(.os = function() "windows")
  expect_message(made <- add_shortcut(desktop = FALSE, start_menu = FALSE, ask = TRUE),
                 "Nothing to make")
  expect_identical(made, character())
})

test_that("add_shortcut() with `ask` neither TRUE nor FALSE stops before making anything", {
  cov_launcher()
  expect_error(add_shortcut(ask = NA), "asks before it makes anything")
  expect_false(dir.exists(.launcher_dir()))
})

test_that("add_shortcut() on a first run sets tflplanner up in the console's language", {
  cov_launcher()
  local_mocked_bindings(.os = function() "linux", .is_set_up = function(...) FALSE)
  got <- NULL
  local_mocked_bindings(setup_tflplanner = function(...) got <<- list(...))
  suppressMessages(add_shortcut(ask = FALSE))
  expect_identical(names(got), "language")
})

test_that("add_shortcut() on macOS writes the app bundles, a bundle already there replaced", {
  p <- cov_launcher()
  local_mocked_bindings(.os = function() "mac")
  stale <- file.path(p$shortcuts, "applications", paste0(.shortcut_base(), ".app"), "old.txt")
  dir.create(dirname(stale), recursive = TRUE)
  writeLines("old", stale)
  made <- suppressMessages(add_shortcut(ask = FALSE))
  expect_identical(basename(made), paste0(.shortcut_base(), ".app"))
  expect_false(file.exists(stale))
  expect_true(file.exists(file.path(made, "Contents", "Info.plist")))
})

test_that("remove_shortcut() asked and refused removes nothing", {
  cov_launcher()
  local_mocked_bindings(.os = function() "linux")
  made <- suppressMessages(add_shortcut(ask = FALSE))
  local_mocked_bindings(.ask_yes = function(question) FALSE)
  out <- capture.output(expect_message(gone <- remove_shortcut(ask = TRUE),
                                       "Nothing was removed"))
  expect_identical(gone, character())
  expect_true(file.exists(made))
  expect_true(any(grepl("will remove", out, fixed = TRUE)))
})

test_that("remove_shortcut() with `ask` neither TRUE nor FALSE stops before removing anything", {
  cov_launcher()
  local_mocked_bindings(.os = function() "linux")
  made <- suppressMessages(add_shortcut(ask = FALSE))
  expect_error(remove_shortcut(ask = NA), "asks before it removes anything")
  expect_true(file.exists(made))
})

test_that("remove_shortcut() on Windows removes the Start menu folder it leaves empty, not one with other files", {
  p <- cov_launcher()
  local_mocked_bindings(.os = function() "windows")
  dir <- .write_launcher(lang = "en")
  sm <- file.path(p$shortcuts, "Programs", "tflplanner")
  other <- file.path(p$shortcuts, "Other", "tflplanner")
  dir.create(sm, recursive = TRUE)
  dir.create(other, recursive = TRUE)
  lnk <- c(file.path(sm, "a.lnk"), file.path(other, "b.lnk"))
  file.create(lnk)
  writeLines("keep", file.path(other, "readme.txt"))
  writeLines(lnk, file.path(dir, "shortcuts.txt"))
  suppressMessages(remove_shortcut(ask = FALSE))
  expect_false(dir.exists(sm))
  expect_true(dir.exists(other))
  expect_false(file.exists(lnk[2]))
})

test_that("launch_app() checks the port, writes the launcher, starts launch.R and says the address", {
  cov_launcher()
  expect_error(launch_app(port = 80), "from 1024 to 65535")
  got <- NULL
  local_mocked_bindings(process = list(new = function(command, args, ...) {
    got <<- list(command = command, args = args)
    invisible(NULL)
  }), .package = "processx")
  expect_message(url <- launch_app(port = 7481), "http://127.0.0.1:7481/", fixed = TRUE)
  expect_identical(url, "http://127.0.0.1:7481/")
  expect_identical(basename(got$args[1]), "launch.R")
  expect_identical(got$args[2], "--port=7481")
  expect_true(file.exists(got$args[1]))
})
