# The tests never write the user's own settings: tools::R_user_dir() (the
# home tflplanner opens, the launcher) is a temporary folder for the run.
local({
  d <- tempfile("tflplanner-user-dirs")
  old <- Sys.getenv(c("R_USER_CONFIG_DIR", "R_USER_DATA_DIR", "R_USER_CACHE_DIR"),
                    unset = NA)
  Sys.setenv(R_USER_CONFIG_DIR = file.path(d, "config"),
             R_USER_DATA_DIR = file.path(d, "data"),
             R_USER_CACHE_DIR = file.path(d, "cache"))
  withr::defer({
    for (k in names(old)) {
      if (is.na(old[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, as.list(old[k]))
    }
    unlink(d, recursive = TRUE)
  }, envir = testthat::teardown_env())
})
