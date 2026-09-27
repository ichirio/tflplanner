# The developer's test studies (ICHIRIO-001, ICHIRIO-002 ...) live in a
# tflplanner home of their own -- never in the default home a user sets up
# after installing the package, and never in the sample study that ships
# with it (make-sample-study.R builds that one in a temporary home).
#
# Name the development home first, e.g. on the maintainer's PC:
#
#   Sys.setenv(TFLPLANNER_HOME = "C:/Yrepo/tflplanner-dev-home")
#   (its studies go to C:/Yrepo/studies)
#
# or, in a shell:  TFLPLANNER_HOME=C:/Yrepo/tflplanner-dev-home Rscript ...

if (is.null(getOption("tflplanner.home")) &&
    !nzchar(Sys.getenv("TFLPLANNER_HOME"))) {
  stop("The development scripts need the development home: set ",
       "TFLPLANNER_HOME (see data-raw/dev-home.R).  They do not use the ",
       "default home a user sets up after installing tflplanner.",
       call. = FALSE)
}
if (!.is_set_up()) {
  stop("No tflplanner home at ", tflplanner_home(), ": set it up first ",
       "(setup_tflplanner(studies_root = ...)).", call. = FALSE)
}
message("Development home: ", tflplanner_home(),
        "\nDevelopment studies go to: ", studies_root())
