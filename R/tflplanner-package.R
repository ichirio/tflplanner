#' tflplanner: a Shiny study manager for clinical TFLs
#'
#' The GUI over tflspec (the study's YAML specifications, its templates and
#' the R code written from them) and rtfreporter (the ARD and table engine
#' and the RTF renderer).
#'
#' @section Start here:
#' `setup_tflplanner()` once: where the app keeps its settings and where
#' new study folders go (its home, under [tools::R_user_dir()] unless you
#' choose another).  Then `run_app()` opens the app.  `create_sample_study()`
#' makes a study to try it on: tflspec's oncology sample data with tables,
#' a listing and figures, ready to run.
#'
#' @keywords internal
#' @importFrom rtfreporter listing_spec
"_PACKAGE"
