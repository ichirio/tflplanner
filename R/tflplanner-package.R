#' tflplanner: a Shiny study manager for clinical TFLs
#'
#' The GUI over tflspec (the Excel specifications, ARD and plan engine) and
#' rtfreporter (the RTF renderer).
#'
#' @section Start here:
#' `setup_tflplanner()` once: where the app keeps its settings and where
#' new study folders go (its home, under [tools::R_user_dir()] unless you
#' choose another).  Then `run_app()` opens the app.  `create_sample_study()`
#' makes a study to try it on: CDISC pilot data with tables, a listing
#' and figures, ready to run.
#'
#' @keywords internal
#' @importFrom rtfreporter listing_spec
"_PACKAGE"
