# Screenshots of the Plot Designer for vignettes/articles/ja-figures.Rmd.
# Run with NOT_CRAN=true from the package folder, e.g.
#   NOT_CRAN=true Rscript data-raw/make-article-screenshots.R vignettes/articles/figures
# args: [1] output folder, [2] optional library (tflplanner to show).
a <- commandArgs(TRUE)
out <- a[1]
if (length(a) > 1) .libPaths(c(a[2], .libPaths()))
suppressWarnings(suppressPackageStartupMessages(library(tflplanner)))
dir.create(out, showWarnings = FALSE, recursive = TRUE)
base <- file.path(tempdir(), "shots")
home <- file.path(base, "home"); root <- file.path(base, "studies")
unlink(base, recursive = TRUE); dir.create(root, recursive = TRUE)
# the user's own settings (tools::R_user_dir(): the home tflplanner opens,
# the launcher) are not this script's: it keeps its own
Sys.setenv(R_USER_CONFIG_DIR = file.path(base, "config"),
           R_USER_DATA_DIR = file.path(base, "data"),
           R_USER_CACHE_DIR = file.path(base, "cache"))
setup_tflplanner(home = home, studies_root = root, language = "ja")
options(tflplanner.home = home)
create_sample_study(root = root, run = FALSE, home = home)

# the article's two new figures: F-14-2-4 (chapter 3, made on screen below)
# and F-14-2-5 (chapter 4: the mean-over-time design with the n table)
st <- open_study("SAMPLE-01", home = home)
st$planner <- add_output(st$planner, "F-14-2-4", type = "figure",
                         description = "KM curves, made in this guide")
st$planner <- add_output(st$planner, "F-14-2-5", type = "figure",
                         description = "Mean change from baseline in systolic blood pressure")
mn <- tflspec::tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP", pop = "SAFFL",
                                group = "TRT01A", value = "CHG", join_adsl = TRUE)
mn$plot$y_label <- "Mean change from baseline (+/- SE), mmHg"
adv <- tflspec::tfl_fig_advice(mn)
mn <- tflspec::tfl_fig_apply_fix(mn, adv$fix[[which(adv$rule == "mean_n")]])
mn$plot$height <- 5
mn$template <- NULL   # chapter 4 builds it from empty: no template named
st$planner <- set_fig_design(st$planner, "F-14-2-5", mn)
save_study(st, home = home)

app <- planner_app()
d <- shinytest2::AppDriver$new(app, width = 1600, height = 1150, load_timeout = 90000,
                               options = list(tflplanner.home = home))
# the temporary folders shown on the page, as a reader's would be
mask <- function() {
  paths <- unique(c(normalizePath(root, winslash = "/", mustWork = FALSE),
                    normalizePath(root, winslash = "\\", mustWork = FALSE), root))
  js <- sprintf("(function(ps){var w=document.createTreeWalker(document.body,NodeFilter.SHOW_TEXT);var n;while(n=w.nextNode()){ps.forEach(function(p){if(n.nodeValue.indexOf(p)>=0)n.nodeValue=n.nodeValue.split(p).join('C:/studies');});}})(%s)",
                jsonlite::toJSON(paths))
  d$run_js(js)
}
shot <- function(name, selector = NULL, wait = 1) {
  Sys.sleep(wait)
  mask()
  f <- file.path(out, paste0(name, ".png")); unlink(f)
  if (is.null(selector)) d$get_screenshot(f) else d$get_screenshot(f, selector = selector)
  message("wrote ", f)
}
img_ready <- function(timeout = 90000) {
  d$wait_for_js("document.querySelector('#pd_img img') !== null && document.querySelector('#pd_img img').complete",
                timeout = timeout)
  Sys.sleep(1.5)
}
# the editor's input for a field of the piece chosen (its id carries a counter)
field_id <- function(label) {
  d$get_js(sprintf(
    "(function(){var l=Array.from(document.querySelectorAll('label')).find(function(x){return x.innerText.indexOf('%s')===0;});return l?l.getAttribute('for'):null;})()",
    label))
}
Sys.sleep(5)
# ---- 1.2: the Studies tab (the last study, SAMPLE-01, open and chosen)
shot("fig-studies")

# ---- 3.1: F-14-2-4's first step, km_simple with ADTTE's values
d$set_inputs(nav = "outputs", wait_ = FALSE); Sys.sleep(2)
d$set_inputs(target = "F-14-2-4", wait_ = FALSE); Sys.sleep(2)
d$set_inputs(rep_nav = "content", wait_ = FALSE); Sys.sleep(3)
d$set_inputs(pd_tpl = "km_simple", wait_ = FALSE); Sys.sleep(1)
d$set_inputs(pd_tpl_data = "ADTTE", wait_ = FALSE); Sys.sleep(3)
d$set_inputs(pd_tpl_param = "TTDE", wait_ = FALSE); Sys.sleep(0.5)
d$set_inputs(pd_tpl_pop = "SAFFL", wait_ = FALSE); Sys.sleep(0.5)
d$set_inputs(pd_tpl_group = "TRT01A", wait_ = FALSE); Sys.sleep(0.5)
d$set_inputs(pd_tpl_unit = "days", wait_ = FALSE)
d$set_window_size(1600, 2400); Sys.sleep(2)
shot("fig-start", selector = ".card:has(#pd_start)", wait = 2)
d$set_window_size(1600, 1150); Sys.sleep(1)

# ---- 2.2 / 3.2: applied, then the median line added in the designer
d$click("pd_start", wait_ = FALSE)
img_ready()
d$set_inputs(pd_add = "hline", wait_ = FALSE); Sys.sleep(1)
d$click("pd_addbtn", wait_ = FALSE); Sys.sleep(3)
y_id <- field_id("y \u306e\u4f4d\u7f6e")            # "y の位置"
do.call(d$set_inputs, stats::setNames(list("0.5", FALSE), c(y_id, "wait_")))
Sys.sleep(3); img_ready()
shot("fig-designer")

# ---- 3.4: advice after removing the censor marks (layers[2])
d$set_inputs(pd_act = list(op = "del", sec = "layers", i = 2L, n = runif(1)),
             allow_no_input_binding_ = TRUE, wait_ = FALSE)
Sys.sleep(3); img_ready()
shot("fig-advice")

# ---- 4: the mean-over-time figure, designed (F-14-2-5)
d$set_inputs(target = "F-14-2-5", wait_ = FALSE)
Sys.sleep(3); img_ready()
shot("fig-mean")

d$stop()
