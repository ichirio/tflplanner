# Screenshots of the app (English) for vignettes/articles/guide.Rmd and the
# README.  Run with NOT_CRAN=true from the package folder, e.g.
#   NOT_CRAN=true Rscript data-raw/make-guide-screenshots.R vignettes/articles/figures
# args: [1] output folder, [2] optional library (tflplanner to show).
# The sample study is made whole (its ARD and reports: about a minute), so
# the screens show the ARD made and the table as it prints.
a <- commandArgs(TRUE)
out <- a[1]
if (length(a) > 1) .libPaths(c(a[2], .libPaths()))
suppressWarnings(suppressPackageStartupMessages(library(tflplanner)))
dir.create(out, showWarnings = FALSE, recursive = TRUE)
base <- file.path(tempdir(), "guide-shots")
home <- file.path(base, "home"); root <- file.path(base, "studies")
unlink(base, recursive = TRUE); dir.create(root, recursive = TRUE)
# the user's own settings (tools::R_user_dir(): the home tflplanner opens,
# the launcher) are not this script's: it keeps its own
Sys.setenv(R_USER_CONFIG_DIR = file.path(base, "config"),
           R_USER_DATA_DIR = file.path(base, "data"),
           R_USER_CACHE_DIR = file.path(base, "cache"))
setup_tflplanner(home = home, studies_root = root, language = "en")
options(tflplanner.home = home)
create_sample_study(root = root, run = TRUE, home = home)

# what the guide shows beyond the sample: the study's font (the study tab),
# and a column made in T-14-2-1's analysis data (2-1's columns made)
st <- open_study("SAMPLE-01", home = home)
st$planner <- set_study_page_value(st$planner, "font", "Courier New")
st$planner <- set_study_page_value(st$planner, "font_size_half_points", "20")
st$planner <- set_analysis_data(
  st$planner, "T-14-2-1", "advs_w24", from = "ADVS",
  where = 'PARAMCD == "SYSBP" & AVISIT == "Week 24"',
  derive = 'AGEGRP = cut(AGE, c(-Inf, 65, 75, Inf), c("<65", "65-74", ">=75"), right = FALSE)',
  old = "advs_w24")
save_study(st, home = home)

app <- planner_app()
d <- shinytest2::AppDriver$new(app, width = 1500, height = 1100, load_timeout = 90000,
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
nav <- function(value) {
  d$run_js(sprintf("(function(){var a=[...document.querySelectorAll('a.nav-link')].find(function(x){return x.getAttribute('data-value')===%s && x.offsetParent;}); if(a) a.click();})()",
                   jsonlite::toJSON(value, auto_unbox = TRUE)))
}
pick <- function(input, value) {
  d$run_js(sprintf("Shiny.setInputValue(%s, %s, {priority: 'event'});",
                   jsonlite::toJSON(input, auto_unbox = TRUE),
                   jsonlite::toJSON(value, auto_unbox = TRUE)))
}
report <- function(id) {
  d$run_js(sprintf("(function(){var s=document.getElementById('target'); if(s && s.selectize) s.selectize.setValue(%s);})()",
                   jsonlite::toJSON(id, auto_unbox = TRUE)))
}
tall <- function(h) { d$set_window_size(1500, h); Sys.sleep(1.5) }
Sys.sleep(6)

# ---- the study tab: SAMPLE-01 open; every report's header and font
shot("guide-study")

# ---- the data tab: the study's files
nav("data"); shot("guide-data", wait = 4)

# ---- step 1: this report's code lists, and the copy dialog
nav("make"); Sys.sleep(2); report("T-14-1-1"); Sys.sleep(3)
nav("codelist"); Sys.sleep(4)
tall(1500); shot("guide-step1-codelists", wait = 2)
d$click("cl_copy", wait_ = FALSE); Sys.sleep(3)
shot("guide-step1-copy", selector = ".modal-dialog", wait = 1)
d$run_js("document.querySelector('.modal.show .btn-close, .modal.show [data-bs-dismiss=modal]').click()")
Sys.sleep(2)

# ---- step 2: 2-1 the analysis data (T-14-2-1's advs_w24 with its column made)
report("T-14-2-1"); Sys.sleep(3)
nav("ard"); Sys.sleep(4)
pick("ard_adata_pick", "advs_w24"); Sys.sleep(4)
tall(2200); shot("guide-step2-adata", selector = "#step .tab-pane.active", wait = 2)

# ---- step 2: 2-2 an analysis (T-14-1-1's CONT)
report("T-14-1-1"); Sys.sleep(3)
pick("ard_adata_pick", "adsl_saf"); Sys.sleep(2)   # close 2-1's form (a second click)
pick("ard_adata_pick", "adsl_saf"); Sys.sleep(2)
d$run_js("(function(){var c=[...document.querySelectorAll('#step .tab-pane.active *')].find(function(x){return x.innerText && x.innerText.trim()==='CONT' && x.children.length===0;}); if(c) c.click();})()")
Sys.sleep(4)
shot("guide-step2-analysis", selector = "#step .tab-pane.active", wait = 2)

# ---- step 3: the table builder's statistics and the table as it prints
nav("content"); Sys.sleep(6)
tall(2600); shot("guide-step3-stats", selector = "#step .tab-pane.active", wait = 3)

# ---- step 4: this report's font, over every report's
nav("page"); Sys.sleep(4)
tall(1500); shot("guide-step4-font", selector = "#step .tab-pane.active", wait = 2)
tall(1100)

# ---- the report list and the runs
nav("outputs"); shot("guide-reports", wait = 4)
nav("results"); shot("guide-runs", wait = 4)

d$stop()
