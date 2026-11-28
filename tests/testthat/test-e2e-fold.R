# [Collapse all] / [Expand all] in a browser (#835): headless Chrome
# (chromote) on the app started here, on a free port, with a home of its
# own.  Not on CRAN, not without Chrome and chromote, and only when asked:
# TFLPLANNER_E2E=true.  Every step: Collapse all folds its cards to their
# title lines, Expand all opens them, and neither sends a value to the app.

test_that("Collapse all / Expand all in each step of the report screen", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("TFLPLANNER_E2E"), "true"), "TFLPLANNER_E2E is not true")
  skip_if_not_installed("chromote")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "no Chrome")
  `%||%` <- function(a, b) if (is.null(a)) b else a
  base <- tempfile("tflplanner-e2e")
  dir.create(base)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  port <- httpuv::randomPort()
  code <- sprintf(paste(
    "Sys.setenv(R_USER_CONFIG_DIR = '%1$s/config', R_USER_DATA_DIR = '%1$s/data',",
    "R_USER_CACHE_DIR = '%1$s/cache', TFLPLANNER_HOME = '%1$s/home');",
    "options(tflplanner.home = '%1$s/home');",
    "suppressMessages(tflplanner::setup_tflplanner(studies_root = '%1$s/studies', language = 'en'));",
    "suppressMessages(tflplanner::create_sample_study(run = FALSE));",
    "tflplanner::run_app('SAMPLE-01', port = %2$dL, launch.browser = FALSE)"),
    normalizePath(base, "/"), port)
  app <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", code),
                               env = c("current", R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
                               stdout = "|", stderr = "2>&1")
  on.exit(app$kill(), add = TRUE)
  url <- sprintf("http://127.0.0.1:%d", port)
  up <- FALSE
  for (i in 1:120) {
    if (!app$is_alive()) break
    up <- tryCatch({
      con <- url(url)
      ok <- tryCatch({ readLines(con, n = 1L, warn = FALSE); TRUE }, finally = close(con))
      ok
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (up) break
    Sys.sleep(1)
  }
  skip_if_not(up, "the app did not start")
  spec <- file.path(base, "studies", "SAMPLE-01", "spec")
  spec_md5 <- function() {
    f <- list.files(spec, recursive = TRUE, full.names = TRUE)
    unname(tools::md5sum(f))
  }

  b <- chromote::ChromoteSession$new(width = 1100, height = 900)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  js <- function(code, timeout = 60) {
    r <- b$Runtime$evaluate(code, awaitPromise = TRUE, returnByValue = TRUE, timeout_ = timeout)
    if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% "error")
    r$result$value
  }
  wait_for <- function(cond, secs = 30) {
    js(sprintf("(async () => { const t0 = Date.now(); while (Date.now() - t0 < %d) { try { if (%s) return true; } catch (e) {} await new Promise(r => setTimeout(r, 200)); } return false; })()",
               secs * 1000, cond), timeout = secs + 10)
  }
  idle <- function() {
    Sys.sleep(0.8)
    wait_for("!document.documentElement.classList.contains('shiny-busy')", 60)
    Sys.sleep(0.8)
  }

  invisible(b$Page$navigate(url))
  Sys.sleep(3)
  wait_for("window.$ && window.Shiny && Shiny.setInputValue && $('#target').length && $('#target')[0].selectize", 60)
  Sys.sleep(2)
  js("(() => { $('.modal.show button').last().click(); return true; })()")
  Sys.sleep(1)
  js("(() => { $('#target')[0].selectize.setValue('T-14-1-1'); return true; })()")
  Sys.sleep(1)
  js("(() => { $('#nav a[data-value=\"make\"]').tab('show'); return true; })()")
  idle()
  before <- spec_md5()
  # the values sent to the app (cleared before each step's clicks)
  js("(() => { window.rpSent = []; $(document).on('shiny:inputchanged.foldtest', function(e) {
       if (!/^\\./.test(e.name)) window.rpSent.push(e.name); }); return true; })()")
  # the step's cards: the open ones, the folded ones (visible cards only)
  state <- function(st) {
    js(sprintf("(() => { const s = $('.rp-fold-scope[data-fold-stage=%s]');
      const rp = s.find('.rp-card:visible'), gen = s.find('.card:visible, .pg-card:visible, .rp-b-card:visible')
        .filter(function() { return $(this).children().first().is('.card-header, h6') &&
                                    !$(this).children().first().find('.nav').length &&
                                    !$(this).parents('.card, .pg-card, .rp-b-card, .rp-card').length; });
      return {rp: rp.length, rp_open: rp.filter('.rp-card-open').length,
              rp_body: rp.find('.rp-card-body:visible').length,
              gen: gen.length, gen_closed: gen.filter('.rp-fold-closed').length,
              gen_rest: gen.filter('.rp-fold-closed').children(':visible').not(':first-child').length,
              kept: rp.filter('.rp-card-error').length}; })()", st))
  }
  steps <- c("data", "ard", "content", "page")
  seen <- 0L
  for (st in steps) {
    js(sprintf("(() => { $('#step a[data-value=\"%s\"]').tab('show'); return true; })()", st))
    idle()
    expect_true(isTRUE(js(sprintf("$('.rp-fold-scope[data-fold-stage=%s] > .rp-fold-bar:visible').length == 1", st))),
                info = st)
    js("(() => { window.rpSent = []; return true; })()")
    js(sprintf("(() => { $('.rp-fold-scope[data-fold-stage=%s] > .rp-fold-bar [data-fold=close]').click(); return true; })()", st))
    Sys.sleep(0.5)
    s <- state(st)
    seen <- seen + s$rp + s$gen
    # every card its title line (a card with an error stays open)
    expect_identical(s$rp_open, s$kept, info = st)
    expect_identical(s$gen_closed, s$gen, info = st)
    expect_identical(s$gen_rest, 0L, info = st)
    if (s$kept == 0L) expect_identical(s$rp_body, 0L, info = st)
    js(sprintf("(() => { $('.rp-fold-scope[data-fold-stage=%s] > .rp-fold-bar [data-fold=open]').click(); return true; })()", st))
    Sys.sleep(0.5)
    s <- state(st)
    expect_identical(s$rp_open, s$rp, info = st)
    expect_identical(s$gen_closed, 0L, info = st)
    # the two clicks sent no value to the app
    sent <- unlist(js("window.rpSent"))
    expect_identical(length(sent), 0L, info = paste(st, ":", paste(sent, collapse = ", ")))
  }
  # there were cards to fold
  expect_gt(seen, 0L)
  # nothing written
  expect_identical(spec_md5(), before)
})
