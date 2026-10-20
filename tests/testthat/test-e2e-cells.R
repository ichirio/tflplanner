# plan_cells() by variable in a browser, as a user clicks it (#454):
# headless Chrome (chromote) on the app started here, on a free port, with
# a home of its own.  Not on CRAN, not without Chrome and chromote, and
# only when asked: TFLPLANNER_E2E=true.  (a) a variable's own template
# edited: written into the report's YAML; (b) a condition added in its
# fields: written as !r and read back into them; (c) the decimals changed:
# the condition kept, its decimals rewritten; (d) the company standard
# copied into the study's: the ticked item only.

test_that("plan_cells() by variable in the browser: own template, condition, decimals, company copy", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("TFLPLANNER_E2E"), "true"), "TFLPLANNER_E2E is not true")
  skip_if_not_installed("chromote")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "no Chrome")
  `%||%` <- function(a, b) if (is.null(a)) b else a
  # the app: its own R, its own home (never the user's), a free port
  base <- tempfile("tflplanner-e2e")
  dir.create(base)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  port <- httpuv::randomPort()
  code <- sprintf(paste(
    "Sys.setenv(R_USER_CONFIG_DIR = '%1$s/config', R_USER_DATA_DIR = '%1$s/data',",
    "R_USER_CACHE_DIR = '%1$s/cache', TFLPLANNER_HOME = '%1$s/home');",
    "options(tflplanner.home = '%1$s/home');",
    "suppressMessages(tflplanner::setup_tflplanner(studies_root = '%1$s/studies', language = 'ja'));",
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
  yml <- function(f) paste(readLines(file.path(spec, f), encoding = "UTF-8", warn = FALSE), collapse = "\n")

  b <- chromote::ChromoteSession$new(width = 1280, height = 900)
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
  # a field's value set as typed: the change event the page listens to
  set_field <- function(selector, value) {
    js(sprintf("(() => { const e = document.querySelector(%s); e.focus(); e.value = %s;
      e.dispatchEvent(new Event('change', {bubbles: true})); return true; })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE), jsonlite::toJSON(value, auto_unbox = TRUE)))
    idle()
  }
  click <- function(selector) {
    js(sprintf("(() => { const e = document.querySelector(%s); e.scrollIntoView({block: 'center'}); e.click(); return true; })()",
               jsonlite::toJSON(selector, auto_unbox = TRUE)))
    idle()
  }
  save <- function() {
    click("#save")
    Sys.sleep(2)
    idle()
  }
  res <- list()
  check <- function(name, ok, detail = "") {
    res[[name]] <<- isTRUE(ok)
    testthat::expect_true(isTRUE(ok), info = paste(name, detail))
  }

  invisible(b$Page$navigate(url))
  Sys.sleep(3)
  wait_for("window.$ && window.Shiny && Shiny.setInputValue && $('#target').length && $('#target')[0].selectize", 60)
  Sys.sleep(2)
  js("(() => { $('.modal.show button').last().click(); return true; })()")
  Sys.sleep(1)
  # T-14-2-2 (a KM table: prob, time of their own), its builder
  js("(() => { $('#target')[0].selectize.setValue('T-14-2-2'); return true; })()")
  Sys.sleep(1)
  js("(() => { $('#nav a[data-value=\"make\"]').tab('show'); return true; })()")
  Sys.sleep(1)
  js("(() => { $('#step a[data-value=\"content\"]').tab('show'); return true; })()")
  wait_for("$('.rp-cv').length > 0 || $('#fetch3').length > 0", 60)
  if (isTRUE(js("$('#fetch3').length > 0"))) click("#fetch3")
  check("the plan_cells() card's table", wait_for("$('.rp-cv-row[data-var=time]').length > 0", 240))
  idle()
  row <- function(v) sprintf(".rp-cv-row[data-var=%s]", v)

  # (a) time's own template edited: in its YAML once saved
  set_field(paste(row("time"), "input.rp-cv-tpl[data-k='0']"), "{estimate:.1f%}")
  save()
  check("(a) the own template written", grepl("time: '{estimate:.1f%}'", yml("outputs/T-14-2-2.yml"), fixed = TRUE),
        yml("outputs/T-14-2-2.yml"))

  # (b) a condition: added, its fields set, written as !r, read back
  click(paste(row("time"), "button[data-act=cond_add]"))
  wait_for(sprintf("$('%s .rp-cv-cond').length > 0", row("time")), 20)
  js(sprintf("(() => { const s = document.querySelector('%s .rp-cv-cond select[data-f=col]');
    s.value = 'estimate'; s.dispatchEvent(new Event('change', {bubbles: true})); return true; })()", row("time")))
  idle()
  js(sprintf("(() => { const s = document.querySelector('%s .rp-cv-cond select[data-f=op]');
    s.value = '<'; s.dispatchEvent(new Event('change', {bubbles: true})); return true; })()", row("time")))
  idle()
  set_field(paste(row("time"), ".rp-cv-cond input[data-f=value]"), "0.5")
  set_field(paste(row("time"), ".rp-cv-cond input[data-f=tpl]"), "<{mean:.1f}")
  save()
  y <- yml("outputs/T-14-2-2.yml")
  check("(b) the condition written as !r", grepl("!r estimate < 0.5 ~ \"<{mean:.1f}\"", y, fixed = TRUE), y)
  # (the form drawn again -- another report, then this one: the fields
  # read back from the YAML)
  js("(() => { $('#target')[0].selectize.setValue('T-14-2-1'); return true; })()")
  idle()
  wait_for(sprintf("$('%s').length == 0", row("time")), 60)
  js("(() => { $('#target')[0].selectize.setValue('T-14-2-2'); return true; })()")
  idle()
  wait_for(sprintf("$('%s .rp-cv-cond input[data-f=value]').length > 0", row("time")), 120)
  back <- js(sprintf("(() => { const c = document.querySelector('%s .rp-cv-cond');
    return [c.querySelector('[data-f=col]').value, c.querySelector('[data-f=op]').value,
            c.querySelector('[data-f=value]').value, c.querySelector('[data-f=tpl]').value]; })()", row("time")))
  check("(b) the condition read back into its fields",
        identical(unlist(back), c("estimate", "<", "0.5", "<{mean:.1f}")), paste(unlist(back), collapse = " | "))

  # (c) T-14-1-1: SEX made its own (+ Variable), a condition on it; the
  # decimals of N changed -- the condition kept, its decimals rewritten
  # (they once took a variable's conditions away)
  js("(() => { $('#target')[0].selectize.setValue('T-14-1-1'); return true; })()")
  idle()
  wait_for("$('.rp-cv-addsel').length > 0 || $('#fetch3').length > 0", 60)
  if (isTRUE(js("$('#fetch3').length > 0"))) click("#fetch3")
  wait_for("$('.rp-cv-addsel').length > 0", 240)
  idle()
  js("(() => { const s = document.querySelector('.rp-cv-addsel'); s.value = 'SEX';
    s.dispatchEvent(new Event('change', {bubbles: true})); return true; })()")
  idle()
  check("(c) SEX made its own", wait_for(sprintf("$('%s input.rp-cv-tpl').length > 0", row("SEX")), 30))
  click(paste(row("SEX"), "button[data-act=cond_add]"))
  wait_for(sprintf("$('%s .rp-cv-cond').length > 0", row("SEX")), 20)
  js(sprintf("(() => { const c = document.querySelector('%s .rp-cv-cond');
    const s = c.querySelector('select[data-f=col]');
    if (![...s.options].some(o => o.value == 'n')) s.add(new Option('n', 'n'));
    s.value = 'n'; s.dispatchEvent(new Event('change', {bubbles: true})); return true; })()", row("SEX")))
  idle()
  js(sprintf("(() => { const s = document.querySelector('%s .rp-cv-cond select[data-f=op]');
    s.value = '>='; s.dispatchEvent(new Event('change', {bubbles: true})); return true; })()", row("SEX")))
  idle()
  set_field(paste(row("SEX"), ".rp-cv-cond input[data-f=value]"), "0")
  set_field(paste(row("SEX"), ".rp-cv-cond input[data-f=tpl]"), "{N:.0f}")
  js("(() => { const c = document.querySelector('.rp-card[data-fn=plan_digits]'); if (c && !c.classList.contains('rp-card-open')) c.querySelector('.rp-card-toggle').click(); return true; })()")
  wait_for("$('input[id$=\"_dg_N\"]').length > 0", 30)
  dg <- js("(() => { const e = $('input[id$=\"_dg_N\"]').first(); return e.length ? e.attr('id') : null; })()")
  if (!is.null(dg)) {
    js(sprintf("(() => { const e = $('#%s'); e.val(1).trigger('change'); return true; })()", dg))
    idle()
    Sys.sleep(1)
    save()
    y <- yml("outputs/T-14-1-1.yml")
    check("(c) the decimals changed: the condition kept, its decimals too",
          grepl("!r n >= 0 ~ \"{N:.1f}\"", y, fixed = TRUE), y)
  } else check("(c) the decimals of N", FALSE, js("$('input[id*=\"_dg_\"]').map((i, e) => e.id).get().join(' ')"))

  # (d) the study tab: the company standard's first item copied, it alone
  d0 <- yml("defaults.yml")
  js("(() => { $('#nav a[data-value=\"study\"]').tab('show'); return true; })()")
  wait_for("$('details.rp-sec[data-sec=cells]').length > 0", 30)
  js("(() => { document.querySelector('details.rp-sec[data-sec=cells]').open = true; return true; })()")
  wait_for("$('#std_cells_pick input[type=checkbox]').length > 0", 30)
  idle()
  Sys.sleep(2)
  picked <- js("(() => { const c = $('#std_cells_pick input[type=checkbox]'); c.first()[0].click(); return c.first().val(); })()")
  idle()
  click("#std_cells_copy")
  note <- js("$('.shiny-notification').text() + ' | ' + JSON.stringify(Shiny.shinyapp.$inputValues['std_cells_pick']) + ' | ' + $('[id=std_cells_pick]').length + ' ' + $('#std_cells_pick input:checked').length")
  save()
  d1 <- yml("defaults.yml")
  # (the sample's study standard has no continuous rows: the company's
  # rows are one item; its default stays)
  check("(d) the ticked item copied", identical(picked, "continuous") &&
          grepl("continuous:", d1, fixed = TRUE) && grepl("Mean (SD)", d1, fixed = TRUE), paste(picked, note))
  check("(d) nothing else changed", grepl("default: '{n:.0f} ({p:.1f%})'", d1, fixed = TRUE) &&
          nchar(d1) > nchar(d0), d1)
  expect_true(all(unlist(res)))
})
