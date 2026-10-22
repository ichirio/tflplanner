# The template report screen in a browser, as a user clicks it (#440):
# headless Chrome (chromote) on the app started here, on a free port, with
# a home of its own.  Not on CRAN, not without Chrome and chromote, and
# only when asked: TFLPLANNER_E2E=true.  (a) a card added: its head in
# view once the page settles; (b) an empty card says "Not set"; (c) an
# argument changed: the page not veiled; (d) the dialog's first OK makes
# the report; (e) a study standard's field edited: the card is "This table".
# The template report shows the template's card and the cards overridden
# (none at first: tfl_tpl() writes no study-standard plan_*() line); the
# test waits for the page's state, not for fixed times.

test_that("a template report in the browser: add, empty card, no veil, OK, this table", {
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
  b <- chromote::ChromoteSession$new(width = 1280, height = 900)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  js <- function(code, timeout = 60) {
    r <- b$Runtime$evaluate(code, awaitPromise = TRUE, returnByValue = TRUE, timeout_ = timeout)
    if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% "error")
    r$result$value
  }
  `%||%` <- function(a, b) if (is.null(a)) b else a
  wait_for <- function(cond, secs = 30) {
    js(sprintf("(async () => { const t0 = Date.now(); while (Date.now() - t0 < %d) { try { if (%s) return true; } catch (e) {} await new Promise(r => setTimeout(r, 200)); } return false; })()",
               secs * 1000, cond), timeout = secs + 10)
  }
  # a real click: the element brought into view, then the mouse pressed and
  # released at its centre
  click <- function(selector) {
    p <- js(sprintf("(() => { const e = document.querySelector(%s); if (!e) return null;
      e.scrollIntoView({block: 'center', behavior: 'instant'}); let r = e.getBoundingClientRect();
      if (r.top < 0 || r.bottom > window.innerHeight) { window.scrollTo({top: window.scrollY + r.top - window.innerHeight / 2, behavior: 'instant'}); r = e.getBoundingClientRect(); }
      return [r.left + r.width / 2, r.top + r.height / 2]; })()", jsonlite::toJSON(selector, auto_unbox = TRUE)))
    if (is.null(p)) stop("no element ", selector)
    Sys.sleep(0.4)
    p <- js(sprintf("(() => { const r = document.querySelector(%s).getBoundingClientRect();
      return [r.left + r.width / 2, r.top + r.height / 2]; })()", jsonlite::toJSON(selector, auto_unbox = TRUE)))
    for (ty in c("mouseMoved", "mousePressed", "mouseReleased")) {
      b$Input$dispatchMouseEvent(type = ty, x = p[[1]], y = p[[2]], button = "left", clickCount = 1)
    }
    invisible(TRUE)
  }
  type_text <- function(text) b$Input$insertText(text = text)
  key <- function(k, code = k, vk = NULL) {
    b$Input$dispatchKeyEvent(type = "keyDown", key = k, code = code, windowsVirtualKeyCode = vk)
    b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = code, windowsVirtualKeyCode = vk)
  }
  res <- list()
  check <- function(name, ok, detail = "") {
    res[[name]] <<- isTRUE(ok)
    testthat::expect_true(isTRUE(ok), info = paste(name, detail))
  }
  # a watcher in the page: every class html has had while it ran
  watch <- function() js("(() => { window._busy = []; if (window._bw) clearInterval(window._bw);
    window._bw = setInterval(() => { if (document.documentElement.classList.contains('rp-user-busy')) window._busy.push(Date.now()); }, 50); return true; })()")

  invisible(b$Page$navigate(url))
  Sys.sleep(3)
  wait_for("window.$ && ($('.modal.show').length || $('#study_detail').text().length > 50)", 60)
  Sys.sleep(2)
  # (a dialog at the start -- a draft from before: thrown away)
  js("(() => { const b = $('.modal.show button').filter((i, e) => ['\u6368\u3066\u308b', '\u9589\u3058\u308b', 'Close'].includes($(e).text().trim())); b.first().attr('data-e2e', 'close'); return b.length; })()")
  if (isTRUE(js("$('[data-e2e=close]').length > 0"))) {
    click("[data-e2e=close]")
    Sys.sleep(3)
    if (isTRUE(js("$('.modal.show').length > 0"))) { js("(() => { $('[data-e2e=close]').click(); return true; })()"); Sys.sleep(3) }
  }
  wait_for("$('.modal.show').length == 0", 10)
  id <- paste0("T-E", format(Sys.time(), "%H%M%S"))

  # (d) the dialog's first OK makes the report
  click('#nav a[data-value="outputs"]')
  Sys.sleep(1)
  wait_for("document.querySelector('#add') && document.querySelector('#add').offsetParent !== null", 20)
  for (k in 1:3) {
    click("#add")
    if (isTRUE(wait_for("document.querySelector('#modal_tpl_table') && document.querySelector('#modal_tpl_table').selectize", 20))) break
  }
  click("#modal_id")
  type_text(id)
  js("(() => { $('#modal_tpl_table')[0].selectize.setValue('demog'); return true; })()")
  Sys.sleep(0.5)
  js("(() => { $('#mtpl_demog__population')[0].selectize.setValue('SAF'); return true; })()")
  wait_for("!$('#add_ok').prop('disabled')", 10)
  inview <- js("(() => { const r = document.querySelector('#add_ok').getBoundingClientRect(); return r.bottom <= window.innerHeight && r.top >= 0; })()")
  dims <- js("(() => { const d = document.querySelector('.modal.show .modal-dialog'), c = document.querySelector('.modal.show .modal-content'), y = document.querySelector('.modal.show .modal-body'); return [d.getBoundingClientRect().height, c.getBoundingClientRect().height, y.scrollHeight, y.clientHeight, getComputedStyle(y).overflowY, window.innerHeight]; })()")
  okr <- js("(() => { const r = document.querySelector('#add_ok').getBoundingClientRect(); return [r.top, r.bottom]; })()")
  check("dialog OK in view (1280x900)", inview, paste(c(unlist(dims), "ok", unlist(okr)), collapse = " "))
  t0 <- Sys.time()
  click("#add_ok")
  made <- wait_for(sprintf("$('#target option').map((i, e) => e.value).get().includes('%s') && $('#tpl_card').text().length > 0 && !document.body.classList.contains('rp-switching')", id), 30)
  check("(d) the first OK makes the report", made,
        sprintf("%.1f s to its card", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  # the page at rest: no Shiny work running, for a while
  idle <- function(secs = 60) {
    Sys.sleep(0.5)
    wait_for("!document.documentElement.classList.contains('shiny-busy') && !document.body.classList.contains('rp-switching')", secs)
    Sys.sleep(0.5)
    wait_for("!document.documentElement.classList.contains('shiny-busy')", secs)
  }
  # a card from "+ Add a card": the menu opened, its entry clicked, the card
  # in the builder
  add_card <- function(fn) {
    idle()
    for (k in 1:3) {
      click(".rp-builder-cards .rp-add-menu .dropdown-toggle")
      if (isTRUE(wait_for("$('.rp-builder-cards .rp-add-menu .dropdown-menu.show').length > 0", 3))) break
    }
    click(sprintf(".rp-add-menu .dropdown-menu.show a[onclick*=\"'%s'\"]", fn))
    ok <- wait_for(sprintf("$('.rp-card[data-fn=%s] > .rp-card-head').length > 0", fn), 30)
    idle()
    ok
  }
  card_head <- function(fn) {
    js(sprintf("(() => { const h = document.querySelector('.rp-card[data-fn=%s] > .rp-card-head'); return h ? h.innerText : ''; })()", fn))
  }

  # the preview (ARD and table) made by itself: the template's card and the
  # builder's "+ Add a card"; no other card till one is overridden (the
  # template writes no study-standard line of its own)
  cards <- wait_for("$('#tpl_card').text().includes('tfl_tpl()') && $('.rp-builder-cards .rp-add-menu').length > 0", 60)
  idle()
  shown <- js("$('.rp-builder-cards .rp-card').map((i, e) => e.dataset.fn).get().join(' ')")
  check("the template card and + Add a card after the preview", cards)
  check("no card but the template's before an override", identical(shown, ""), shown)

  # (a), (b) a card added from "+ Add a card": its head in view once the page
  # settles; an empty card says so
  check("plan_hide() added", add_card("plan_hide"))
  pos <- js("(() => { const h = document.querySelector('.rp-card[data-fn=plan_hide] > .rp-card-head');
    const r = h.getBoundingClientRect(); return [r.top, r.bottom, window.innerHeight, window.scrollY]; })()")
  check("(a) the added card's head in view", pos[[1]] >= 0 && pos[[2]] <= pos[[3]],
        sprintf("top %.0f bottom %.0f of %.0f (scrollY %.0f)", pos[[1]], pos[[2]], pos[[3]], pos[[4]]))
  head <- card_head("plan_hide")
  check("(b) the empty card says Not set", grepl("\u672a\u8a2d\u5b9a", head), gsub("\\s+", " ", head))
  check("(b) the empty card is not This table", !grepl("\u3053\u306e\u8868", head))
  shown <- js("$('.rp-builder-cards .rp-card').map((i, e) => e.dataset.fn).get().join(' ')")
  check("the template card and the added card only", identical(shown, "plan_hide"), shown)

  # (c) an argument changed (x on a chip): the page is not veiled
  if (isTRUE(js("!document.querySelector('#tpl_card details').open"))) click("#tpl_card details > summary")
  wait_for("document.querySelector('#tpl_card details').open && $('#tpl_demog__categorical').parent().find('.selectize-input .item .remove').filter((i, e) => e.offsetParent !== null).length > 0", 20)
  idle()
  watch()
  sel <- js("(() => { const i = $('#tpl_demog__categorical');
    const it = i.parent().find('.selectize-input .item').last(); if (!it.length) return null;
    it.attr('data-e2e', 'x'); return it.attr('data-value'); })()")
  if (!is.null(sel)) {
    click(".selectize-input .item[data-e2e='x'] .remove")
    gone_js <- sprintf("!($('#tpl_demog__categorical').val() || []).includes('%s')", sel)
    wait_for(gone_js, 20)
    idle()
    # (still gone once the server has answered: the change kept)
    gone <- js(gone_js)
    n <- js("window._busy.length")
    check("(c) the chip removed by its x", gone, sel)
    check("(c) no veil (rp-user-busy) on an argument change", n == 0, sprintf("veiled %d samples", n))
  } else check("(c) no veil on an argument change", FALSE, "no chip to remove")

  # (e) a study standard's field edited: the card is this table's.  The
  # template writes no plan_cells(): its card, added, shows the study
  # standard's values till one is changed.
  check("plan_cells() added", add_card("plan_cells"))
  wait_for("$('.rp-card[data-fn=plan_cells] [id$=\"_pct\"]').filter((i, e) => e.offsetParent !== null).length > 0", 30)
  before <- card_head("plan_cells")
  check("(e) the added plan_cells() card shows the study standard", grepl("\u8a66\u9a13\u6a19\u6e96", before),
        gsub("\\s+", " ", before))
  pct <- js("$('.rp-card[data-fn=plan_cells] [id$=\"_pct\"]').filter((i, e) => e.offsetParent !== null).first().attr('id')")
  old <- js(sprintf("document.getElementById('%s').value", pct))
  new <- if (identical(old, "2")) "3" else "2"
  click(paste0("#", pct))
  js(sprintf("(() => { const e = document.getElementById('%s'); e.select(); return true; })()", pct))
  type_text(new)
  key("Tab", "Tab", 9)
  wait_for("document.querySelector('.rp-card[data-fn=plan_cells] > .rp-card-head').innerText.includes('\u3053\u306e\u8868')", 30)
  idle()
  after <- card_head("plan_cells")
  check("(e) the edited study standard's card says This table", grepl("\u3053\u306e\u8868", after),
        sprintf("before: %s | after: %s", gsub("\\s+", " ", before), gsub("\\s+", " ", after)))

  # the study left as it was: the changes thrown away
  click("#discard")
  wait_for("$('#discard_ok').length > 0", 10)
  click("#discard_ok")
  Sys.sleep(3)
  expect_true(all(unlist(res)))
})
