# The plan_cells() card's table of variables (#454): a line saying which
# variables have a template of their own, and -- unfolded when one has --
# a row a variable: common or its own, its template, its statistics as
# chips, its conditions.  Every control sends one event (`ev`, an input of
# the session) that the server writes from; nothing is written unless a
# value is changed.

# The table.  `pc`: plan_cells()' arguments (the report's, over its
# layers); `meta`: ard_meta(); `st`: builder_read(); `dg`: each statistic's
# decimals (the report's rule); `kinds`: the table's own choice of a
# variable's kind; `diff`: .cells_company_diff() (a mark when the study
# standard differs).
.cells_vars_ui <- function(pc, meta, st, dg = integer(), kinds = list(), lang = "en",
                           ev = "b_cv_ev", diff = NULL) {
  t <- function(x) tr(x, lang)
  rows <- .cells_vars_rows(pc, meta, kinds)
  any_own <- any(rows$own)
  pct <- if (identical(st$cat_format, "own")) NA else st$pct_decimals %||% 1
  cols <- .cells_cond_columns(meta)
  sel <- function(cls, choices, selected, names = choices, ...) {
    shiny::tags$select(
      class = paste("form-select form-select-sm", cls), ...,
      lapply(seq_along(choices), function(i) shiny::tags$option(
        value = choices[i], selected = if (identical(choices[i], selected)) NA,
        names[i])))
  }
  btn <- function(act, label, title, i = NA, cls = "btn-outline-secondary") {
    shiny::tags$button(type = "button", class = paste("btn btn-sm py-0 px-1 rp-cv-btn", cls),
                       `data-act` = act, `data-i` = if (!is.na(i)) i, title = title, label)
  }
  row_ui <- function(r) {
    v <- rows$variable[r]
    own <- rows$own[r]
    kind <- rows$kind[r]
    e <- if (own) .cells_entry_parse(pc[[v]])
    stats <- .cells_var_stats(meta, v)
    # (the statistics its template reads that the ARD lacks: grey, to step 1)
    used <- if (own) unique(unlist(lapply(.cells_entry_templates(pc[[v]]), .template_stats))) else
      character()
    lack <- if (length(stats)) setdiff(used, stats) else character()
    mode_ui <- shiny::tagList(
      sel("rp-cv-mode", c("common", "own"), if (own) "own" else "common",
          names = t(c("Common", "Own (this variable)")),
          title = t("Own: a copy of the common template, edited here; it does not follow the common one afterwards."),
          disabled = if (own && !rows$in_ard[r]) NA),
      if (!own) sel("rp-cv-kind", c("continuous", "categorical"), kind,
                    names = paste0(t(c("Continuous", "Categorical")),
                                   ifelse(c("continuous", "categorical") %in% rows$kind_auto[r],
                                          paste0(" (", t("from the ARD"), ")"), "")),
                    title = t("Which common template the variable takes: from the ARD's statistics (p: categorical); choose to override.")))
    tpl_ui <- if (!own) {
      shiny::div(class = "rp-cv-ro text-muted small",
                 .cells_entry_text(.cells_common_entry(pc, st, kind)))
    } else if (identical(e$type, "single")) {
      shiny::tags$input(type = "text", class = "form-control form-control-sm rp-cv-tpl",
                        `data-k` = "0", value = if (!is.na(e$fallback)) e$fallback else "",
                        title = if (length(e$conds)) t("When no condition holds"))
    } else if (identical(e$type, "rows")) {
      shiny::tagList(lapply(names(e$rows), function(lb) shiny::div(
        class = "d-flex gap-1 align-items-center mb-1",
        shiny::span(class = "small text-nowrap rp-cv-rowlab", lb),
        shiny::tags$input(type = "text", class = "form-control form-control-sm rp-cv-tpl",
                          `data-k` = paste0("r:", lb), value = e$rows[[lb]]))))
    } else {
      shiny::div(class = "small", shiny::tags$code(.cells_entry_text(e$raw)),
                 shiny::div(class = "text-muted", t("Edit in R (this card's R).")))
    }
    chips <- shiny::div(
      class = "d-flex flex-wrap gap-1",
      lapply(stats, function(k) shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-primary py-0 px-1 rp-cv-chip",
        `data-ins` = .cells_chip_text(k, dg, pct), disabled = if (!own) NA,
        title = .cells_chip_text(k, dg, pct), k)),
      lapply(lack, function(k) shiny::tags$a(
        href = "#", class = "badge text-bg-light text-decoration-none rp-cv-lack",
        title = t("Not in the ARD: add it in step 1"),
        onclick = "Shiny.setInputValue('b_rows_step1', Math.random()); return false;",
        paste(k, "\u2192", t("step 1")))))
    cond_ui <- if (own && identical(e$type, "single")) shiny::tagList(
      lapply(seq_along(e$conds), function(i) {
        c1 <- e$conds[[i]]
        moves <- shiny::span(
          class = "text-nowrap",
          btn("cond_up", "\u2191", t("Up (the first that holds is used)"), i),
          btn("cond_down", "\u2193", t("Down"), i),
          btn("cond_rm", "\u00d7", t("Remove the condition"), i, "btn-outline-danger"))
        if (!isTRUE(c1$ok)) {
          return(shiny::div(
            class = "d-flex gap-1 align-items-center mb-1 rp-cv-cond", `data-i` = i,
            shiny::tags$code(class = "small", as.character(unclass(c1$raw))),
            shiny::span(class = "small text-muted text-nowrap", t("Edit in R")), moves))
        }
        shiny::div(
          class = "d-flex gap-1 align-items-center mb-1 rp-cv-cond", `data-i` = i,
          sel("rp-cv-cf", unique(c(c1$col, cols)), c1$col, `data-f` = "col",
              title = t("Column")),
          sel("rp-cv-cf", .cells_ops, c1$op, `data-f` = "op", title = t("Operator")),
          shiny::tags$input(type = "text", class = "form-control form-control-sm rp-cv-cf",
                            `data-f` = "value", value = c1$value, title = t("Value (a number or a text)"),
                            style = "width:6em"),
          shiny::tags$input(type = "text", class = "form-control form-control-sm rp-cv-cf rp-cv-tpl2",
                            `data-f` = "tpl", value = c1$tpl, title = t("Template when it holds")),
          moves)
      }),
      btn("cond_add", paste0("\uff0b", t("Condition")), t("A condition: the first that holds is used"),
          cls = "btn-outline-primary"))
    shiny::tags$tr(
      class = "rp-cv-row", `data-var` = v,
      shiny::tags$td(shiny::div(
        class = "rp-cv-head", shiny::strong(v),
        if (!rows$in_ard[r]) shiny::span(
          class = "badge text-bg-warning",
          title = t("Not in the ARD: kept, its settings would be lost"),
          t("Not in the ARD")),
        mode_ui)),
      shiny::tags$td(class = "rp-cv-tplcell", `data-label` = t("Template"), tpl_ui),
      shiny::tags$td(`data-label` = t("Statistics"), chips),
      shiny::tags$td(`data-label` = t("Conditions"), cond_ui))
  }
  commons <- rows$variable[!rows$own & rows$in_ard]
  differs <- !is.null(diff) && any(diff$differs)
  shiny::div(
    class = "rp-cv mt-2", `data-ev` = ev,
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      shiny::span(class = "small rp-cv-sum", .cells_vars_summary(pc, lang)),
      if (length(commons)) shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-primary py-0 rp-cv-addvar",
        paste0("\uff0b", t("Variable"))),
      if (length(commons)) shiny::span(
        class = "rp-cv-addbox d-none",
        sel("rp-cv-addsel", c("", commons), "", names = c(t("A variable of its own..."), commons),
            style = "width:auto;display:inline-block")),
      if (differs) shiny::tags$a(
        href = "#", class = "badge text-bg-light text-decoration-none rp-cv-std",
        title = paste(t("The study standard differs from the company standard:"),
                      paste(diff$label[diff$differs], collapse = ", ")),
        onclick = "Shiny.setInputValue('b_cv_std', Math.random()); return false;",
        t("Differs from the company standard"))),
    shiny::tags$details(
      class = "rp-cv-tbl mt-1", open = if (any_own) NA,
      shiny::tags$summary(class = "small text-muted", t("Variables and formats")),
      shiny::div(
        class = "table-responsive",
        shiny::tags$table(
          class = "table table-sm align-top small mb-1",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th(t("Variable")), shiny::tags$th(t("Common / own")),
            shiny::tags$th(t("Template")), shiny::tags$th(t("Statistics")),
            shiny::tags$th(t("Conditions")))),
          shiny::tags$tbody(lapply(seq_len(nrow(rows)), row_ui))))),
    shiny::singleton(shiny::tags$head(shiny::tags$script(shiny::HTML(.cells_vars_js)),
                                      shiny::tags$style(shiny::HTML(.cells_vars_css)))))
}

# the templates an entry holds (its rows', its conditions' after the ~)
.cells_entry_templates <- function(x) {
  e <- .cells_entry_parse(x)
  switch(e$type,
         single = c(vapply(e$conds, function(c) c$tpl %||% NA_character_, ""), e$fallback),
         rows = unlist(e$rows),
         character())
}

# the columns a condition can test: the statistics, then the ARD's keys
.cells_cond_columns <- function(meta) {
  skip <- c("stat", "stat_fmt", "stat_name", "stat_label", "fmt_fun", "fmt_fn",
            "warning", "error", "context", "variable", "variable_level")
  cn <- meta$columns %||% character()
  cn <- cn[!startsWith(cn, ".") & !cn %in% skip & !grepl("^group[0-9]+(_level)?$", cn)]
  unique(c(meta$stats %||% character(), cn))
}

# the study tab's plan_cells() of the study standard: what it says, and the
# company standard's items it differs in, each to copy (ticked)
.cells_std_ui <- function(p, lang = "en", company = .cells_company()) {
  t <- function(x) tr(x, lang)
  sc <- .cells_study(p)
  d <- .cells_company_diff(p, company)
  now <- if (length(sc)) shiny::tags$dl(
    class = "rp-args-view small mb-2",
    lapply(names(sc), function(k) shiny::tagList(shiny::tags$dt(k),
                                                 shiny::tags$dd(shiny::tags$code(.cells_entry_text(sc[[k]]))))))
  else shiny::p(class = "small text-muted", t("The study standard says none: the company standard's apply."))
  shiny::tagList(
    now,
    if (!nrow(d)) shiny::p(class = "small text-muted", t("The same as the company standard.")) else
      shiny::div(
        class = "border rounded p-2",
        shiny::div(class = "small fw-bold mb-1",
                   with_tip(t("Copy from the company standard"),
                            t("The company standard's value of each ticked item is written into the study standard (defaults.yml); the others stay. Every report takes it unless it has its own."))),
        shiny::checkboxGroupInput(
          "std_cells_pick", NULL,
          choiceNames = lapply(seq_len(nrow(d)), function(i) shiny::span(
            shiny::strong(d$label[i]), ": ",
            shiny::tags$code(d$company[i]),
            shiny::span(class = "text-muted ms-1",
                        if (is.na(d$study[i])) t("(the study standard has none)") else
                          paste0("(", t("study standard"), ": ", d$study[i], ")")))),
          choiceValues = d$key, width = "100%"),
        .btn("std_cells_copy", t("Copy the ticked items"), class = "btn-sm btn-outline-primary")))
}

.cells_vars_css <- "
.rp-cv table, .rp-cv tbody, .rp-cv tr, .rp-cv td { display: block; width: 100%; }
.rp-cv thead { display: none; }
.rp-cv tr.rp-cv-row { border: 1px solid var(--bs-border-color, #dee2e6); border-radius: .3rem;
  padding: .3rem .4rem; margin-bottom: .4rem; }
.rp-cv td { border: 0; padding: .1rem 0; }
.rp-cv td[data-label]:not(:empty)::before { content: attr(data-label); display: block;
  font-size: 11px; color: var(--bs-secondary-color, #6c757d); }
.rp-cv .rp-cv-head { display: flex; flex-wrap: wrap; gap: .3rem; align-items: center; }
.rp-cv select.form-select { width: auto; display: inline-block; }
.rp-cv .rp-cv-ro { white-space: pre-wrap; }
.rp-cv .rp-cv-cond { flex-wrap: wrap; }
.rp-cv .rp-cv-cond input[data-f=tpl] { flex: 1 1 10em; }
.rp-cv .rp-cv-rowlab { min-width: 5em; }
"

.cells_vars_js <- "
(function() {
  if (window.rpCv) return;
  window.rpCv = true;
  var last = null;
  function send(el, o) {
    var box = el.closest('.rp-cv');
    var row = el.closest('.rp-cv-row');
    if (row) o.v = row.getAttribute('data-var');
    o.nonce = Math.random();
    Shiny.setInputValue(box.getAttribute('data-ev'), o, {priority: 'event'});
  }
  document.addEventListener('focusin', function(e) {
    var el = e.target;
    if (el.closest && el.closest('.rp-cv') && (el.classList.contains('rp-cv-tpl') ||
        el.classList.contains('rp-cv-tpl2'))) last = el;
  });
  document.addEventListener('change', function(e) {
    var el = e.target;
    if (!el.closest || !el.closest('.rp-cv')) return;
    var cl = el.classList;
    if (cl.contains('rp-cv-mode')) send(el, {act: 'mode', value: el.value});
    else if (cl.contains('rp-cv-kind')) send(el, {act: 'kind', value: el.value});
    else if (cl.contains('rp-cv-addsel')) { if (el.value) send(el, {act: 'mode', v: el.value, value: 'own'}); }
    else if (cl.contains('rp-cv-cf')) {
      var c = el.closest('.rp-cv-cond');
      send(el, {act: 'cond', i: +c.getAttribute('data-i'), field: el.getAttribute('data-f'), value: el.value});
    }
    else if (cl.contains('rp-cv-tpl')) send(el, {act: 'tpl', k: el.getAttribute('data-k'), value: el.value});
  });
  document.addEventListener('click', function(e) {
    var el = e.target.closest ? e.target.closest('.rp-cv button') : null;
    if (!el) return;
    if (el.classList.contains('rp-cv-addvar')) {
      var box = el.closest('.rp-cv').querySelector('.rp-cv-addbox');
      if (box) box.classList.toggle('d-none');
      return;
    }
    if (el.classList.contains('rp-cv-chip')) {
      var row = el.closest('.rp-cv-row');
      var inp = (last && row.contains(last)) ? last : row.querySelector('.rp-cv-tpl');
      if (!inp) return;
      var s = inp.selectionStart == null ? inp.value.length : inp.selectionStart;
      var t = inp.selectionEnd == null ? s : inp.selectionEnd;
      var ins = el.getAttribute('data-ins');
      inp.value = inp.value.slice(0, s) + ins + inp.value.slice(t);
      inp.focus();
      inp.setSelectionRange(s + ins.length, s + ins.length);
      inp.dispatchEvent(new Event('change', {bubbles: true}));
      return;
    }
    var act = el.getAttribute('data-act');
    if (!act) return;
    var o = {act: act};
    if (el.hasAttribute('data-i')) o.i = +el.getAttribute('data-i');
    send(el, o);
  });
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('rp-open-sec', function(name) {
      setTimeout(function() {
        var d = document.querySelector('details.rp-sec[data-sec=\"' + name + '\"]');
        if (!d) return;
        d.open = true;
        d.scrollIntoView({block: 'start'});
      }, 300);
    });
  }
})();
"
