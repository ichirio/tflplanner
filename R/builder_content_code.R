# The table builder's content, part 4: normalize_ard(), the rows shown, code
# line -> card, writing an item back, and style.
# Split from R/builder_content.R (no code change).

# ---- normalize_ard() ------------------------------------------------------------

# (normalize_ard()'s card is a form card like the others: .plan_form_body(),
# its hierarchy a field marked the ARD's (.ard_checked()) -- and its arguments written
# into the report's YAML)

# ---- the rows: the variables shown --------------------------------------------------

# A summary table's variables not shown: the row filter the builder writes
# for them (tables$keep_where `!variable %in% c("AGE", ...)`) read back.
# NULL when the row filter says something else (it is the user's, kept).
.hidden_vars_read <- function(where) {
  if (length(where) != 1L || is.na(where) || !nzchar(trimws(where))) return(character())
  e <- tryCatch(str2lang(where), error = function(e) NULL)
  ok <- is.call(e) && identical(e[[1L]], as.name("!")) && is.call(e[[2L]]) &&
    identical(e[[2L]][[1L]], as.name("%in%")) && identical(e[[2L]][[2L]], as.name("variable"))
  if (!ok) return(NULL)
  v <- tryCatch(eval(e[[2L]][[3L]], envir = baseenv()), error = function(e) NULL)
  if (!is.character(v)) NULL else v
}

# ... and written: NA for none
.hidden_vars_write <- function(hidden) {
  if (!length(hidden)) return(NA_character_)
  sprintf("!variable %%in%% %s", paste(deparse(as.character(hidden), width.cutoff = 500L),
                                       collapse = " "))
}

# ---- code line -> card -------------------------------------------------------------

# The lines of a report's program that each item wrote: the plan's pipe
# (`plan <- table_plan(...) |> plan_*() ...`), an item's lines from the one
# its verb starts to the next one's, and the code lists at its head
# (`cl_<variable> <- c(...)`, plan_levels()'s).  The k-th call of a verb is
# its k-th item.  A data frame: `line`, `path`.
builder_code_map <- function(lines, items) {
  lines <- unlist(strsplit(paste(lines, collapse = "\n"), "\n", fixed = TRUE))
  empty <- data.frame(line = integer(), path = character(), stringsAsFactors = FALSE)
  start <- grep("^plan <- ", lines)
  if (!length(start)) return(empty)
  start <- start[1L]
  paths <- vapply(items, function(it) it$path, "")
  fns <- vapply(items, function(it) it$fn, "")
  out_l <- integer()
  out_p <- character()
  seen <- character()
  cur <- NULL
  pending <- integer()
  for (i in start:length(lines)) {
    s <- lines[[i]]
    if (!nzchar(trimws(s))) break
    m <- regmatches(s, regexec("^(plan <- [A-Za-z_.]+ \\|> |plan <- |\\s+)([A-Za-z_.]+)\\(", s))[[1L]]
    fn <- if (length(m)) m[3L] else NA_character_
    if (!is.na(fn) && fn %in% fns) {
      seen <- c(seen, fn)
      k <- sum(seen == fn)
      at <- which(fns == fn)
      cur <- if (k <= length(at)) paths[at[k]] else NULL
      if (!is.null(cur) && length(pending)) {
        out_l <- c(out_l, pending)
        out_p <- c(out_p, rep(cur, length(pending)))
      }
      pending <- integer()
    } else if (grepl("^\\s*#", s)) {
      # a note on the item below it ("# (study default)")
      pending <- c(pending, i)
      next
    }
    if (!is.null(cur)) {
      out_l <- c(out_l, i)
      out_p <- c(out_p, cur)
    }
  }
  # the code lists above the plan: plan_levels()'s
  lev <- paths[fns == "plan_levels"]
  if (length(lev)) {
    cl <- grep("^# the code lists", lines)
    cl <- cl[cl < start]
    if (length(cl)) {
      at <- seq.int(cl[length(cl)], start - 1L)
      at <- at[nzchar(trimws(lines[at]))]
      out_l <- c(out_l, at)
      out_p <- c(out_p, rep(lev[[1L]], length(at)))
    }
  }
  # the ARD's normalization above the plan: normalize_ard()'s card
  if ("normalize_ard" %in% paths) {
    na <- grep("normalize_ard\\(", lines)
    na <- na[na < start]
    if (length(na)) {
      i <- na[length(na)]
      at <- i
      # (its call over several lines: to the line that closes it)
      while (i < start - 1L && !grepl("\\)\\s*$", lines[[i]])) {
        i <- i + 1L
        at <- c(at, i)
      }
      out_l <- c(out_l, at)
      out_p <- c(out_p, rep("normalize_ard", length(at)))
    }
  }
  o <- order(out_l)
  data.frame(line = out_l[o], path = out_p[o], stringsAsFactors = FALSE)
}

# plan_cells()' entries of a variable of its own (not the default, the
# continuous or categorical rows, the value printed, a variable's own
# decimals `skip`), as the card says them: "prob (3 rows), time"; NULL none
.cells_own_vars <- function(pc, skip = character()) {
  k <- setdiff(names(pc %||% list()),
               c("default", "continuous", "categorical", "value", "stats", "na", "notes", skip))
  if (!length(k)) return(NULL)
  vapply(k, function(v) {
    x <- pc[[v]]
    if (inherits(x, "tfl_call")) x <- x$args
    n <- if (is.list(x) || length(x) > 1L) length(x) else 1L
    if (n > 1L) sprintf("%s (%d)", v, n) else v
  }, "", USE.NAMES = FALSE)
}

# ---- writing an item back ------------------------------------------------------------

# One item of a table written into the report's YAML: the `k`-th item of
# `fn` given these arguments -- its whole arguments, as the card says them
# (.spec_set_item(): the report's own layer holds what differs from the
# layers below).  plan_digits()' rounding is the study's, one for every
# report: it stays the layers', and a new one is the study's rounding.
.builder_item_write <- function(x, output_id, fn, args, k = 1L) {
  if (identical(fn, "plan_nest")) {
    its <- builder_items(x, output_id)
    x <- .spec_set_item(x, output_id, fn, args, k)
    return(.nest_order_kept(x, output_id, its))
  }
  rd <- if (identical(fn, "plan_digits")) args$rounding
  x <- .spec_set_item(x, output_id, fn, args, k,
                      below_args = if (identical(fn, "plan_digits")) "rounding" else character())
  if (!is.null(rd) && !inherits(rd, "tfl_r") &&
      !identical(as.character(rd), unname(x$study[["rounding"]] %||% NA_character_))) {
    x <- .spec_set_rounding(x, as.character(rd))
  }
  x
}

# ... and the report's own `k`-th item of `fn` taken away: its layers' apply
# again (none: the verb is gone from the plan)
.builder_item_revert <- function(x, output_id, fn, k = 1L) {
  if (!identical(fn, "plan_nest")) return(.spec_revert_item(x, output_id, fn, k))
  its <- builder_items(x, output_id)
  .nest_order_kept(.spec_revert_item(x, output_id, fn, k), output_id, its)
}

# A variable no longer nested (its plan_nest() entry gone: changed, the
# card off or removed, #428) keeps its place among the variables: it was
# shown after the level's variable (and that variable's other nested ones),
# and plan_levels()' order of the variables, which leaves a nested one out,
# takes it there -- not after the others, where a variable the order does
# not name goes.  `its`: the items before the change.
.nest_order_kept <- function(x, output_id, its) {
  now <- builder_items(x, output_id)
  lv <- .builder_args(now, "plan_levels")
  ord <- as.character(unlist(lv$variable))
  if (!length(ord)) return(x)
  was <- names(.builder_args(its, "plan_nest") %||% list())
  freed <- setdiff(setdiff(was, names(.builder_args(now, "plan_nest") %||% list())), ord)
  if (!length(freed)) return(x)
  vr <- .builder_vars_rows(its)
  o <- suppressWarnings(as.numeric(vr$order))
  seq_was <- vr$variable[order(is.na(o), o)]
  new <- seq_was[seq_was %in% c(ord, freed)]
  lv$variable <- c(new, setdiff(ord, new))
  .spec_set_item(x, output_id, "plan_levels", lv, 1L)
}

# ---- style -----------------------------------------------------------------------------

.builder_content_css <- "
.rp-args-view { display: grid; grid-template-columns: max-content minmax(0, 1fr);
  gap: .1rem .6rem; margin: 0; font-size: 12.5px; }
.rp-args-view dt { font-weight: normal; }
.rp-args-view dd { margin: 0; overflow-wrap: anywhere; }
.rp-args-view code { color: #374151; background: none; padding: 0; font-size: 12px; }
.rp-card-switch .form-check-input:disabled { cursor: default; }
@media (min-width: 992px) {
  /* SPEC | Code | Result beside the cards stays in view as they scroll: the
     column is the sticky box (the tabs inside it are not), its own scroll */
  .rp-b-right { position: sticky; top: calc(var(--rp-top, 0px) + .5rem);
    max-height: calc(100vh - var(--rp-top, 0px) - 1rem); overflow-y: auto; }
  .rp-b-right .rp-tabs { position: static; max-height: none; overflow-y: visible; }
}
.rp-back-code { padding: 0 .3rem; font-size: 12px; text-decoration: none; }
/* a card's rows of names the user gives (comparison columns, groups of
   statistics, nests): the name narrow, its value the rest of the row */
.rp-dots-table { width: 100%; font-size: 12.5px; margin-top: .2rem; }
.rp-dots-table th { font-weight: normal; color: #6b7280; font-size: 12px; padding: 0 .4rem .1rem 0; }
.rp-dots-table td { padding: .1rem .4rem .1rem 0; vertical-align: middle; }
.rp-dots-table td:first-child { width: 1%; white-space: nowrap; }
.rp-dots-table td .form-group, .rp-dots-table td .shiny-input-container { margin-bottom: 0; width: 100% !important; }
.rp-b-sec { margin-top: .5rem; }
.rp-b-levels { display: flex; flex-wrap: wrap; gap: .3rem; align-items: center; }
.rp-b-level { font-family: Consolas, 'Courier New', monospace; font-size: 12px; color: #374151;
  background: #f3f4f6; border: 1px dashed #cbd5e1; border-radius: .3rem; padding: 0 .4rem; }
.rp-b-fixed-note { margin-top: .15rem; }
.rp-b-sec > .rp-b-sec-label { font-size: 12.5px; margin-bottom: .15rem; }
/* the chips of an order (the columns' levels ...): each its whole text,
   onto the next line when narrow -- never cut */
.rp-builder-cards .default-sortable.horizontal .rank-list { flex-wrap: wrap; gap: .25rem; }
.rp-builder-cards .default-sortable.horizontal .rank-list-item { flex: 0 0 auto;
  overflow: visible; white-space: nowrap; max-width: 100%; }
/* plan_levels()' chips: small, a line or two (#417) */
.rp-builder-cards .rp-levels-var .rank-list-item { font-size: 12px; padding: .05rem .4rem;
  line-height: 1.4; min-height: 0; }
.rp-builder-cards .rp-levels-var .rank-list { gap: .2rem; padding: .2rem; }
/* plan_levels()' variables (#428): a line each until opened; the chips
   cut to no height, not hidden (the list reads their text) */
.rp-levels-toggle { color: inherit; text-decoration: none; font-size: 12.5px; display: inline; }
.rp-levels-toggle code { color: #111827; background: none; padding: 0; font-size: 12.5px;
  font-weight: 600; }
.rp-levels-go { color: var(--bs-link-color, #0d6efd); white-space: nowrap; }
.rp-levels-n { color: #6b7280; }
.rp-levels-body { max-height: 0; overflow: hidden; }
.rp-levels-var.rp-open > .rp-levels-body { max-height: none; overflow: visible; }
.rp-levels-var:not(.rp-open) .rp-open-only, .rp-levels-var.rp-open .rp-closed-only { display: none; }
.rp-levels-state { font-size: 12px; margin-left: .6rem; }
.rp-levels-reset { white-space: nowrap; }
"

# the cards' buttons of the table builder, one input each: Revert, Edit as
# R, Remove (the card's path)
.builder_content_js <- "
$(document).on('click', '.rp-builder-cards .rp-revert', function() {
  var p = $(this).closest('.rp-card').data('path');
  Shiny.setInputValue('b_card_revert', {path: String(p), t: Date.now()}, {priority: 'event'});
});
$(document).on('click', '.rp-builder-cards .rp-tool-r', function() {
  var p = $(this).closest('.rp-card').data('path');
  Shiny.setInputValue('b_card_r', {path: String(p), t: Date.now()}, {priority: 'event'});
});
$(document).on('click', '.rp-builder-cards .rp-tool-remove', function() {
  var p = $(this).closest('.rp-card').data('path');
  Shiny.setInputValue('b_card_remove', {path: String(p), t: Date.now()}, {priority: 'event'});
});
// plan_levels()' variables: opened and closed (kept as the card is drawn
// again), their line written again as the values are dragged, and the way
// back to the ARD's order
(function() {
  var open = {};
  function summary(lv) {
    var out = [], n = 0;
    for (var i = 0; i < lv.length; i++) {
      n += String(lv[i]).length + 2;
      if (i > 0 && n > 40) { out.push('\\u2026'); break; }
      out.push(lv[i]);
    }
    return out.join(', ');
  }
  $(document).on('click', '.rp-levels-toggle', function(e) {
    e.preventDefault();
    var v = $(this).closest('.rp-levels-var');
    v.toggleClass('rp-open');
    open[v.data('input')] = v.hasClass('rp-open');
  });
  $(document).on('shiny:value', function() {
    setTimeout(function() {
      $('.rp-levels-var[data-input]').each(function() {
        if (open[$(this).data('input')]) $(this).addClass('rp-open');
      });
    }, 0);
  });
  $(document).on('shiny:inputchanged', function(e) {
    if (!e.name || !Array.isArray(e.value)) return;
    var v = $('.rp-levels-var[data-input=\"' + e.name + '\"]');
    if (!v.length) return;
    v.find('.rp-levels-sum').first().text(summary(e.value));
    v.find('.rp-levels-n').first().text('(' + e.value.length + ')');
    var st = v.find('.rp-levels-states').first();
    if (st.length) {
      var ard = JSON.parse(st.attr('data-ard') || '[]');
      var same = ard.length === e.value.length &&
        ard.every(function(x, i) { return x === String(e.value[i]); });
      st.find('.rp-st-same').toggle(same);
      st.find('.rp-st-own').toggle(!same);
    }
  });
  $(document).on('click', '.rp-levels-reset', function(e) {
    e.preventDefault();
    Shiny.setInputValue('b_lv_reset', {variable: String($(this).data('variable')), t: Date.now()},
                        {priority: 'event'});
  });
})();
"

# The fixed items of a selectize of layers (a hierarchy's levels, the block
# of variables): neither its x nor Backspace removes them -- selectize's
# onDelete refuses -- and their x is not drawn (.fixed_items_css())
.fixed_items_js <- function(fixed) {
  I(sprintf(
    "function(values) { var fixed = %s; return !values.some(function(v) { return fixed.indexOf(v) >= 0; }); }",
    jsonlite::toJSON(as.character(fixed))))
}

# ... their look: no x, a fixed chip (by the input's id and their values)
.fixed_items_css <- function(id, fixed) {
  if (!length(fixed)) return(NULL)
  sel <- function(v, tail = "") sprintf(
    "#%s ~ .selectize-control .item[data-value=\"%s\"]%s", id, v, tail)
  shiny::tags$style(shiny::HTML(paste0(
    paste(vapply(fixed, sel, "", tail = " .remove"), collapse = ", "),
    " { display: none !important; } ",
    paste(vapply(fixed, sel, ""), collapse = ", "),
    " { background: #f3f4f6 !important; color: #374151 !important; border-style: dashed !important; padding-right: 6px !important; }")))
}

# The row layers' chips, named "name (meaning)": the block of the analysis
# variables in words, a hierarchy's last level marked the rows' text
# (label), its other levels, a stack's keys (`stack`), the other keys; a
# constant heading as it is
.layer_names <- function(layers, hierarchy = NULL, lang = "en", stack = character()) {
  t <- function(x) tr(x, lang)
  vapply(layers, function(k) {
    if (identical(k, ".variable")) return(paste0(t("Analysis variables"), " (variable)"))
    if (length(hierarchy) && identical(k, utils::tail(hierarchy, 1L))) return(paste0(k, " (label)"))
    if (.is_heading(k)) return(k)
    if (k %in% hierarchy) return(sprintf(t("%s (hierarchy)"), k))
    if (k %in% stack) return(sprintf(t("%s (stack)"), k))
    sprintf(t("%s (key)"), k)
  }, "", USE.NAMES = FALSE)
}

# the columns' chips: "TRT01A (column key)"
.col_key_names <- function(keys, lang = "en") {
  stats::setNames(keys, sprintf(tr("%s (column key)", lang), keys))
}

# A constant heading typed into the row layers ("Overall, typed with a
# quote first): its text in double quotes, as tables$rows writes it
# (group1 = "Overall")
.heading_create_js <- paste0(
  "function(input) { var v = String(input).replace(/^[\\s\"']+|[\\s\"']+$/g, '').replace(/\"/g, '');",
  " if (!v.length) return false; v = '\"' + v + '\"'; return {value: v, text: v}; }")

# the k-th call of its verb a card's path names ("plan_cell_style#2": 2)
.path_k <- function(path) {
  if (!is.null(path) && grepl("#", path, fixed = TRUE)) as.integer(sub("^.*#", "", path)) else 1L
}

# the item a card's path names (NULL for none)
.builder_item_at <- function(items, path) {
  if (is.null(path)) return(NULL)
  for (it in items) if (identical(it$path, as.character(path))) return(it)
  NULL
}
