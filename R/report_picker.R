# The reports to choose from, for a study of 100 to 200 of them: a list
# to search (ID, title, analysis set, data -- the function search's
# normalization: case, full / half width, kana), folded by the TOC's
# sections, filtered by state, moved through with the arrow keys; the
# study defaults and every report's rows first.  Folded away, a small
# chooser with a search takes its place.  The rows and the search are
# plain functions, so the report list and the runs table search the same
# way.

# A report's section of the TOC: its heading when the report list has one
# (`section`, from the TOC or written in the app), else the numbers of its
# ID after the letters (T-14-1-1 -> 14.1), else its kind
.report_section <- function(output_id, type, heading = NULL) {
  num <- regmatches(output_id, regexpr("[0-9]+([^0-9]+[0-9]+)?", output_id))
  out <- rep(NA_character_, length(output_id))
  has <- regexpr("[0-9]+([^0-9]+[0-9]+)?", output_id) > 0
  out[has] <- gsub("[^0-9]+", ".", num)
  out[!has] <- type[!has]
  if (!is.null(heading)) {
    h <- !is.na(heading) & nzchar(trimws(heading))
    out[h] <- trimws(heading[h])
  }
  out
}

# A report's short title: its own title line that is not its number nor
# its analysis set, else its description
.report_short_title <- function(x, output_id) {
  lines <- .title_lines(x, output_id)
  lines <- trimws(lines[!grepl("^(Table|Figure|Listing)\\s|^<.*>$", trimws(lines))])
  d <- x$outputs$description[match(output_id, x$outputs$output_id)]
  if (length(lines)) lines[[1L]] else if (!is.na(d)) d else ""
}

# Each report's run, from its files only (the list's mark, for 200 reports
# at a glance): "error" (its last preview's log failed after its RTF), "not
# run" (no RTF), "outdated" (its program, or a file it sources, changed
# after the RTF, or it was made with another study setup), "ok".
# study_status() says more (a program edited by hand, a definition not
# saved yet) by writing every program again, which takes a while for a big
# study.
.report_run_light <- function(study) {
  p <- study$planner
  lay <- study_layout()
  ids <- p$outputs$output_id
  setup <- .report_setup_recorded(study$path, ids)
  now <- .study_setup_hash(study$path)
  # (as study_status(): a figure printing an ARD's numbers turns with it)
  ard_why <- .fig_ard_why(study, ids)
  st <- vapply(ids, function(id) {
    info <- report_info(p, id)
    prog <- file.path(study$path, lay[["programs_tfl"]], info$program)
    t_rtf <- .mtime(file.path(study$path, info$file))
    log <- file.path(study$path, lay[["logs_preview"]],
                     sub("\\.[Rr]$", ".log", info$program))
    t_log <- .mtime(log)
    failed <- !is.na(t_log) && (is.na(t_rtf) || t_log > t_rtf) &&
      any(grepl("^Error|Execution halted", readLines(log, warn = FALSE, encoding = "UTF-8")))
    if (failed) "error" else if (is.na(t_rtf)) "not run" else
      if (isTRUE(.program_time(prog, study$path) > t_rtf) ||
          .setup_changed(setup[match(id, ids)], study$path, now) ||
          nzchar(ard_why[[id]])) "outdated" else "ok"
  }, "")
  data.frame(output_id = ids, status = unname(st), stringsAsFactors = FALSE)
}

# sections in the TOC's order: by their numbers (14.2 before 14.10; a
# heading by the number it starts with, "14.1 Demographics"), those
# without numbers after them
.section_order <- function(secs) {
  lead <- regmatches(secs, regexpr("^[0-9]+(\\.[0-9]+)*", secs))
  num <- grepl("^[0-9]+(\\.[0-9]+)*", secs)
  key <- rep(NA_character_, length(secs))
  key[num] <- vapply(strsplit(lead, ".", fixed = TRUE), function(v)
    paste(sprintf("%06d", suppressWarnings(as.integer(v))), collapse = "."), "")
  key[num] <- paste(key[num], secs[num])
  key[!num] <- secs[!num]
  secs[order(!num, key, method = "radix")]
}

# One state for a report, from its ARD's and its run's: "error",
# "outdated" (to make again), "not built", or "ok"
.report_state <- function(ard, run) {
  ard[is.na(ard)] <- ""
  run[is.na(run)] <- ""
  ifelse(ard == "error" | run == "error", "error",
         ifelse(ard == "outdated" | run %in% c("outdated", "unsaved"), "outdated",
                ifelse(ard == "not built" | run %in% c("not run", "no program", "todo"),
                       "not built", "ok")))
}

# The rows of the list: `output_id`, `title`, `type`, `section`,
# `population`, `datasets`, `state`; `ard` ard_status(), `run`
# study_status() (either may be NULL)
.report_rows <- function(x, ard = NULL, run = NULL) {
  ids <- x$outputs$output_id
  if (!length(ids)) {
    return(data.frame(output_id = character(), title = character(), type = character(),
                      section = character(), population = character(),
                      datasets = character(), state = character(),
                      stringsAsFactors = FALSE))
  }
  type <- vapply(ids, function(id) report_info(x, id)$type, "")
  # the report's analysis set (the report list's, the TOC's); else those
  # its analyses have (their own, or their analysis data's)
  a <- x$ard$analyses
  pop <- vapply(ids, function(id) {
    ad <- .adata_rows(x, id)
    own <- report_population(x, id)
    if (!is.na(own)) return(own)
    k <- !is.na(a$output_id) & a$output_id == id
    v <- a$population_id[k]
    d <- if (!is.null(a$data)) a$data[k] else character()
    v <- c(v, vapply(d[!is.na(d) & d %in% ad$data_id], function(i) .adata_pop(ad, i), ""))
    paste(unique(stats::na.omit(v)), collapse = " | ")
  }, "")
  # the datasets the report reads; until its definition names them, the
  # TOC's
  toc_ds <- x$outputs$datasets %||% rep(NA_character_, length(ids))
  ds <- vapply(seq_along(ids), function(i) {
    v <- tryCatch(.report_datasets(x, ids[i], type[i]), error = function(e) character())
    v <- paste(unique(stats::na.omit(v)), collapse = " | ")
    if (!nzchar(v) && !is.na(toc_ds[i])) toc_ds[i] else v
  }, "")
  st <- .report_state(if (!is.null(ard)) ard$state[match(ids, ard$output_id)] else NA,
                      if (!is.null(run)) run$status[match(ids, run$output_id)] else NA)
  data.frame(output_id = ids,
             title = vapply(ids, function(id) .report_short_title(x, id), ""),
             type = unname(type),
             section = .report_section(ids, unname(type), x$outputs$section),
             population = pop, datasets = ds, state = st,
             stringsAsFactors = FALSE, row.names = NULL)
}

# The words of a search as typed (split at blanks, not at the signs of
# an ID: "T-14-3" is one word), each normalized as the function search
# does (case, full / half width, kana) and with its blanks taken out
.report_query <- function(q) {
  w <- unlist(strsplit(trimws(q %||% ""), "[[:space:]\u3000]+"))
  w <- gsub(" ", "", .fn_norm(w), fixed = TRUE)
  w[nzchar(w)]
}

# Each row's text to search: its ID, title, analysis set, data and
# section, each normalized and without blanks, a blank between them (a
# word does not run from one into the next)
.report_hay <- function(rows) {
  cols <- intersect(c("output_id", "title", "population", "datasets", "section"), names(rows))
  if (!nrow(rows)) return(character())
  parts <- lapply(cols, function(k) gsub(" ", "", .fn_norm(rows[[k]]), fixed = TRUE))
  do.call(paste, parts)
}

# The rows a search finds (every word somewhere in the ID, title,
# analysis set or data), of a state ("all", "outdated", "not built",
# "error")
.report_filter <- function(rows, q = "", state = "all") {
  if (!identical(state, "all") && !is.null(state)) rows <- rows[rows$state == state, , drop = FALSE]
  words <- .report_query(q)
  if (!length(words) || !nrow(rows)) return(rows)
  hay <- .report_hay(rows)
  hit <- Reduce(`&`, lapply(words, function(w) grepl(w, hay, fixed = TRUE)), TRUE)
  rows[hit, , drop = FALSE]
}

# the text a table's hidden column holds for its search (DT's own search,
# whose words are ANDed: the rows keep their numbers), and the search it
# is given (.report_query()'s words)
.report_search_keys <- function(rows, ids) {
  key <- .report_hay(rows)
  out <- key[match(ids, rows$output_id)]
  miss <- is.na(out)
  out[miss] <- gsub(" ", "", .fn_norm(ids[miss]), fixed = TRUE)
  out
}
.report_dt_search <- function(q) paste(.report_query(q), collapse = " ")

# a table's search set (DT's own updateSearch() needs its search box)
.report_dt_set_search <- function(session, id, q) {
  session$sendCustomMessage("rp-dt-search", list(id = id, q = .report_dt_search(q)))
}

.report_state_marks <- c(ok = "\u25cf", outdated = "\u27f3", `not built` = "\u25cb",
                         error = "!")
.report_state_class <- c(ok = "text-success", outdated = "text-warning",
                         `not built` = "text-muted", error = "text-danger")
.report_state_words <- c(ok = "Made", outdated = "To make again",
                         `not built` = "Not made yet", error = "Error")

# a search box for a table of reports (the report list, the runs)
report_search_ui <- function(id, lang = "en") {
  shiny::div(
    class = "rp-search mb-1", style = "max-width: 22rem",
    shiny::textInput(id, NULL, "", width = "100%",
                     placeholder = tr("Search: ID, title, population, data", lang)))
}

.report_picker_css <- "
.rp-picker .rp-list { max-height: calc(100vh - 16rem); overflow-y: auto; }
.rp-picker .rp-item { display: flex; gap: .35rem; align-items: baseline; width: 100%;
  border: 0; background: none; text-align: left; padding: .1rem .3rem;
  font-size: .85rem; border-radius: .25rem; white-space: nowrap; }
.rp-picker .rp-item:hover, .rp-picker .rp-item.rp-focus { background: var(--bs-tertiary-bg, #eef1f5); }
.rp-picker .rp-item.rp-now { font-weight: 600; background: var(--bs-primary-bg-subtle, #dbe7ff); }
.rp-picker .rp-id { flex: none; }
.rp-picker .rp-title { flex: 1 1 auto; overflow: hidden; text-overflow: ellipsis; color: var(--bs-secondary-color, #6b7280); }
.rp-picker .rp-review-n { flex: 0 0 auto; margin-left: .3rem; font-variant-numeric: tabular-nums; white-space: nowrap; }
.rp-picker .rp-mark { flex: none; }
.rp-picker details > summary { font-size: .8rem; color: var(--bs-secondary-color, #6b7280); cursor: pointer; }
.rp-picker .rp-fixed { border-bottom: 1px solid var(--bs-border-color, #dee2e6); margin-bottom: .25rem; padding-bottom: .25rem; }
.rp-compact { display: none; }
.bslib-sidebar-layout.sidebar-collapsed .rp-compact { display: block; }
"

# the arrow keys move through the rows shown, Enter chooses; a click
# chooses; the folded sections and the search stay as they were when the
# list is drawn again
.report_picker_js <- "
$(document).on('click', '.rp-picker .rp-item', function() {
  var p = $(this).closest('.rp-picker');
  Shiny.setInputValue(p.data('pick'), $(this).data('id'), {priority: 'event'});
});
// the list folded or not, remembered by the browser (if it lets us: a
// browser that keeps nothing shows the list open, as before)
(function() {
  var KEY = 'tflplanner.reportList';
  function get() { try { return window.localStorage.getItem(KEY); } catch (e) { return null; } }
  function put(v) { try { window.localStorage.setItem(KEY, v); } catch (e) {} }
  $(document).on('click', '.bslib-sidebar-layout:has(.rp-picker) > .collapse-toggle', function() {
    var lay = $(this).closest('.bslib-sidebar-layout');
    setTimeout(function() { put(lay.hasClass('sidebar-collapsed') ? 'folded' : 'open'); }, 50);
  });
  $(document).on('shiny:connected', function() {
    if (get() !== 'folded') return;
    var lay = $('.rp-picker').closest('.bslib-sidebar-layout');
    if (lay.length && !lay.hasClass('sidebar-collapsed')) lay.children('.collapse-toggle').click();
  });
})();
// a table's search (the report list, the runs), set without a search box
Shiny.addCustomMessageHandler('rp-dt-search', function(m) {
  var el = $('#' + m.id + ' table.dataTable');
  if (el.length) el.DataTable().search(m.q).draw();
});
// drawn again: the report chosen in sight
$(document).on('shiny:value', function(e) {
  var el = $(e.target);
  if (!el.parent().hasClass('rp-picker')) return;
  setTimeout(function() {
    var n = el.find('.rp-now')[0];
    if (n) n.scrollIntoView({block: 'nearest'});
  }, 0);
});
$(document).on('keydown', '.rp-picker', function(e) {
  if (['ArrowDown', 'ArrowUp', 'Enter'].indexOf(e.key) < 0) return;
  var items = $(this).find('.rp-item:visible');
  if (!items.length) return;
  var i = items.index(items.filter('.rp-focus'));
  if (i < 0) i = items.index(items.filter('.rp-now'));
  // nothing marked: the first report found (the two fixed rows above it)
  var first = items.index($(this).find('.rp-list .rp-item:visible').first());
  if (e.key === 'Enter') {
    var k = i >= 0 ? i : first;
    if (k >= 0) items.eq(k).click();
    e.preventDefault(); return;
  }
  if (i < 0) i = e.key === 'ArrowDown' ? first : Math.max(first, 0);
  else i = e.key === 'ArrowDown' ? Math.min(i + 1, items.length - 1) : Math.max(i - 1, 0);
  items.removeClass('rp-focus'); items.eq(i).addClass('rp-focus')[0].scrollIntoView({block: 'nearest'});
  e.preventDefault();
});
"

#' @noRd
report_picker_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  shiny::div(
    class = "rp-picker", `data-pick` = ns("pick"), tabindex = "0",
    shiny::tags$style(shiny::HTML(.report_picker_css)),
    shiny::tags$script(shiny::HTML(.report_picker_js)),
    shiny::textInput(ns("q"), NULL, "", width = "100%",
                     placeholder = tr("Search: ID, title, population, data", lang)),
    shiny::selectInput(ns("state"), NULL, width = "100%", selectize = FALSE,
                       stats::setNames(c("all", "outdated", "not built", "error"),
                                       tr(c("All", "To make again", "Not made yet", "Error"), lang))),
    shiny::uiOutput(ns("list")))
}

# when the sidebar is folded: a chooser with a search, at the top of the page
report_picker_compact_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  shiny::div(class = "rp-compact mb-2", style = "max-width: 32rem",
             shiny::selectizeInput(ns("compact"), NULL, choices = NULL, width = "100%",
                                   options = list(placeholder = tr("Search: ID, title, population, data", lang))))
}

# `rows` a reactive of .report_rows(), `now` the value chosen (an output id,
# or the study defaults / every report's rows), `pick` called with the
# value the user chose; `fixed` the two rows always first (value = label);
# `folded` whether the list is folded away (its small chooser shown)
report_picker_server <- function(id, rows, now, pick, fixed, lang = "en",
                                 folded = shiny::reactive(FALSE),
                                 counts = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(x) tr(x, lang)
    # a report's review: its errors, checks and what to set by hand (a
    # dash for none), apart from the run's mark
    review_n <- function(value, cn) {
      if (is.null(cn) || !nrow(cn)) return(NULL)
      k <- match(value, cn$output_id)
      if (is.na(k)) return(NULL)
      n <- c(cn$error[k], cn$check[k], cn$hand[k])
      if (!any(n > 0L)) return(NULL)
      txt <- ifelse(n > 0L, as.character(n), "-")
      shiny::span(
        class = "rp-review-n small",
        title = sprintf(t("%d errors, %d to check, %d to set by hand"), n[1L], n[2L], n[3L]),
        shiny::span(class = if (n[1L]) "text-danger fw-bold" else "text-muted", txt[1L]), "/",
        shiny::span(class = if (n[2L]) "text-warning" else "text-muted", txt[2L]), "/",
        shiny::span(class = "text-muted", txt[3L]))
    }
    item <- function(value, label, title = "", state = NA, now_v = "", cn = NULL) {
      mark <- if (!is.na(state)) shiny::span(
        class = paste("rp-mark", .report_state_class[[state]]),
        title = t(.report_state_words[[state]]), .report_state_marks[[state]])
      shiny::tags$button(
        type = "button", class = paste("rp-item", if (identical(value, now_v)) "rp-now"),
        `data-id` = value, title = if (nzchar(title)) paste(label, title) else label,
        shiny::span(class = "rp-id", label),
        shiny::span(class = "rp-title", title), review_n(value, cn), mark)
    }
    output$list <- shiny::renderUI({
      d <- rows()
      cn <- counts()
      now_v <- now() %||% ""
      f <- .report_filter(d, input$q, input$state %||% "all")
      fixed_rows <- shiny::div(
        class = "rp-fixed",
        lapply(names(fixed), function(v) item(v, fixed[[v]], now_v = now_v, cn = cn)))
      if (!nrow(f)) {
        return(shiny::tagList(fixed_rows, shiny::p(class = "small text-muted",
                                                   t("No report matches."))))
      }
      secs <- .section_order(unique(f$section))
      searching <- nzchar(trimws(input$q %||% "")) || !identical(input$state %||% "all", "all")
      shiny::tagList(
        fixed_rows,
        shiny::div(
          class = "rp-list",
          lapply(secs, function(s) {
            g <- f[f$section == s, , drop = FALSE]
            shiny::tags$details(
              open = NA,
              shiny::tags$summary(sprintf("%s (%d)", t(s), nrow(g))),
              lapply(seq_len(nrow(g)), function(i)
                item(g$output_id[i], g$output_id[i], g$title[i], g$state[i], now_v, cn)))
          })),
        if (searching) shiny::p(class = "small text-muted mt-1",
                                sprintf(t("%d of %d reports"), nrow(f), nrow(d))))
    })
    shiny::observeEvent(input$pick, pick(input$pick))
    # the folded chooser: the same values, its search selectize's own.  A
    # value the server set comes back as input$compact too, maybe after the
    # report changed: only a value the user chose is passed on
    set_by_us <- new.env()
    set_compact <- function(...) {
      args <- list(...)
      set_by_us$v <- args$selected
      shiny::updateSelectizeInput(session, "compact", ...)
    }
    shiny::observe({
      d <- rows()
      lab <- ifelse(nzchar(d$title), paste(d$output_id, d$title), d$output_id)
      ch <- c(stats::setNames(names(fixed), unname(unlist(fixed))),
              stats::setNames(d$output_id, lab))
      set_compact(choices = ch, selected = shiny::isolate(now()), server = FALSE)
    })
    shiny::observeEvent(now(), {
      if (!identical(input$compact, now())) set_compact(selected = now())
    }, ignoreNULL = TRUE)
    shiny::observeEvent(input$compact, {
      v <- input$compact
      # chosen there only while it is shown
      if (!isTRUE(shiny::isolate(folded()))) return()
      if (identical(v, set_by_us$v)) {
        set_by_us$v <- NULL
        return()
      }
      if (nzchar(v %||% "") && !identical(v, now())) pick(v)
    }, ignoreInit = TRUE)
  })
}

# What a run leaves in a study folder, as one stamp: the study ARD's status
# (each ARD program writes it), the batches' run.csv and the previews'
# logs, by modification time -- a change of it is a run that ended
.run_files_stamp <- function(path) {
  lay <- study_layout()
  f <- c(file.path(path, lay[["ard"]], "ard_status.csv"),
         Sys.glob(file.path(path, "runs", "*", "run.csv")),
         Sys.glob(file.path(path, lay[["logs_preview"]], "*.log")))
  f <- f[file.exists(f)]
  if (!length(f)) return("")
  paste(length(f), format(max(file.mtime(f)), "%Y%m%d%H%M%OS3"))
}
