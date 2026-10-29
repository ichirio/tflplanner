# A listing template's columns (tflspec's `listing`: `columns`, records of
# vars, label, width, collapse_repeats) as a field of their own: the data's
# columns chosen in order (dragged), each with its heading -- the data's
# label to start with --, its width and whether a repeated value is printed
# once.  The field says its value as the lines the list field writes
# ("AEDECOD | Preferred term | 20"), so the template's readers
# (.tpl_value(), .tpl_list_parse()) take it as they do the list field's.
# The columns are the chosen dataset's: the template's `data` field beside
# it changes them in the page.  (tflplanner#504: the listing's two ways in,
# as the tables'; this replaces "Start from the data".)

# A list parameter whose records start with their variables: a listing's
# columns
.tpl_is_columns <- function(prm) {
  identical(prm$type, "list") && isTRUE(prm$multiple) &&
    identical(as.character(prm$fields %||% character())[1L], "vars")
}

# The datasets' columns with their labels, for the page: list(ADAE =
# list(list("AEDECOD", "Dictionary-Derived Term"), ...))
.tpl_columns_labels <- function(data) {
  lapply(data, function(d) unname(lapply(names(d), function(v) {
    lab <- attr(d[[v]], "label") %||% ""
    list(v, if (length(lab) == 1L && !is.na(lab)) as.character(lab) else "")
  })))
}

# The field's rows as it starts: the records shown (the report's, or none)
.tpl_columns_rows <- function(x) {
  if (is.null(x) || !is.list(x)) return(list())
  unname(lapply(x, function(r) list(
    v = paste(as.character(unlist(r$vars %||% character())), collapse = ", "),
    l = as.character(r$label %||% ""),
    w = if (is.null(r$width)) "" else as.character(r$width),
    c = isTRUE(r$collapse_repeats))))
}

# The field: `input_id` takes the lines, `data_id` is the template's `data`
# field (the dataset the columns come from), `dataset` the one shown first
.tpl_columns_widget <- function(input_id, label, shown, data, data_id, dataset = NULL, t = identity) {
  ds <- toupper(as.character(dataset %||% "")[1L])
  cols <- .tpl_columns_labels(data)
  now <- if (!is.na(ds) && nzchar(ds) && !is.null(cols[[ds]])) cols[[ds]] else
    unique(unlist(cols, recursive = FALSE))
  rows <- .tpl_columns_rows(shown)
  picked <- vapply(rows, function(r) strsplit(r$v, ", ", fixed = TRUE)[[1L]][1L], "")
  choices <- union(picked, vapply(now, function(x) x[[1L]], ""))
  json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  shiny::div(
    class = "rp-lcols mb-3", id = paste0(input_id, "__box"),
    `data-id` = input_id, `data-src` = data_id,
    `data-cols` = json(cols), `data-rows` = json(rows),
    `data-heads` = json(list(t("Column"), t("Heading"), t("Width"), t("Once"))),
    shiny::selectizeInput(paste0(input_id, "__pick"), label, choices, selected = picked,
                          multiple = TRUE, width = "100%",
                          options = list(plugins = list("remove_button", "drag_drop"))),
    shiny::div(class = "small text-muted mt-n2 mb-1",
               t("Drag to order; each column's heading starts as the data's label. Width: a share of the page (blank: the study's)."),
               " ", t("Once: a value repeated in the rows below is printed once.")),
    shiny::div(class = "rp-lcols-rows"),
    shiny::tags$script(shiny::HTML(sprintf("rpLcolsStart('%s');", input_id))))
}

# The page's part of the field (once, in the app's head): its rows drawn
# from the chosen columns, kept as typed, said back as lines
.tpl_columns_js <- "
window.rpLcolsStart = function(id) {
  var tries = 0;
  (function go() {
    var box = document.getElementById(id + '__box');
    var sel = document.getElementById(id + '__pick');
    if (!box || !sel || !sel.selectize) { if (++tries < 50) setTimeout(go, 100); return; }
    if (box.dataset.started) return;
    box.dataset.started = '1';
    var cols = JSON.parse(box.dataset.cols || '{}');
    var heads = JSON.parse(box.dataset.heads || '[]');
    var kept = {};
    JSON.parse(box.dataset.rows || '[]').forEach(function(r) {
      kept[r.v.split(', ')[0]] = r;
    });
    var labelOf = function(v) {
      for (var ds in cols) {
        var c = cols[ds].find(function(x) { return x[0] === v; });
        if (c && c[1]) return c[1];
      }
      return v;
    };
    var rowsBox = box.querySelector('.rp-lcols-rows');
    var clean = function(s) { return String(s || '').split('|').join('/').trim(); };
    var say = function() {
      var lines = sel.selectize.getValue().map(function(v) {
        var r = kept[v] || {};
        var cells = [r.v || v, clean(r.l), clean(r.w), r.c ? 'TRUE' : ''];
        while (cells.length > 1 && cells[cells.length - 1] === '') cells.pop();
        return cells.join(' | ');
      });
      Shiny.setInputValue(id, lines.join(String.fromCharCode(10)));
    };
    var draw = function() {
      var vs = sel.selectize.getValue();
      vs.forEach(function(v) { if (!kept[v]) kept[v] = {v: v, l: labelOf(v), w: '', c: false}; });
      var h = '<table class=\"table table-sm small mb-1\"><thead><tr>' +
        heads.map(function(x) { return '<th>' + x + '</th>'; }).join('') + '</tr></thead><tbody>';
      vs.forEach(function(v) {
        var r = kept[v];
        h += '<tr data-v=\"' + v + '\"><td><code>' + r.v + '</code></td>' +
          '<td><input type=\"text\" class=\"form-control form-control-sm rp-lc-l\"></td>' +
          '<td style=\"width: 6rem\"><input type=\"number\" min=\"1\" class=\"form-control form-control-sm rp-lc-w\"></td>' +
          '<td style=\"width: 4rem\"><input type=\"checkbox\" class=\"form-check-input rp-lc-c\"></td></tr>';
      });
      rowsBox.innerHTML = vs.length ? h + '</tbody></table>' : '';
      rowsBox.querySelectorAll('tr[data-v]').forEach(function(tr) {
        var r = kept[tr.dataset.v];
        var l = tr.querySelector('.rp-lc-l'), w = tr.querySelector('.rp-lc-w'), c = tr.querySelector('.rp-lc-c');
        l.value = r.l; w.value = r.w; c.checked = !!r.c;
        l.addEventListener('change', function() { r.l = l.value; say(); });
        w.addEventListener('change', function() { r.w = w.value; say(); });
        c.addEventListener('change', function() { r.c = c.checked; say(); });
      });
      say();
    };
    sel.selectize.on('change', draw);
    // the dataset changed: its columns offered, those it has kept
    var src = document.getElementById(box.dataset.src);
    if (src) $(src).on('change', function() {
      var cs = cols[String($(src).val() || '').toUpperCase()];
      if (!cs) return;
      var s = sel.selectize, keep = s.getValue().filter(function(v) {
        return cs.some(function(x) { return x[0] === v; });
      });
      s.clearOptions();
      cs.forEach(function(x) { s.addOption({value: x[0], text: x[1] ? x[0] + ' (' + x[1] + ')' : x[0]}); });
      s.setValue(keep, true);
      s.refreshOptions(false);
      draw();
    });
    draw();
  })();
};
"
