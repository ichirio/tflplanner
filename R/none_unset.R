# "(none)": the screen's word for `!unset` (tflspec::tfl_unset()) -- "do not
# pass this argument at all".  The Page tab (R/page_form.R, R/page_spec.R) has
# it first; the template cards and the code SPEC cards say it the same way:
# the field shows "(none)", a small "(none)" button (or an item of a select)
# sets it, and what is written is `!unset`.  The helpers shared by the three.

.none_word <- "(none)"

# what "(none)" is said as in any of the app's languages ("(none)",
# "（なし）"): the screen shows t(.none_word), and a field gives either back
.none_texts <- function() {
  s <- .strings()
  w <- unlist(s[s$en == .none_word, setdiff(names(s), "en")], use.names = FALSE)
  unique(c(.none_word, w[!is.na(w) & nzchar(w)]))
}

.is_unset <- function(x) inherits(x, "tfl_unset")

# text an input gives that says "(none)", in any language (a lone item, trimmed)
.is_none_text <- function(v) {
  is.character(v) && length(v) == 1L && !is.na(v) && trimws(v) %in% .none_texts()
}

# Two values as the same setting: an unset only equals an unset
.same_unset <- function(a, b) {
  if (.is_unset(a) || .is_unset(b)) .is_unset(a) && .is_unset(b) else NA
}

# The small "(none)" button, as the Page tab's .pg_btn("none", "(none)",
# t("None here (!unset)")) looks; `...` its attributes (an onclick).  Not
# .pg_btn() itself: that one belongs to the Page form's own click handler.
.none_btn <- function(t = identity, ...) {
  shiny::tags$button(type = "button", class = "btn btn-link none-btn",
                     title = t("None here (!unset)"), ..., t(.none_word))
}

.none_css <- "
.none-btn.btn-link { font-size: .7rem; padding: 0 .2rem; }
.none-row { display: flex; align-items: center; gap: .4rem; margin-top: -.5rem; margin-bottom: .5rem;
  min-height: 1rem; }
.none-note { display: none; font-size: .75rem; color: #6b7280; }
.none-note.none-on { display: inline; }
.none-row:has(.none-note.none-on) > .none-btn { display: none; }
.rp-none-toggle { margin: 0; cursor: pointer; font-size: 11px; font-family: Consolas, 'Courier New', monospace;
  color: #9ca3af; border: 1px solid #e5e7eb; border-radius: .25rem; padding: 0 .3rem; line-height: 1.5; }
.rp-none-toggle input { display: none; }
.rp-field.rp-field-none .rp-none-toggle { color: #fff; background: var(--bs-primary, #0d6efd);
  border-color: var(--bs-primary, #0d6efd); }
.rp-field-row > .rp-none-show { display: none; flex: 1 1 auto; min-width: 0; font-size: 13px;
  color: #6b7280; font-style: italic; padding: 2px 6px; border: 1px solid #e5e7eb; border-radius: .25rem; }
.rp-field.rp-field-none > .rp-field-row > .rp-form-input,
.rp-field.rp-field-none > .rp-field-row > .rp-r-input { display: none !important; }
.rp-field.rp-field-none > .rp-field-row > .rp-none-show { display: block; }
"

.none_js <- "
$(document).on('shiny:connected', function() {
  // a template field's \"(none)\" button: the value of a text, number or list
  // field becomes \"(none)\" as the button says it (read on leaving the
  // field), a switch is turned off and says so to the server
  window.tplNone = function(id, btn) {
    var el = document.getElementById(id);
    if (!el) return;
    var box = $(el).closest('.shiny-input-container');
    if (el.type === 'checkbox') {
      el.checked = false;
      box.find('.none-note').addClass('none-on');
      Shiny.setInputValue(id + '__none', true, {priority: 'event'});
      return;
    }
    if (el.type === 'number') el.type = 'text';
    el.value = btn ? btn.textContent.trim() : '(none)';
    $(el).trigger('input').trigger('change').trigger('blur');
  };
  // a switch moved by hand is a value again
  $(document).on('change', '.tpl-flag-field input[type=checkbox]', function() {
    $(this).closest('.shiny-input-container').find('.none-note').removeClass('none-on');
    Shiny.setInputValue(this.id + '__none', false);
  });
  // a code card's (none) toggle
  $(document).on('change', '.rp-isnone', function() {
    $(this).closest('.rp-field').toggleClass('rp-field-none', this.checked);
  });
});
"
