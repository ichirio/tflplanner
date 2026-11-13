# The style and script of the builder cards.
# Split from R/builder_cards.R by section; no code changed.

# ---- style and script -----------------------------------------------------------------------

.builder_cards_css <- "
.rp-card { border: 1px solid var(--bs-border-color, #dee2e6); border-radius: .5rem;
  margin-bottom: .6rem; background: #fff; }
.rp-card.rp-card-required { border-left: 4px solid var(--bs-primary, #0d6efd); }
.rp-card.rp-card-off { opacity: .6; }
.rp-card.rp-card-off .rp-card-body { display: none; }
.rp-card.rp-card-selected { box-shadow: 0 0 0 2px var(--bs-primary, #0d6efd); }
.rp-card.rp-card-error { border-color: #dc3545; }
.rp-card-head { display: flex; align-items: center; gap: .4rem; padding: .35rem .6rem;
  flex-wrap: wrap; }
.rp-card-bar { width: .25rem; align-self: stretch; margin-left: -.25rem; }
.rp-card-switch { margin: 0; padding: 0; min-height: 0; }
.rp-card-switch .form-check-input { margin: 0; cursor: pointer; }
.rp-card-toggle { border: 0; background: none; padding: 0 .2rem; color: #6b7280;
  font-size: 13px; width: 1.1em; }
.rp-card-toggle::before { content: '\\25b8'; }
.rp-card.rp-card-open > .rp-card-head .rp-card-toggle::before { content: '\\25be'; }
.rp-card-title { font-weight: 600; }
.rp-card-req { font-size: 11px; color: #9ca3af; font-weight: normal; }
.rp-card-sep { display: flex; align-items: center; gap: .5rem; margin: .7rem 0 .4rem;
  font-size: 11.5px; color: #6b7280; letter-spacing: .02em; }
.rp-card-sep::after { content: ''; flex: 1 1 auto; border-top: 1px solid #e5e7eb; }
.rp-cards-bar { display: flex; align-items: center; gap: .4rem; margin-bottom: .4rem; }
.rp-cards-bar .dropdown-menu { max-height: 60vh; overflow-y: auto; font-size: 13px; }
.rp-cards-bar .dropdown-item.rp-layer-rare { color: #6b7280; }
.rp-card.rp-card-rare > .rp-card-head { padding-top: .2rem; padding-bottom: .2rem; }
.rp-card-note { font-size: 12.5px; color: #4b5563; margin: .1rem 0 .35rem; }
.rp-card-summary { color: #6b7280; font-size: 12px; margin-left: .2rem; }
.rp-card.rp-card-open .rp-card-summary { display: none; }
.rp-card-err { color: #dc3545; font-size: 12px; }
.rp-card-menu { margin-left: auto; }
.rp-card-dots { color: #6b7280; text-decoration: none; padding: 0 .3rem; font-size: 16px; }
.rp-card-body { padding: .2rem .8rem .6rem; }
.rp-card:not(.rp-card-open) .rp-card-body { display: none; }
.rp-label .rp-name { font-family: Consolas, 'Courier New', monospace; font-size: .85em;
  color: #6b7280; font-weight: normal; background: none; padding: 0; }
.rp-label .rp-name-only { color: #374151; font-size: .95em; }
.rp-badge { display: inline-block; font-size: 11px; line-height: 1.3; padding: 0 .4rem;
  border-radius: .6rem; background: var(--bs-tertiary-bg, #f3f4f6); color: #4b5563;
  border: 1px solid #e5e7eb; white-space: nowrap; vertical-align: middle; }
.rp-badge-own { background: #ecfdf5; border-color: #a7f3d0; color: #047857; }
.rp-badge-absent { background: none; border-style: dashed; color: #6b7280; }
.rp-card.rp-card-absent { border-style: dashed; }
.rp-card.rp-card-absent > .rp-card-head .rp-card-title { color: #6b7280; }
.rp-badge-standards { background: #eef6ff; border-color: #cfe2ff; color: #1d4ed8; }
.rp-badge-template { background: #f5f3ff; border-color: #ddd6fe; color: #6d28d9; }
.rp-badge-ard { background: #fff7ed; border-color: #fed7aa; color: #9a3412; }
.rp-card-stage { font-size: 10.5px; color: #9ca3af; margin-left: auto; white-space: nowrap; }
.rp-card-stage + .rp-card-menu { margin-left: 0; }
.rp-card-sep-note { color: #9ca3af; letter-spacing: 0; }
.rp-revert { padding: 0 .3rem; font-size: 12px; text-decoration: none; white-space: nowrap; flex-shrink: 0; }
/* the template card's Revert (one line, its text is the width it needs) */
button[onclick*='tpl_revert'] { white-space: nowrap; flex-shrink: 0; }
.rp-field { margin-bottom: .35rem; min-width: 0; }
.rp-field-label { font-size: 12.5px; margin-bottom: .1rem; display: flex; align-items: center;
  gap: .25rem; flex-wrap: wrap; }
.rp-field-label .rp-req { color: #dc3545; font-weight: 600; }
.rp-field-label .rp-help-link { font-family: Consolas, 'Courier New', monospace; font-size: 11px;
  color: #6b7280; text-decoration: none; }
.rp-field-row { display: flex; align-items: center; gap: .3rem; }
.rp-field-row > .rp-reset, .rp-field-row > .rp-r-toggle, .rp-field-row > .rp-r-label { flex: 0 0 auto; }
.rp-field-row > .rp-form-input { flex: 1 1 auto; min-width: 0; }
.rp-field-row > .rp-r-input { flex: 1 1 auto; min-width: 0; display: none; }
.rp-field.rp-field-r:not(.rp-field-ronly) > .rp-field-row > .rp-form-input { display: none; }
.rp-field.rp-field-r:not(.rp-field-ronly) > .rp-field-row > .rp-r-input { display: block; }
/* (an argument that is R only: its field is the R field, never hidden) */
.rp-field.rp-field-ronly > .rp-field-row > .rp-form-input { display: block; }
.rp-field-locked .selectize-control.multi .selectize-input.disabled > div { color: #4b5563;
  background: #e5e7eb; }
/* the R switch: its checkbox is hidden visually, not removed (the keyboard
   reaches it; the label shows the focus) */
.rp-r-toggle { position: relative; margin: 0; cursor: pointer; font-size: 11px;
  font-family: Consolas, 'Courier New', monospace; color: #6b7280; border: 1px solid #d1d5db;
  border-radius: .25rem; padding: 0 .35rem; line-height: 1.6; user-select: none; }
.rp-r-toggle input { position: absolute; opacity: 0; width: 1px; height: 1px; margin: 0;
  left: 50%; top: 50%; pointer-events: none; }
.rp-r-toggle:focus-within { outline: 2px solid var(--bs-primary, #0d6efd); outline-offset: 1px; }
.rp-r-toggle:hover { border-color: #9ca3af; }
.rp-field.rp-field-r:not(.rp-field-ronly) .rp-r-toggle { color: #fff; background: var(--bs-primary, #0d6efd);
  border-color: var(--bs-primary, #0d6efd); }
/* an R-only argument: a plain grey letter, not a switch */
.rp-r-label { font-size: 11px; font-family: Consolas, 'Courier New', monospace; color: #9ca3af;
  padding: 0 .35rem; cursor: default; }
.rp-field .shiny-input-container { width: 100% !important; margin-bottom: 0; }
/* one height, inset and font size for the select, the text and the number */
.rp-field .form-control, .rp-field .form-select, .rp-field .selectize-input {
  box-sizing: border-box; height: 30px; min-height: 30px; padding: 3px 8px; font-size: 13px;
  line-height: 22px; }
.rp-field .selectize-control { margin: 0; }
.rp-field .selectize-control.multi .selectize-input { height: auto; padding: 2px 6px; }
.rp-field .selectize-control.multi .selectize-input > div { font-size: 13px; line-height: 20px;
  margin: 1px 3px 1px 0; }
.rp-field .selectize-input > input, .rp-field .selectize-input .item { font-size: 13px !important;
  line-height: 22px !important; }
.rp-field .selectize-control.single .selectize-input:after { right: 8px; }
.rp-field .form-control::placeholder, .rp-field .selectize-input > input::placeholder {
  color: #9ca3af; font-style: italic; opacity: 1; font-size: 13px; }
/* set (the report's own value): a bar on the left and the reset */
.rp-field.rp-set .form-control, .rp-field.rp-set .form-select, .rp-field.rp-set .selectize-input {
  box-shadow: inset 3px 0 0 var(--bs-primary, #0d6efd); border-color: #93b4f5; }
.rp-reset { display: none; border: 0; background: none; color: #6b7280; padding: 0 .3rem;
  font-size: 17px; line-height: 1; cursor: pointer; }
.rp-reset:hover { color: #dc3545; }
.rp-field.rp-set .rp-reset { display: inline-block; }
/* a switch: dimmed while it shows the default, normal and barred once set */
.rp-field-logical .rp-form-input { padding-left: 3px; border-left: 3px solid transparent; }
.rp-field-logical:not(.rp-set) .rp-check { opacity: .45; }
.rp-field-logical.rp-set .rp-form-input { border-left-color: var(--bs-primary, #0d6efd); }
.rp-mono { font-family: Consolas, 'Courier New', monospace; }
.rp-badge-locked { background: #f3f4f6; border-color: #d1d5db; color: #4b5563; }
.rp-badge-locked .rp-lock { vertical-align: -1px; }
.rp-badge-unset { background: #fef2f2; border-color: #fecaca; color: #b91c1c; }
.rp-field:not(.rp-field-unset) .rp-badge-unset, .rp-field-unset .rp-req { display: none; }
/* locked: a grey fill, the text in full contrast, no fading */
.rp-field-locked .form-control, .rp-field-locked .form-select, .rp-field-locked .selectize-input,
.rp-field-locked .selectize-input.disabled { background: #f3f4f6 !important; color: #111827 !important;
  opacity: 1 !important; box-shadow: none; }
.rp-field-locked .selectize-input .item, .rp-field-locked .selectize-input.disabled .item {
  color: #111827 !important; opacity: 1 !important; }
.rp-field-locked .selectize-control.single .selectize-input:after { opacity: .4; }
.rp-field-unset .selectize-input, .rp-field-unset .form-control, .rp-field-unset .form-select {
  border-color: #fca5a5; }
/* (selectize copies the select's classes to its wrapper: the wrapper is no box) */
.rp-field .selectize-control.form-control, .rp-field .selectize-control.form-select {
  height: auto !important; min-height: 0 !important; padding: 0 !important; border: 0 !important;
  box-shadow: none !important; background: transparent !important; }
.rp-check { margin: 0; padding: 0; min-height: 0; }
.rp-check .form-check-input { margin: 0; }
.rp-swatch { display: inline-block; width: .9em; height: .9em; border: 1px solid #9ca3af;
  border-radius: .15rem; margin-right: .35rem; vertical-align: -.1em; }
.rp-call { border-left: 2px solid #e5e7eb; padding-left: .5rem; }
.rp-map-row { display: grid; grid-template-columns: 4.5em minmax(0, 1fr) auto; gap: .3rem;
  align-items: center; margin-bottom: .2rem; }
.rp-map-key { font-family: Consolas, 'Courier New', monospace; font-size: 12px; color: #374151; }
.rp-map-row .rp-map-value { display: none; }
.rp-map-row.rp-map-val .rp-map-col { display: none; }
.rp-map-row.rp-map-val .rp-map-value { display: block; }
.rp-map-switch { margin: 0; cursor: pointer; font-size: 11px; color: #6b7280;
  border: 1px solid #e5e7eb; border-radius: .25rem; padding: 0 .3rem; white-space: nowrap; }
.rp-map-switch input { display: none; }
.rp-map-switch .rp-map-val-lab, .rp-map-row.rp-map-val .rp-map-switch .rp-map-col-lab { display: none; }
.rp-map-row.rp-map-val .rp-map-switch .rp-map-val-lab { display: inline; }
.rp-group { margin-bottom: .35rem; }
.rp-group-label { font-size: 12.5px; font-weight: 600; color: #4b5563; margin-bottom: .1rem; }
.rp-group-row { display: flex; gap: .6rem; flex-wrap: wrap; }
.rp-group-row > .rp-group-cell { flex: 1 1 8em; min-width: 0; }
/* a field of several values (its chips wrap: a box taller than a text's)
   on a row of its own, the others side by side under it */
.rp-group-row > .rp-group-cell:has(> .rp-field-list) { flex-basis: 100%; }
.rp-more { margin-top: .3rem; }
.rp-more-toggle { border: 0; background: none; padding: 0; font-size: 12.5px; color: #6b7280; }
.rp-more-arrow { display: inline-block; transition: transform .15s; }
.rp-more.rp-open .rp-more-arrow { transform: rotate(90deg); }
.rp-more-body { padding-left: .6rem; margin-top: .2rem; }
.rp-more:not(.rp-open) .rp-more-search { display: none; }
.rp-more:not(.rp-open) .rp-more-body > .rp-field:not(.rp-has-value) { display: none; }
.rp-more-search { margin-bottom: .3rem; max-width: 18em; }
.rp-more .rp-field.rp-hidden { display: none !important; }
.rp-nav-hint { font-size: 12.5px; color: #4b5563; background: var(--bs-tertiary-bg, #f6f7f9);
  border-left: 3px solid #cbd5e1; padding: .3rem .6rem; margin: .2rem 0 .5rem; border-radius: 0 .3rem .3rem 0; }
.rp-nav-hint.rp-nav-field { font-size: 12px; padding: .15rem .5rem; margin: .1rem 0 .3rem; }
html:not(.rp-nav-on):not(.rp-nav) .rp-nav-hint { display: none; }
.rp-code-view pre .rp-ln.rp-ln-hl { background: #fff7cc; }
.rp-code-view pre .rp-ln[data-path] { cursor: pointer; }
.rp-add-item { display: flex; gap: .4rem; align-items: center; margin: .3rem 0 .6rem; }
.rp-add-item .shiny-input-container { flex: 1 1 auto; margin-bottom: 0; }
"

# the cards' behaviour: open / close remembered in this browser, the R
# and the column / value switches, the folded arguments' search and
# promotion, Code line <-> card, Enter adds an item
.builder_cards_js <- "
(function() {
  function store(k, v) { try { window.localStorage.setItem(k, v); } catch (e) {} }
  function read(k) { try { return window.localStorage.getItem(k); } catch (e) { return null; } }
  $(document).on('click', '.rp-card-toggle', function() {
    var card = $(this).closest('.rp-card');
    card.toggleClass('rp-card-open');
    store('rp-card-open:' + card.data('card'), card.hasClass('rp-card-open') ? '1' : '0');
  });
  $(document).on('shiny:value shiny:visualchange', function() {
    setTimeout(function() {
      $('.rp-card').each(function() {
        var v = read('rp-card-open:' + $(this).data('card'));
        if (v === '1') $(this).addClass('rp-card-open');
        if (v === '0') $(this).removeClass('rp-card-open');
      });
    }, 0);
  });
  // a card's mark and Revert after a write, without drawing the form again
  $(document).on('shiny:connected', function() {
    Shiny.addCustomMessageHandler('rp-card-mark', function(x) {
      var head = $('#' + x.card + ' > .rp-card-head');
      if (!head.length) return;
      var old = head.children('.rp-badge, .rp-revert');
      Shiny.unbindAll(old.get(0) ? old.parent().get(0) : head.get(0));
      old.remove();
      var at = head.children('.rp-card-req');
      if (!at.length) at = head.children('.rp-card-title');
      at.after(x.html);
      // the summary line (what the card says when closed)
      if (typeof x.summary === 'string') {
        var sm = head.children('.rp-card-summary');
        if (!sm.length && x.summary.length) {
          sm = $(document.createElement('span')).addClass('rp-card-summary');
          head.children('.rp-badge, .rp-revert').last().after(sm);
        }
        sm.text(x.summary);
      }
      Shiny.bindAll(head.get(0));
    });
  });
  // a field of a value the ARD decides: warned while it says another
  $(document).on('change', '.rp-field[data-ard] select', function() {
    var f = $(this).closest('.rp-field');
    var ard = String(f.attr('data-ard')).split('|');
    var v = this.selectize ? this.selectize.getValue() : $(this).val();
    if (!Array.isArray(v)) v = v ? [v] : [];
    var same = v.length === 0 || (v.length === ard.length &&
      v.every(function(x, i) { return x === ard[i]; }));
    f.find('.rp-ard-warn').toggle(!same);
  });
  $(document).on('change', '.rp-card-switch input', function() {
    $(this).closest('.rp-card').toggleClass('rp-card-off', !this.checked);
  });
  $(document).on('change', '.rp-isval', function() {
    $(this).closest('.rp-map-row').toggleClass('rp-map-val', this.checked);
  });
  $(document).on('click', '.rp-more-toggle', function() {
    $(this).closest('.rp-more').toggleClass('rp-open');
  });
  $(document).on('input', '.rp-more-search', function() {
    var q = this.value.toLowerCase();
    $(this).closest('.rp-more-body').find('.rp-field').each(function() {
      var name = ($(this).data('arg') || '').toString().toLowerCase();
      $(this).toggleClass('rp-hidden', q.length > 0 && name.indexOf(q) < 0);
    });
  });
  // a value typed into a folded argument keeps its row shown when the fold closes
  $(document).on('change input', '.rp-more .rp-field input, .rp-more .rp-field select', function() {
    var f = $(this).closest('.rp-field');
    var has = $(this).is(':checkbox') ? this.checked : (this.value || '').length > 0;
    f.toggleClass('rp-has-value', has);
  });
  // ---- a field's state: default / set (the changed mark and its reset) -----------
  // Nothing here reaches the server by itself: the report is read when Update
  // is pressed.  The marks are drawn from the controls' own values.
  window.rpQuiet = false;
  function formCtl(f) {
    return f.children('.rp-field-row').children('.rp-form-input').find('select, input, textarea')
      .filter(function() {
        return !$(this).closest('.selectize-control').length && $(this).closest('.rp-field')[0] === f[0];
      }).first();
  }
  function isRMode(f) { return f.hasClass('rp-field-r') && !f.hasClass('rp-field-ronly'); }
  function fieldHasValue(f) {
    if (isRMode(f)) return (f.children('.rp-field-row').children('.rp-r-input').val() || '').length > 0;
    if (f.hasClass('rp-field-logical')) return f.attr('data-flag-set') === '1';
    var c = formCtl(f);
    if (!c.length) return false;
    var v = c.val();
    if (c.is('select')) return Array.isArray(v) ? v.length > 0 : !!v;
    return (v || '').length > 0;
  }
  function refreshField(f) {
    if (!f.children('.rp-field-row').children('.rp-reset').length) return;
    var has = fieldHasValue(f);
    f.toggleClass('rp-set', has);
    if (f.attr('data-nodefault') === '1') f.toggleClass('rp-field-unset', !has);
  }
  // the field's current value written as R: 'a' in double quotes, c() of
  // several, a number as it is, TRUE / FALSE
  function valueAsR(f) {
    var c = formCtl(f);
    if (f.hasClass('rp-field-logical')) {
      return f.attr('data-flag-set') === '1' && c.length ? (c.prop('checked') ? 'TRUE' : 'FALSE') : '';
    }
    if (!c.length) return '';
    var v = c.val();
    if (Array.isArray(v)) {
      if (!v.length) return '';
      return v.length === 1 ? JSON.stringify(v[0]) : 'c(' + v.map(function(x) { return JSON.stringify(x); }).join(', ') + ')';
    }
    if (!v) return '';
    return c.is('select') || c.attr('type') !== 'number' ? JSON.stringify(v) : v;
  }
  function focusCtl(f) {
    var c = formCtl(f);
    if (c.length && c[0].selectize) c[0].selectize.focus(); else c.trigger('focus');
  }
  $(document).on('input change', '.rp-field input, .rp-field select, .rp-field textarea', function() {
    if (window.rpQuiet || $(this).is('.rp-isr')) return;
    var f = $(this).closest('.rp-field');
    if ($(this).is(':checkbox') && f.hasClass('rp-field-logical')) f.attr('data-flag-set', '1');
    var ch = f.data('rpChips');
    if (ch) ch.touched = true;
    refreshField(f);
  });
  $(document).on('click', '.rp-reset', function() {
    var f = $(this).closest('.rp-field');
    var ri = f.children('.rp-field-row').children('.rp-r-input');
    if (ri.length && ri.val()) ri.val('').trigger('input').trigger('change');
    if (isRMode(f)) {
      f.removeClass('rp-field-r');
      f.find('.rp-isr').first().prop('checked', false);
    }
    var c = formCtl(f);
    if (c.length && c[0].selectize) {
      c[0].selectize.clear();
    } else if (f.hasClass('rp-field-logical')) {
      c.prop('checked', f.attr('data-default') === 'true').trigger('change');
    } else if (c.length) {
      c.val('').trigger('input').trigger('change');
    }
    f.attr('data-flag-set', '0');
    f.removeData('rpChips');
    refreshField(f);
  });
  // the R switch: on, the R input takes the value written as R and the focus;
  // off, the form control takes the focus
  $(document).on('change', '.rp-isr', function() {
    var f = $(this).closest('.rp-field');
    f.toggleClass('rp-field-r', this.checked);
    var ri = f.children('.rp-field-row').children('.rp-r-input');
    if (this.checked) {
      if (ri.length && !ri.val()) {
        var s = valueAsR(f);
        if (s) ri.val(s).trigger('input').trigger('change');
      }
      ri.trigger('focus');
    } else {
      focusCtl(f);
    }
    refreshField(f);
  });
  // a list: the first focus fills the default as chips, so one can add a
  // variable without typing the rest; left alone, it goes back to the
  // default (nothing is written)
  $(document).on('focusin', '.rp-field-list .selectize-input', function() {
    var f = $(this).closest('.rp-field');
    if (f.hasClass('rp-field-locked') || f.data('rpChips')) return;
    var c = formCtl(f), inst = c.length ? c[0].selectize : null, d = [];
    try { d = JSON.parse(f.attr('data-default') || '[]'); } catch (e) { d = []; }
    if (!inst || !d.length || inst.items.length) return;
    d.forEach(function(v) { if (!inst.options[v]) inst.addOption({value: v, label: v}); });
    inst.addItems(d, true);
    f.data('rpChips', {touched: false});
  });
  $(document).on('focusout', '.rp-field-list', function() {
    var f = $(this);
    setTimeout(function() {
      var ch = f.data('rpChips');
      if (!ch || ch.touched || f[0].contains(document.activeElement)) return;
      var c = formCtl(f);
      if (c.length && c[0].selectize) c[0].selectize.clear(true);
      f.removeData('rpChips');
    }, 0);
  });
  // Code line <-> card
  function jumpMap(pre) {
    var s = document.querySelector('.rp-code-jump[data-for=\"' + pre.id + '\"]');
    if (!s) return null;
    try { return JSON.parse(s.textContent); } catch (e) { return null; }
  }
  // (again whenever an output comes: the code and its map arrive apart)
  function applyJumps() {
    $('.rp-code-view pre').each(function() {
      var pre = this, m = jumpMap(pre);
      if (!m) return;
      $(pre).find('.rp-ln').each(function(i) {
        var p = m[String(i + 1)];
        if (p) this.setAttribute('data-path', p); else this.removeAttribute('data-path');
      });
    });
  }
  $(document).on('shiny:value', function() { setTimeout(applyJumps, 20); });
  // a line's card: selected, opened and scrolled into view in the cards'
  // column only -- the code beside it stays where it is (its column is
  // sticky, its own scroll); narrow, where the code is below the cards, the
  // card offers the way back to the line
  $(document).on('click', '.rp-code-view .rp-ln[data-path]', function() {
    var p = this.getAttribute('data-path'), line = this;
    $('.rp-card').removeClass('rp-card-selected');
    $('.rp-back-code').remove();
    var card = $('.rp-card[data-path=\"' + p + '\"]');
    card.addClass('rp-card-selected rp-card-open');
    if (card.length) {
      var right = $(line).closest('.rp-b-right')[0];
      var keep = right ? right.scrollTop : null;
      card[0].scrollIntoView({block: 'nearest'});
      if (right) right.scrollTop = keep;
      if (window.innerWidth < 992) {
        var txt = card.closest('.rp-builder-cards').data('back') || 'Back to the code';
        var b = $('<button type=\"button\" class=\"btn btn-link btn-sm rp-back-code\"></button>')
          .text('\\u2193 ' + txt);
        b.on('click', function(e) {
          e.stopPropagation();
          line.scrollIntoView({block: 'center', behavior: 'smooth'});
          b.remove();
        });
        card.find('> .rp-card-head .rp-card-title').after(b);
      }
    }
    if (window.Shiny) Shiny.setInputValue('rp_card_selected', p, {priority: 'event'});
  });
  // a card's field in focus lights its lines (and those of the items it
  // names in data-also: the decimals light plan_cells()'s formats)
  $(document).on('focusin', '.rp-card', function() {
    var ps = [this.getAttribute('data-path')]
      .concat((this.getAttribute('data-also') || '').split(' ').filter(Boolean));
    $('.rp-ln').removeClass('rp-ln-hl');
    ps.forEach(function(p) { $('.rp-ln[data-path=\"' + p + '\"]').addClass('rp-ln-hl'); });
  });
  // the Layers menu: a card chosen is opened, selected and brought into view
  $(document).on('click', '.rp-layer-go', function(e) {
    e.preventDefault();
    var card = $('#' + $(this).data('target'));
    if (!card.length) return;
    $('.rp-card').removeClass('rp-card-selected');
    card.addClass('rp-card-selected rp-card-open');
    card[0].scrollIntoView({block: 'start', behavior: 'smooth'});
  });
  // a card asked for from elsewhere (the AI tab's \"to confirm\": its item,
  // the verb): opened, selected and brought into view once the builder has
  // drawn it -- tried for a few seconds, the report's form drawn in between
  Shiny.addCustomMessageHandler('rp-card-go', function(x) {
    var tries = 0;
    (function look() {
      var card = $('.rp-card[data-path=\"' + x.path + '\"]').first();
      if (!card.length) { if (++tries < 30) setTimeout(look, 200); return; }
      $('.rp-card').removeClass('rp-card-selected');
      card.addClass('rp-card-selected rp-card-open');
      card[0].scrollIntoView({block: 'start', behavior: 'smooth'});
    })();
  });
  // Enter in the add field adds; the new card's first field takes the focus
  var wantFocus = false;
  $(document).on('keydown', '.rp-add-item .selectize-input input', function(e) {
    if (e.key === 'Enter') {
      var box = $(this).closest('.rp-add-item');
      setTimeout(function() { box.find('.rp-add-btn').trigger('click'); }, 50);
    }
  });
  $(document).on('click', '.rp-add-btn', function() { wantFocus = true; });
  $(document).on('shiny:value', function() {
    if (!wantFocus) return;
    setTimeout(function() {
      // the card just added (marked new), else the last
      var card = $('.rp-card.rp-card-new').first();
      if (!card.length) card = $('.rp-card').last();
      if (!card.length) return;
      card[0].scrollIntoView({block: 'nearest'});
      var f = card.find('input:visible, select:visible').first();
      if (f.length) { f.trigger('focus'); wantFocus = false; }
    }, 50);
  });
})();
"
