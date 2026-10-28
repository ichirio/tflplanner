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
.rp-revert { padding: 0 .3rem; font-size: 12px; text-decoration: none; }
.rp-field { margin-bottom: .35rem; min-width: 0; }
.rp-field-label { font-size: 12.5px; margin-bottom: .1rem; display: flex; align-items: center;
  gap: .25rem; flex-wrap: wrap; }
.rp-field-label .rp-req { color: #dc3545; font-weight: 600; }
.rp-field-label .rp-help-link { font-family: Consolas, 'Courier New', monospace; font-size: 11px;
  color: #6b7280; text-decoration: none; }
.rp-field-row { display: flex; align-items: center; gap: .3rem; }
.rp-field-row > .rp-form-input { flex: 1 1 auto; min-width: 0; }
.rp-field-row > .rp-r-input { flex: 1 1 auto; min-width: 0; display: none; }
.rp-field.rp-field-r > .rp-field-row > .rp-form-input { display: none; }
.rp-field.rp-field-r > .rp-field-row > .rp-r-input { display: block; }
.rp-r-toggle { margin: 0; cursor: pointer; font-size: 11px; font-family: Consolas, 'Courier New', monospace;
  color: #9ca3af; border: 1px solid #e5e7eb; border-radius: .25rem; padding: 0 .3rem; line-height: 1.5; }
.rp-r-toggle input { display: none; }
.rp-r-toggle.rp-r-fixed { cursor: default; }
.rp-field.rp-field-r .rp-r-toggle { color: #fff; background: var(--bs-primary, #0d6efd);
  border-color: var(--bs-primary, #0d6efd); }
.rp-field .shiny-input-container { width: 100% !important; margin-bottom: 0; }
.rp-field .form-control, .rp-field .form-select, .rp-field .selectize-input {
  padding: 2px 6px; font-size: 13px; min-height: 0; }
.rp-mono { font-family: Consolas, 'Courier New', monospace; }
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
  $(document).on('change', '.rp-isr', function() {
    $(this).closest('.rp-field').toggleClass('rp-field-r', this.checked);
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
  // a plan card's fields in two groups: those its code writes (above) and
  // those it does not (below, folded).  A value given below moves its
  // field up when the field is left; a field above emptied moves down --
  // each in the function's order (the upper group's data-formals), as the
  // next drawing puts them
  function fieldHas(f) {
    var has = false;
    f.find('input, select, textarea').each(function() {
      if ($(this).hasClass('rp-isr') || this.type === 'search') return;
      if (this.type === 'checkbox' || this.type === 'radio') {
        if (this.checked !== (this.getAttribute('data-init') === '1')) has = true;
      } else if (($(this).val() || '').toString().length > 0) has = true;
    });
    return has;
  }
  function placeField(f, box, wrap, order) {
    var rank = order.indexOf(String(f.data('arg')));
    var el = wrap ? $('<div class=\"rp-group-cell\"></div>').append(f) : f;
    var after = null;
    box.children().each(function() {
      var g = $(this).is('.rp-field') ? $(this) : $(this).find('.rp-field').first();
      var r = order.indexOf(String(g.data('arg')));
      if (r >= 0 && r < rank) after = $(this);
    });
    if (after) after.after(el); else if (wrap) box.prepend(el); else box.children('.rp-more-search').length ? box.children('.rp-more-search').after(el) : box.prepend(el);
  }
  $(document).on('focusout', '.rp-plan-args .rp-field', function() {
    var f = $(this);
    setTimeout(function() {
      if (f[0].contains(document.activeElement)) return;
      var card = f.closest('.rp-plan-args');
      var up = card.find('.rp-args-up .rp-group-row').first();
      var low = card.find('.rp-more-body').first();
      if (!up.length || !low.length) return;
      var order = (card.find('[data-formals]').first().attr('data-formals') || '').split(',');
      var below = f.closest('.rp-more-body').length > 0;
      var has = fieldHas(f);
      if (below && has) {
        placeField(f, up, true, order);
      } else if (!below && !has) {
        var cell = f.parent('.rp-group-cell');
        placeField(f, low, false, order);
        if (cell.length && !cell.children().length) cell.remove();
      }
    }, 0);
  });
  $(document).on('shiny:value', function() {
    setTimeout(function() {
      $('.rp-plan-args input[type=checkbox]:not([data-init])').each(function() {
        this.setAttribute('data-init', this.checked ? '1' : '0');
      });
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
