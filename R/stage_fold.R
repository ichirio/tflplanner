# [Collapse all] / [Expand all] at the top of each step of the report screen
# (#835): one bar, one script and one style for every step.  A collapsed
# card is its title line, as the content step's builder cards are when
# closed.  Collapsing only hides: it changes no value and writes nothing to
# the study.
#
# The cards of a step: the builder cards (.rp-card: closed as their own
# toggle closes them, the same remembered state in this browser) and the
# cards with a title line of their own -- a bslib card's header, an h6 first
# in a .pg-card or .rp-b-card (folded with .rp-fold-closed, for this page
# only; a click on the title line opens one).  A card inside another is
# folded with it; a card whose header holds tabs is a pane, not a card.
# Collapse all leaves open a card with an error, and one whose value was
# changed since the last Collapse all.

# The bar at the top of the step `stage` ("data", "ard", "content", "page")
.stage_fold_bar <- function(stage, lang = "en") {
  t <- function(x) tr(x, lang)
  btn <- function(what, label) {
    shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary py-0",
                       `data-fold` = what, label)
  }
  shiny::div(class = "rp-fold-bar", `data-fold-stage` = stage,
             btn("close", t("Collapse all")), btn("open", t("Expand all")))
}

.stage_fold_css <- "
.rp-fold-bar { display: flex; justify-content: flex-end; gap: .3rem; margin-bottom: .4rem; }
.rp-fold-closed { border: 1px solid var(--bs-border-color, #dee2e6) !important;
  border-radius: .5rem !important; padding: 0 !important; background: #fff; }
.rp-fold-closed > :not(:first-child) { display: none !important; }
.rp-fold-closed > .card-header, .rp-fold-closed > h6 { display: block; margin: 0 !important;
  padding: .35rem .6rem; border: 0; background: none; font-size: 1rem; font-weight: 600;
  cursor: pointer; }
.rp-fold-closed > .card-header::before, .rp-fold-closed > h6::before {
  content: '\\25b8'; display: inline-block; color: #6b7280; font-size: 13px;
  font-weight: normal; width: 1.1em; padding: 0 .2rem; margin-right: .4rem; }
"

.stage_fold_js <- "
(function() {
  var CARDS = '.rp-card, .card, .pg-card, .rp-b-card';
  var folded = {};
  function store(k, v) { try { window.localStorage.setItem(k, v); } catch (e) {} }
  function head(c) {
    var h = c.children().first();
    return h.is('.card-header, h6') ? h : null;
  }
  function foldable(c) {
    if (c.is('.rp-card')) return true;
    var h = head(c);
    return !!h && !h.find('.nav').length;
  }
  function key(c) { return $.trim(head(c).text()); }
  function cards(scope) {
    return scope.find(CARDS).filter(function() {
      var c = $(this);
      if (!foldable(c)) return false;
      var up = c.parentsUntil(scope, CARDS).filter(function() { return foldable($(this)); });
      return !up.length;
    });
  }
  function keep(c) {
    return c.is('.rp-card-error') || c.attr('data-fold-changed') === '1' ||
      c.find('.rp-card-err, .alert-danger, .is-invalid, ' +
             '.shiny-output-error:not(.shiny-output-error-validation)').length > 0;
  }
  function fold(scope, close) {
    var st = scope.attr('data-fold-stage');
    folded[st] = folded[st] || {};
    cards(scope).each(function() {
      var c = $(this);
      if (close && keep(c)) {
        c.removeAttr('data-fold-changed');
        return;
      }
      if (c.is('.rp-card')) {
        c.toggleClass('rp-card-open', !close);
        if (c.data('card')) store('rp-card-open:' + c.data('card'), close ? '0' : '1');
      } else {
        c.toggleClass('rp-fold-closed', close);
        if (close) folded[st][key(c)] = true; else delete folded[st][key(c)];
      }
    });
    if (close) scope.find('[data-fold-changed]').removeAttr('data-fold-changed');
  }
  $(document).on('click', '.rp-fold-bar [data-fold]', function() {
    fold($(this).closest('.rp-fold-scope'), $(this).attr('data-fold') === 'close');
  });
  // a value changed: its card stays open at the next Collapse all
  $(document).on('change', '.rp-fold-scope', function(e) {
    $(e.target).parents(CARDS).attr('data-fold-changed', '1');
  });
  // a folded card's title line opens it (not its own buttons and fields)
  $(document).on('click', '.rp-fold-closed > .card-header, .rp-fold-closed > h6', function(e) {
    if ($(e.target).closest('button, a, select, input, label, .selectize-control').length) return;
    var c = $(this).parent();
    var st = c.closest('.rp-fold-scope').attr('data-fold-stage');
    c.removeClass('rp-fold-closed');
    if (folded[st]) delete folded[st][key(c)];
  });
  // a card drawn again stays folded
  $(document).on('shiny:value shiny:visualchange', function() {
    setTimeout(function() {
      $('.rp-fold-scope').each(function() {
        var scope = $(this), f = folded[scope.attr('data-fold-stage')];
        if (!f) return;
        cards(scope).not('.rp-card').each(function() {
          var c = $(this);
          if (f[key(c)]) c.addClass('rp-fold-closed');
        });
      });
    }, 0);
  });
})();
"
