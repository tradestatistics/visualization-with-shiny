(function () {
  // Utility to set equal heights for column pairs that share an id prefix
  // e.g. an element with id "aggregated_trade_left" and a matching
  // "aggregated_trade_right". Only those pairs are equalized.
  function equalizeRowHeights() {
    // Reset any previous inline heights for all cards inside paired columns
    var allPairedCols = document.querySelectorAll('[id$="_left"], [id$="_right"]');
    allPairedCols.forEach(function (col) {
      var card = col.querySelector && col.querySelector('.card');
      if (card) card.style.height = '';
    });

    // Find all _left columns and match to corresponding _right
    var leftCols = document.querySelectorAll('[id$="_left"]');
    leftCols = Array.prototype.slice.call(leftCols);

    leftCols.forEach(function (left) {
      var id = left.id;
      if (!id) return;
      var prefix = id.replace(/_left$/, '');
      var right = document.getElementById(prefix + '_right');

      // If there's no right partner, skip
      if (!right) return;

      var leftCard = left.querySelector('.card');
      var rightCard = right.querySelector('.card');

      var leftH = leftCard ? leftCard.getBoundingClientRect().height : 0;
      var rightH = rightCard ? rightCard.getBoundingClientRect().height : 0;
      var pairMax = Math.max(leftH, rightH);
      if (!isFinite(pairMax) || pairMax <= 0) return;

      if (leftCard) leftCard.style.height = pairMax + 'px';
      if (rightCard) rightCard.style.height = pairMax + 'px';
    });
  }

  // Run on DOM ready
  document.addEventListener('DOMContentLoaded', function () {
    // Initial equalize
    equalizeRowHeights();

    // Re-run on window resize
    var resizeTimer = null;
    window.addEventListener('resize', function () {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(equalizeRowHeights, 150);
    });

    // Observe mutations inside rows to handle chart/library async renders
    var observer = new MutationObserver(function (mutations) {
      // Very small debounce
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(equalizeRowHeights, 50);
    });

    var rows = document.querySelectorAll('.row-equal-height');
    rows.forEach(function (row) {
      observer.observe(row, { childList: true, subtree: true, attributes: true });
    });

    // Also observe for rows being added later (e.g., when tab shown)
    var bodyObserver = new MutationObserver(function (mutations) {
      var added = false;
      mutations.forEach(function (m) {
        if (m.addedNodes && m.addedNodes.length) {
          m.addedNodes.forEach(function (n) {
            if (n.nodeType === 1 && n.querySelector && n.querySelector('.row-equal-height')) added = true;
          });
        }
      });
      if (added) {
        equalizeRowHeights();
      }
    });
    bodyObserver.observe(document.body, { childList: true, subtree: true });

    // Also run once after a short delay to catch late-loaded charts
    setTimeout(equalizeRowHeights, 500);
  });
})();
