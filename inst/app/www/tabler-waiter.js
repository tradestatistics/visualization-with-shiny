// Add id 'waiter-content' to the main page section so Waiter overlays target it
document.addEventListener('DOMContentLoaded', function() {
  try {
    var section = document.getElementsByClassName('page-body');
    if (section && section.length > 0) {
      section[0].setAttribute('id', 'waiter-content');
    }
  } catch (e) {
    // fail silently
    console && console.warn && console.warn('tabler-waiter.js error:', e);
  }
});
