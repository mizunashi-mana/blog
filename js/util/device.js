export function isTouchDevice() {
  return ('ontouchstart' in document.documentElement)
    || (window.DocumentTouch && document instanceof DocumentTouch)
    ;
}
