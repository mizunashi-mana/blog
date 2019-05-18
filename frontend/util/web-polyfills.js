// http://ajaxian.com/archives/creating-a-queryselector-for-ie-that-runs-at-native-speed

if (!document.querySelectorAll) {
  document.querySelectorAll = function(selectors) {
    let styleElement = document.createElement('style');
    document.documentElement.firstChild.appendChild(styleElement);
    document.__qsa_results = [];

    styleElement.styleSheet.cssText = selectors
      + '{x-qsa:expression(document.__qsa_results && document.__qsa_results.push(this))}';
    window.scrollBy(0, 0);
    styleElement.parentNode.removeChild(styleElement);

    const elements = [];
    for (const element of document.__qsa_results) {
      element.style.removeAttribute('x-qsa');
      elements.push(element);
    }
    document.__qsa_results = null;
    return elements;
  };
}

if (!document.querySelector) {
  document.querySelector = function(selectors) {
    let elements = document.querySelectorAll(selectors);
    return (elements.length > 0) ? elements[0] : null;
  };
}
