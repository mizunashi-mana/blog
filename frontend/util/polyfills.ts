import "core-js";

// http://ajaxian.com/archives/creating-a-queryselector-for-ie-that-runs-at-native-speed

if (!document.querySelectorAll) {
    let querySelectorAllPolyfill = function (selectors: any) {
        let styleElement: any = document.createElement('style');
        document.documentElement.firstChild?.appendChild(styleElement);
        (document as any)["__qsa_results"] = [];

        styleElement["styleSheet"].cssText = selectors
        + '{x-qsa:expression(document.__qsa_results && document.__qsa_results.push(this))}';
        window.scrollBy(0, 0);
        styleElement.parentNode?.removeChild(styleElement);

        const elements: unknown[] = [];
        for (const element of (document as any)["__qsa_results"]) {
            element.style.removeAttribute('x-qsa');
            elements.push(element);
        }
        (document as any)["__qsa_results"] = null;
        return elements;
    };

    document.querySelectorAll = querySelectorAllPolyfill as any;
}

if (!document.querySelector) {
    document.querySelector = function (selectors: any) {
        let elements = document.querySelectorAll(selectors);
        return (elements.length > 0) ? elements[0] : null;
    };
}
