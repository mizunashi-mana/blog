// http://ajaxian.com/archives/creating-a-queryselector-for-ie-that-runs-at-native-speed

if (!document.querySelectorAll) {
    const querySelectorAllPolyfill = function (selectors: string): HTMLElement[] {
        const styleElement = document.createElement('style');
        document.documentElement.firstChild?.appendChild(styleElement);

        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment -- ポリーフィル定義のため
        const documentWithQsa: {
            __qsa_results?: HTMLElement[] | null;
            // eslint-disable-next-line @typescript-eslint/no-explicit-any -- ポリーフィル定義のため
        } = document as any;
        documentWithQsa.__qsa_results = [];

        // eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-member-access -- ポリーフィル定義のため
        (styleElement as any).styleSheet.cssText = selectors
            + '{x-qsa:expression(document.__qsa_results && document.__qsa_results.push(this))}';
        window.scrollBy(0, 0);
        styleElement.parentNode?.removeChild(styleElement);

        const elements: HTMLElement[] = [];
        for (const element of documentWithQsa.__qsa_results) {
            if ('removeAttribute' in element.style && typeof element.style.removeAttribute === 'function') {
                // eslint-disable-next-line @typescript-eslint/no-unsafe-call -- ポリーフィル定義のため
                element.style.removeAttribute('x-qsa');
            }
            elements.push(element);
        }
        documentWithQsa.__qsa_results = null;
        return elements;
    };

    // eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-assignment -- ポリーフィル定義のため
    document.querySelectorAll = querySelectorAllPolyfill as any;
}

if (!document.querySelector) {
    document.querySelector = function (selectors: string) {
        const elements = document.querySelectorAll(selectors);
        return (elements.length > 0) ? elements[0] : null;
    };
}

export {};
