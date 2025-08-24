export function isTouchDevice(): boolean {
    if ('ontouchstart' in document.documentElement) {
        return true;
    }

    if (!('DocumentTouch' in window)) {
        return false;
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any -- ポリーフィル定義のため
    return document instanceof (window.DocumentTouch as any);
}
