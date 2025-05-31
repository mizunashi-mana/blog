export function isTouchDevice(): boolean {
    const touchDocument = (window as any).DocumentTouch

    return ('ontouchstart' in document.documentElement)
        || (touchDocument && document instanceof touchDocument)
        ;
}
