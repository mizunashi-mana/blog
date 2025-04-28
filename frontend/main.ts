import './util/polyfills';
import './lib/fontawesome';
import './lib/katex';
import './component/sentry_monitor';
import * as addFootnoteTooltip from './component/add_footnote_tooltip';

function onLoad(): void {
    addFootnoteTooltip.addFootnoteTooltip();
}

/*!
 * @license MIT https://github.com/jquery/jquery
 * Copyright JS Foundation and other contributors, https://js.foundation/
 */
/**
 * ref: https://github.com/jquery/jquery/blob/3.4.0/src/core/ready.js#L60
 */
function completed() {
    document.removeEventListener("DOMContentLoaded", completed);
    window.removeEventListener("load", completed);
    onLoad();
}

if (
    document.readyState === "complete"
    || document.readyState !== "loading" && !("doScroll" in document.documentElement)
) {
    window.setTimeout(onLoad);
} else {
    document.addEventListener("DOMContentLoaded", completed);

    // A fallback to window.onload, that will always work
    window.addEventListener("load", completed);
}
