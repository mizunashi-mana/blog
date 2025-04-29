import './lib/fontawesome';
import './lib/katex';
import './component/sentry_monitor';

/*!
 * @license MIT https://github.com/jquery/jquery
 * Copyright JS Foundation and other contributors, https://js.foundation/
 */
async function main(): Promise<void> {
    await import('./util/polyfills');

    const addFootnoteTooltip = import('./component/add_footnote_tooltip');
    const onLoad = () => {
        void (async () => await (await addFootnoteTooltip).addFootnoteTooltip())();
    }

    /**
     * ref: https://github.com/jquery/jquery/blob/3.4.0/src/core/ready.js#L60
     */
    const completed = () => {
        document.removeEventListener("DOMContentLoaded", completed);
        window.removeEventListener("load", completed);
        void onLoad();
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
}

void main();
