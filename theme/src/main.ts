import './lib/fontawesome';
import './lib/katex';
import './component/sentry_monitor';
import './util/polyfills';

/*!
 * @license MIT https://github.com/jquery/jquery
 * Copyright JS Foundation and other contributors, https://js.foundation/
 */
function main(): void {
    const onLoad = () => {
        void (async () => {
            const { addFootnoteTooltip } = await import('./component/add_footnote_tooltip');
            await addFootnoteTooltip();
        })();
    };

    /**
     * ref: https://github.com/jquery/jquery/blob/3.4.0/src/core/ready.js#L60
     */
    const completed = () => {
        document.removeEventListener('DOMContentLoaded', completed);
        window.removeEventListener('load', completed);
        void onLoad();
    };

    if (
        document.readyState === 'complete'
        || (document.readyState !== 'loading' && !('doScroll' in document.documentElement))
    ) {
        window.setTimeout(onLoad);
    }
    else {
        document.addEventListener('DOMContentLoaded', completed);

        // A fallback to window.onload, that will always work
        window.addEventListener('load', completed);
    }
}

main();
