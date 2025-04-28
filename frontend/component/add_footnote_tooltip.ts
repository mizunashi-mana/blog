import * as DeviceUtil from '../util/device';

import Tippy from 'tippy.js';
import 'tippy.js/themes/light-border.css';

export function addFootnoteTooltip(): void {
    // disable tooltips on touch-enabled device
    if (DeviceUtil.isTouchDevice()) {
        return;
    }

    let footnoteTitles: {[key: string]: string | undefined;} = {};
    document.querySelectorAll("table.docutils.footnote").forEach(e => {
        const eid = e.getAttribute("id");
        if (eid !== null) {
            footnoteTitles[eid] = e.querySelector("td:not(.label)")?.innerHTML;
        }
    })

    document.querySelectorAll("a.footnote-reference").forEach(e => {
        const reference = e.getAttribute("href")?.substring(1);

        if (reference !== undefined) {
            Tippy(e, {
                placement: 'bottom',
                content: footnoteTitles[reference],
                allowHTML: true,
                maxWidth: '80vw',
                theme: 'light-border',
                arrow: false,
            });
        }
    })
}
