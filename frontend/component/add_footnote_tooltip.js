import * as DeviceUtil from '../util/device';

import Tippy from 'tippy.js';
import 'tippy.js/themes/light-border.css';

export function addFootnoteTooltip() {
  // disable tooltips on touch-enabled device
  if (DeviceUtil.isTouchDevice()) {
    return;
  }

  let footnoteTitles = {};
  for (const e of document.querySelectorAll("table.docutils.footnote")) {
    const eid = e.getAttribute("id");
    footnoteTitles[eid] = e.querySelector("td:not(.label)").innerHTML;
  }

  for (const e of document.querySelectorAll("a.footnote-reference")) {
    const eid = e.getAttribute("id");
    const reference = e.getAttribute("href").substring(1);

    Tippy(document.getElementById(eid), {
      placement: 'bottom',
      content: footnoteTitles[reference],
      allowHTML: true,
      maxWidth: '80vw',
      theme: 'light-border',
      arrow: false,
    });
  }
}
