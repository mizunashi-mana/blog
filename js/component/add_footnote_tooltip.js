import * as DeviceUtil from '../util/device';

import Tooltip from 'tooltip.js';

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
    const reference = e.getAttribute("href").substr(1);

    const tooltipContent = document.createElement('div');
    tooltipContent.setAttribute('class', 'footnote-tooltip-content');
    tooltipContent.innerHTML = footnoteTitles[reference];

    new Tooltip(document.getElementById(eid), {
      placement: "bottom",
      title: tooltipContent,
      html: true,
    });
  }
}
