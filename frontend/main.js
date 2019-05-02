import './component/sentry_monitor';
import './component/render_math';
import * as addShareButtons from './component/add_sharebuttons';
import * as addFootnoteTooltip from './component/add_footnote_tooltip';


function onLoad() {
  const contentMetadata = global.contentMetadataForCustomJS;

  if (['articles'].includes(contentMetadata.type)) {
    addShareButtons.addShareButtons(contentMetadata);
  }

  if (['articles', 'pages'].includes(contentMetadata.type)) {
    addFootnoteTooltip.addFootnoteTooltip();
  }
}

// https://github.com/jquery/jquery/blob/3.4.0/src/core/ready.js#L60
function completed() {
  document.removeEventListener("DOMContentLoaded", completed);
  window.removeEventListener("load", completed);
  onLoad();
}

if (
  document.readyState === "complete"
  || document.readyState !== "loading" && !document.documentElement.doScroll
) {
  window.setTimeout(onLoad);
} else {
  document.addEventListener("DOMContentLoaded", completed);

  // A fallback to window.onload, that will always work
  window.addEventListener("load", completed);
}
