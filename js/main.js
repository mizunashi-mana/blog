import * as addMathJax from './component/mathjax';
import * as addShareButtons from './component/add_sharebuttons';

document.addEventListener('DOMContentLoaded', () => {
  const contentMetadata = global.contentMetadataForCustomJS;

  if (['articles'].includes(contentMetadata.type)) {
    addShareButtons.addShareButtons(contentMetadata);
  }

  if (['articles', 'pages'].includes(contentMetadata.type)) {
    addMathJax.loadMathJaxDocument();
  }
}, false);
