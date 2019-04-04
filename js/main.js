import * as addShareButtons from './add_sharebuttons';

document.addEventListener('DOMContentLoaded', () => {
  const contentMetadata = global.contentMetadataForCustomJS;

  if (contentMetadata.type == 'articles') {
    addShareButtons.addShareButtons(contentMetadata);
  }
}, false);
