import Vue from 'vue';
import Handlebars from 'handlebars';

Handlebars.registerHelper('includeitems', function (item, options) {
  let delim = options.hash['delimiter'];
  if (delim === undefined) {
    delim = ' ';
  }

  let name = options.hash['name'];
  if (name === undefined) {
    name = 'condition';
  }

  const items = options.hash['items'].split(delim);

  const context = Object.assign({}, this);
  context[name] = items.includes(item);

  return options.fn(context, {
    data: options.data,
    blockParams: [context]
  });
});

const shareButtonsTemplate = Handlebars.compile(`
<div class="sharebuttons">
  <ul class="social">
  {{#each linkitems}}
    <li>
      {{#includeitems name name='isme' items='mastodon'}}
      <a {{#if isme}}rel="me"{{/if}} class="sc-{{name}}" href="{{link}}" title="{{title}}" target="_blank">
      {{/includeitems}}
      {{#includeitems name name='isfas' items='envelope rss code clipboard'}}
        <i class="{{#if isfas}}fas{{else}}fab{{/if}} fa-{{name}}">
      {{/includeitems}}
      </a>
    </li>
  {{/each}}
  </ul>
</div>
`, {
  noEscape: true,
});

export function addShareButtons(metadata) {
  const title = metadata.title;
  const url = metadata.url;
  const github_url = metadata.github_url;

  const componentShareButtons = new Vue({
    template: shareButtonsTemplate({
      linkitems: [
        {
          name: 'code',
          title: 'Lookup raw code to GitHub',
          link: github_url,
        },
        {
          name: 'twitter',
          title: 'Tweet share to Twitter',
          link: 'https://twitter.com/intent/tweet?text=' + title + '&tw_p=tweetbutton&url=' + url,
        },
        {
          name: 'facebook',
          title: 'Like share to Facebook',
          link: 'https://www.facebook.com/sharer/sharer.php?src=share_button&u=' + url,
        },
        {
          name: 'get-pocket',
          title: 'Save to Pocket',
          link: 'https://getpocket.com/edit?url=' + url,
        },
        /*{
          name: 'clipboard',
          title: 'Copy post summary to Clipboard for share',
          link: '#'
        }*/
      ]
    }),
  }).$mount();
  const content = document.querySelector("article.single div");
  content.parentNode.insertBefore(
    componentShareButtons.$el,
    content.nextSibling
  );
}
