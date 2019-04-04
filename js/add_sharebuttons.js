import Vue from 'vue';

export function addShareButtons(metadata) {
  const title = metadata.title;
  const url = metadata.url;
  const github_url = metadata.github_url;

  const componentShareButtons = new Vue({
    template: `
      <div class="sharebuttons">
        <ul class="social">
          <li v-for="item in items">
            <a
              v-bind:rel="['mastodon'].includes(item.name) ? 'me' : ''"
              v-bind:class="'sc-' + item.name"
              v-bind:href="item.link"
              v-bind:title="item.title"
              target="_blank"
              >
              <i v-bind:class="[
                ['envelope', 'rss', 'code', 'clipboard'].includes(item.name) ? 'fas' : 'fab',
                'fa-' + item.name
              ]">
            </a>
          </li>
        </ul>
      </div>
      `,
    data: {
      items: [
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
    },
  }).$mount();
  const content = document.querySelector("article.single div");
  content.parentNode.insertBefore(
    componentShareButtons.$el,
    content.nextSibling
  );
}
