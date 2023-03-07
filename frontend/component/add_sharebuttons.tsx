import * as React from "react";
import * as ReactDOM from "react-dom/client";

export type ShareButtonsProps = {
    items: {
        name: string;
        href: string;
        title: string;
        rel?: string;
        faClassName: string;
    }[];
};

export const shareButtons: React.FC<ShareButtonsProps> = (props: ShareButtonsProps) => {
    return (
        <ul className="social">
            {props.items.map(item => (
                <li key={item.name}>
                    <a
                        rel={item.rel}
                        className={`sc-${item.name}`}
                        href={item.href}
                        title={item.title}
                        target="_blank"
                    >
                        <i className={item.faClassName}></i>
                    </a>
                </li>
            ))}
        </ul>
    );
};

export function addShareButtons(metadata: {
    title: string;
    url: string;
    github_url: string;
}) {
    const props: ShareButtonsProps = {
        items: [
            {
                name: 'code',
                title: 'Lookup the raw code of this post on GitHub',
                href: metadata.github_url,
                faClassName: "fas fa-code",
            },
            {
                name: 'mastodon',
                title: 'Share to Mastodon',
                href: `https://mizunashi-mana.github.io/mastodon-front-gateway/share/?text=${metadata.title}&url=${metadata.url}`,
                faClassName: "fab fa-mastodon",
            },
            {
                name: 'twitter',
                title: 'Tweet share to Twitter',
                href: `https://twitter.com/intent/tweet?text=${metadata.title}&tw_p=tweetbutton&url=${metadata.url}`,
                faClassName: "fab fa-twitter",
            },
            {
                name: 'facebook',
                title: 'Like share to Facebook',
                href: `https://www.facebook.com/sharer/sharer.php?src=share_button&u=${metadata.url}`,
                faClassName: "fab fa-facebook"
            },
            {
                name: 'get-pocket',
                title: 'Save to Pocket',
                href: `https://getpocket.com/edit?url=${metadata.url}`,
                faClassName: "fab fa-get-pocket"
            },
        ]
    };

    const targetElem = document.createElement("div");
    targetElem.className = "sharebuttons";

    const reactRoot = ReactDOM.createRoot(targetElem);
    reactRoot.render(shareButtons(props));

    const content = document.querySelector("article.single div");
    if (content !== null) {
        content.parentNode?.insertBefore(
            targetElem,
            content.nextSibling
        );
    }
}
