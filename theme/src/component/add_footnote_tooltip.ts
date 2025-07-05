import * as DeviceUtil from '@/util/device';

export async function addFootnoteTooltip(): Promise<void> {
    if (DeviceUtil.isTouchDevice()) {
        return;
    }

    await import('tippy.js/themes/light-border.css');
    const { default: Tippy } = await import('tippy.js');

    const footnoteTitles: Record<string, string | undefined> = {};
    document.querySelectorAll('table.docutils.footnote').forEach((e) => {
        const eid = e.getAttribute('id');
        if (eid !== null) {
            footnoteTitles[eid] = e.querySelector('td:not(.label)')?.innerHTML;
        }
    });

    document.querySelectorAll('a.footnote-reference').forEach((e) => {
        const reference = e.getAttribute('href')?.substring(1);

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
    });
}
