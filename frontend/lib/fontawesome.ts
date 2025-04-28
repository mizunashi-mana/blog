async function loadFontAwesome(): Promise<void> {
    const { library, dom } = await import('@fortawesome/fontawesome-svg-core');
    const { fas } = await import('@fortawesome/free-solid-svg-icons');
    const { far } = await import('@fortawesome/free-regular-svg-icons');
    const { fab } = await import('@fortawesome/free-brands-svg-icons');
    library.add(fas, far, fab);
    await dom.i2svg();
}

void loadFontAwesome();
