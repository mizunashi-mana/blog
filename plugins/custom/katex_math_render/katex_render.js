#!/usr/bin/env node

import KaTeX from 'katex';

function katexRenderingPipe(instream, outstream) {
    let chunkrest = '';

    function onData(chunk) {
        const chunks = chunk.toString('utf8').split('\n');
        chunks[0] = chunkrest + chunks[0];

        let l = chunks.length - 1
        for (let i = 0; i < l; i++) {
            const content = JSON.parse(chunks[i]);
            if (typeof content !== 'object' || content === null) {
                throw new Error(`invalid content: ${content}`);
            }

            let opts = {
                strict: (errorCode, errorMsg, _token) => {
                    console.error(`${errorCode}:${errorMsg}:${JSON.stringify(content)}`);
                },
            };
            if (content.m == 'b') {
                opts.displayMode = true;
            }

            outstream.write(JSON.stringify({
                'r': KaTeX.renderToString(content.t, opts),
            }));
            outstream.write("\n");
        }
        chunkrest = chunks[l];
    }

    instream.on("data", onData);
    instream.on("end", () => {
        outstream.end('');
    });
}

katexRenderingPipe(process.stdin, process.stdout);
