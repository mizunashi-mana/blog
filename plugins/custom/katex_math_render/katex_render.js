#!/usr/bin/env node

import KaTeX from 'katex';

function katexRenderingPipe(instream, outstream) {
    let chunkrest = '';

    function onData(chunk) {
        const chunks = chunk.toString('utf8').split('\n');
        chunks[0] = chunkrest + chunks[0];

        const l = chunks.length - 1;
        for (let i = 0; i < l; i += 1) {
            const content = JSON.parse(chunks[i]);
            if (typeof content !== 'object' || content === null) {
                throw new Error(`invalid content: ${content}`);
            }

            const opts = {
                strict: (errorCode, errorMsg) => {
                    process.stderr.write(`${errorCode}:${errorMsg}:${JSON.stringify(content)}\n`);
                },
            };
            if (content.m === 'b') {
                opts.displayMode = true;
            }

            outstream.write(JSON.stringify({
                r: KaTeX.renderToString(content.t, opts),
            }));
            outstream.write('\n');
        }
        chunkrest = chunks[l];
    }

    instream.on('data', onData);
    instream.on('end', () => {
        outstream.end('');
    });
}

katexRenderingPipe(process.stdin, process.stdout);
