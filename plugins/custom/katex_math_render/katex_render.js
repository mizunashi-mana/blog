#!/usr/bin/env node

const KaTeX = require("katex");

function katexRenderingPipe(instream, outstream) {
  let chunkrest = "";

  function onData(chunk) {
    const chunks = chunk.toString("utf8").split("\t\n");
    chunks[0] = chunkrest + chunks[0];

    let l = chunks.length - 1
    for (i = 0; i < l; i++) {
      const str = chunks[i].replace(/\t /g, "\t");
      if (typeof str !== "string") {
        throw new Error("not string stream");
      }

      outstream.write(KaTeX.renderToString(str).replace(/\t/g, "\t "));
      outstream.write("\t\n");
    }
    chunkrest = chunks[l];
  }

  instream.on("data", onData);
  instream.on("end", () => {
    outstream.end('');
  });
}

katexRenderingPipe(process.stdin, process.stdout);
