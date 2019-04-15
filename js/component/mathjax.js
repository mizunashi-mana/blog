import { MathJax } from 'mathjax3';
import { MathML } from 'mathjax3/input/mathml';
import { CHTML } from 'mathjax3/output/chtml';
import { browserAdaptor } from 'mathjax3/adaptors/browserAdaptor';
import { RegisterHTMLHandler } from 'mathjax3/handlers/html';

export function loadMathJaxDocument() {
  RegisterHTMLHandler(browserAdaptor());

  const MathJaxDocument = MathJax.document(document, {
    InputJax: new MathML(),
    OutputJax: new CHTML({
      fontURL: 'https://cdn.rawgit.com/mathjax/mathjax-v3/3.0.0-beta.3/mathjax2/css',
    }),
  });

  return MathJaxDocument
    .findMath()
    .compile()
    .getMetrics()
    .typeset()
    .updateDocument()
    ;
}
