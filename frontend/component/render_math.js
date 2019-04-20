import KaTeX from 'katex';
import 'katex/dist/katex.min.css';

export function renderMathBlock() {
  for (const e of document.querySelectorAll("article.single pre.math")) {
    try {
      const ne = document.createElement('div');
      ne.setAttribute('class', 'math block');

      const mathText = e.textContent;
      e.parentNode.replaceChild(ne, e);
      KaTeX.render(mathText, ne);
    } catch (e) {
      console.error(e);
    }
  }

  for (const e of document.querySelectorAll("article.single tt.math")) {
    try {
      const ne = document.createElement('span');
      ne.setAttribute('class', 'math inline');

      const mathText = e.textContent;
      e.parentNode.replaceChild(ne, e);

      KaTeX.render(mathText, ne);
    } catch (e) {
      console.error(e);
    }
  }
}
