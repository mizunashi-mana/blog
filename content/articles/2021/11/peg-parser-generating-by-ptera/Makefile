.SUFFIXES: .tex .pdf .png .eps .svg .hs .html

.pdf.png:
	pdftoppm -png -f 1 -l 1 $< $@
	@mv $@-*.png $@

.tex.pdf:
	pdflatex -shell-escape $<
	rm $(subst .tex,.aux,$<) $(subst .tex,.log,$<)
	pdfcrop $@ $@

.hs.html:
	pygmentize -f html -l haskell -O "full,linenos,lineanchors=line,anchorlinenos,title=$<" -o $@ $<
