.SUFFIXES: .tex .pdf .png .eps .svg

.pdf.png:
	pdftoppm -png -f 1 -l 1 $< $@
	@mv $@-*.png $@

.tex.pdf:
	pdflatex -shell-escape $<
	rm $(subst .tex,.aux,$<) $(subst .tex,.log,$<)
	pdfcrop $@ $@
