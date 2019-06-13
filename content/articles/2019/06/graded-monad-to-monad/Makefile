.SUFFIXES: .tex .pdf .png .eps .svg

.pdf.png:
	pdftopng -f 1 -l 1 $< $@
	@mv $@-000001.png $@

.tex.pdf:
	pdflatex $<
	rm $(subst .tex,.aux,$<) $(subst .tex,.log,$<)
	pdfcrop $@ $@
