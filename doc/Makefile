##
# Documentation generation
#

.PHONY: doc

doc: project.pdf

project.tex: project.org
	emacs $< --batch -f org-babel-tangle -f org-latex-export-to-latex --kill

project.pdf: project.tex
	latexmk -pdf $<
	latexmk -c
