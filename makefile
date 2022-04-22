# Make a differenced document 
diff: docs/diff.pdf

docs/diff.pdf: docs/diff.tex
	pdflatex $< -job-name=$@

docs/diff.tex: docs/alameda_destinationchoice.tex
	latexdiff docs/alameda_destinationchoice_submitted.tex $< > $@
	
figs: 
	mkdir -p alameda_destinationchoice_files/
	cp -r _bookdown_files/alameda_destinationchoice_files/ alameda_destinationchoice_files/
	
doc:
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book", output_yaml = "_output.yml")'
	
site: 
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'
