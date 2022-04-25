# Make a differenced document 
diff: docs/diff.pdf

docs/diff.pdf: docs/diff.tex figs
	cp book.bib docs/book.bib
	pdflatex -output-directory=docs/ -job-name=$@ $<
	bibtex docs/diff
	pdflatex -output-directory=docs/ -job-name=$@ $<
	

docs/diff.tex: docs/alameda_destinationchoice.tex
	latexdiff docs/alameda_destinationchoice_submitted.tex $< > $@
	
figs: 
	mkdir -p alameda_destinationchoice_files/
	cp -r _bookdown_files/alameda_destinationchoice_files/ alameda_destinationchoice_files/
	
doc:
	Rscript -e '\
	Sys.setenv(RSTUDIO_PANDOC="Applications/RStudio.app/Contents/MacOS/quarto/bin");\
	bookdown::render_book("index.Rmd", "bookdown::pdf_book", output_yaml = "_output.yml")'
	
site: 
	Rscript -e '\
	Sys.setenv(RSTUDIO_PANDOC="Applications/RStudio.app/Contents/MacOS/quarto/bin");\
	bookdown::render_book("index.Rmd", "bookdown::gitbook")'
	
	
response: response2/response.pdf
	
response2/response.pdf: response2/response.tex
	cp book.bib response2/book.bib
	pdflatex -output-directory=response2/ -job-name=$@ $<
	bibtex response2/response
	pdflatex -output-directory=response2/ -job-name=$@ $<
	pdflatex -output-directory=response2/ -job-name=$@ $<
	