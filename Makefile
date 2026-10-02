R = Rscript

ANALYTIC_RMDS = \
  01_measurement-checks.Rmd \
  02_descriptives-and-zero-order.Rmd \
  03_multilevel-models-hypothesis-tests.Rmd \
  04_dif-and-mg-cfa-hpt-bias.Rmd \
  05_sensitivity-analyses.Rmd

DOCUMENTATION_RMDS = \
  06_appendix-tables-and-figures.Rmd \
  07_reproducibility-report.Rmd

ANALYTIC_PDFS = $(addprefix outputs/,$(ANALYTIC_RMDS:.Rmd=.pdf))
DOCUMENTATION_PDFS = $(addprefix outputs/,$(DOCUMENTATION_RMDS:.Rmd=.pdf))

.PHONY: all analytic documentation codebook supplement list clean

all: analytic documentation

analytic: $(ANALYTIC_PDFS)

documentation: $(DOCUMENTATION_PDFS)

codebook: normalised_responses_codebook.tex

	latexmk -pdf -interaction=nonstopmode normalised_responses_codebook.tex

supplement: submissions/pci_psychology/supplementary_materials.md
	pandoc $< --pdf-engine=xelatex -o submissions/pci_psychology/supplementary_materials.pdf

teacher:
	# Convert TEACHER_NAME to a safe filename (spaces -> underscores)
	@SAFE_NAME=$$(echo "$(TEACHER_NAME)" | tr ' ' '_'); \
	OUTPUT="report_$${SAFE_NAME}_$(SCHOOL_ID)_CONFIDENTIAL.pdf"; \
	$(R) -e "rmarkdown::render('teacher_report.Rmd', \
		output_format = 'pdf_document', \
		output_dir = 'outputs', \
		output_file = '$${OUTPUT}', \
		params = list( \
			school_id = '$(SCHOOL_ID)', \
			teacher_name = '$(TEACHER_NAME)' \
		) \
	)"

outputs/%.pdf: %.Rmd
	$(R) -e "rmarkdown::render('$<', output_format='all', output_dir='outputs')"

$(ANALYTIC_PDFS): normalised_responses.RData

list:
	@echo "Analytic reports:" $(ANALYTIC_RMDS)
	@echo "Documentation reports:" $(DOCUMENTATION_RMDS)

clean:
	rm -f $(ANALYTIC_PDFS) $(DOCUMENTATION_PDFS)
