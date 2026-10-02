R = Rscript

ANALYTIC_RMDS = \
  01_measurement_checks.Rmd \
  02_descriptives_and_zero_order_correlations.Rmd \
  03_multilevel_models_hypothesis_tests.Rmd \
  04_dif_and_mg_cfa_measurement_bias.Rmd \
  05_sensitivity_analyses.Rmd

DOCUMENTATION_RMDS = \
  06_appendix_tables_and_figures.Rmd \
  07_reproducibility_report.Rmd

ANALYTIC_PDFS = $(addprefix ../outputs/,$(ANALYTIC_RMDS:.Rmd=.pdf))
DOCUMENTATION_PDFS = $(addprefix ../outputs/,$(DOCUMENTATION_RMDS:.Rmd=.pdf))
DATA_LINK = student_responses.RDS

.PHONY: all reports analytic documentation codebook list clean

# Creates a temporary data link and removes it, including after a failed render.
all:
	@set -e; trap 'rm -f $(DATA_LINK)' EXIT; \
	$(MAKE) --no-print-directory reports

reports: $(ANALYTIC_PDFS) $(DOCUMENTATION_PDFS)

analytic: $(ANALYTIC_PDFS)

documentation: $(DOCUMENTATION_PDFS)

codebook: ../data/codebook_source.tex
	cd ../data && latexmk -pdf -interaction=nonstopmode codebook_source.tex

$(DATA_LINK): ../data/student_responses.RDS
	ln -s ../data/student_responses.RDS $@

../outputs/%.pdf: %.Rmd
	$(R) -e "rmarkdown::render('$<', output_format='all', output_dir='../outputs')"

$(ANALYTIC_PDFS): $(DATA_LINK)

list:
	@echo "Analytic reports:" $(ANALYTIC_RMDS)
	@echo "Documentation reports:" $(DOCUMENTATION_RMDS)

clean:
	rm -f $(ANALYTIC_PDFS) $(DOCUMENTATION_PDFS) $(DATA_LINK)
