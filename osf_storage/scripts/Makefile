R = Rscript

# List your Rmd reports here
RMDS = \
  01_measurement_checks.Rmd \
  02_descriptives_and_zero_order_correlations.Rmd \
  03_multilevel_models_hypothesis_tests.Rmd \
  04_dif_and_mg_cfa_measurement_bias.Rmd \
  05_sensitivity_analyses.Rmd

PDFS = $(RMDS:.Rmd=.pdf)

.PHONY: all clean list

codebook: codebook_source.tex
	latexmk -pdf -interaction=nonstopmode codebook_source.tex

all: $(PDFS)

# Generic rule: any .Rmd -> .pdf
%.pdf: %.Rmd
	$(R) -e "rmarkdown::render('$<', output_format='all', output_dir='../outputs')"

# File-specific data dependency (these reports read student_responses.RDS)
01_measurement_checks.pdf: student_responses.RDS
02_descriptives_and_zero_order_correlations.pdf: student_responses.RDS
03_multilevel_models_hypothesis_tests.pdf: student_responses.RDS
04_dif_and_mg_cfa_measurement_bias.pdf: student_responses.RDS
05_sensitivity_analyses.pdf: student_responses.RDS

list:
	@echo "Rmd files:" $(RMDS)
