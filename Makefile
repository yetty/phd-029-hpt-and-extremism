R = Rscript

# List your Rmd reports here
RMDS = \
  01_measurement-checks.Rmd \
  02_descriptives-and-zero-order.Rmd \
  03_multilevel-models-hypothesis-tests.Rmd \
  04_dif-and-mg-cfa-hpt-bias.Rmd \
  05_sensitivity-analyses.Rmd

PDFS = $(RMDS:.Rmd=.pdf)

.PHONY: all clean list

all: $(PDFS)

# Generic rule: any .Rmd -> .pdf
%.pdf: %.Rmd
	$(R) -e "rmarkdown::render('$<', output_format='all', output_dir='outputs')"

# File-specific data dependency (this report loads normalised_responses.RData)
01_measurement-checks.pdf: normalised_responses.RData
02_descriptives-and-zero-order.pdf: normalised_responses.RData
03_multilevel-models-hypothesis-tests.pdf: normalised_responses.RData
04_dif-and-mg-cfa-hpt-bias.pdf: normalised_responses.RData
05_sensitivity-analyses.pdf: normalised_responses.RData

list:
	@echo "Rmd files:" $(RMDS)