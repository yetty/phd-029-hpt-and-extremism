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
GENERATED_REPORT_ARTIFACTS = \
  $(ANALYTIC_PDFS:.pdf=.md) \
  $(ANALYTIC_PDFS:.pdf=.tex) \
  $(DOCUMENTATION_PDFS:.pdf=.md) \
  $(DOCUMENTATION_PDFS:.pdf=.tex) \
  $(ANALYTIC_PDFS:.pdf=_files) \
  $(DOCUMENTATION_PDFS:.pdf=_files)
DATA_LINK = student_responses.RDS

.PHONY: all reports analytic documentation codebook list clean remove-data-link

all: reports

reports: analytic documentation

# The trap removes only a link created by this invocation. Existing files or
# user-created links are never removed by an analytic render.
analytic:
	@set -e; created=0; \
	if [ ! -e "$(DATA_LINK)" ] && [ ! -L "$(DATA_LINK)" ]; then \
		ln -s ../data/student_responses.RDS "$(DATA_LINK)"; created=1; \
	fi; \
	trap 'if [ "$$created" -eq 1 ]; then rm -f "$(DATA_LINK)"; fi' EXIT; \
	$(MAKE) --no-print-directory $(ANALYTIC_PDFS); \
	rm -f ../outputs/*.log

documentation: $(DOCUMENTATION_PDFS)
	@rm -f ../outputs/*.log
	@$(MAKE) --no-print-directory remove-data-link

codebook: ../data/codebook_source.tex
	cd ../data && latexmk -pdf -interaction=nonstopmode codebook_source.tex

$(DATA_LINK): ../data/student_responses.RDS
	ln -s ../data/student_responses.RDS $@

../outputs/%.pdf: %.Rmd
	$(R) -e "rmarkdown::render('$<', output_format='pdf_document', output_dir='../outputs')"

$(ANALYTIC_PDFS): $(DATA_LINK)

list:
	@echo "Analytic reports:" $(ANALYTIC_RMDS)
	@echo "Documentation reports:" $(DOCUMENTATION_RMDS)

clean:
	rm -f $(ANALYTIC_PDFS) $(DOCUMENTATION_PDFS)
	rm -f $(filter %.md %.tex %.log,$(GENERATED_REPORT_ARTIFACTS))
	rm -rf $(filter %_files,$(GENERATED_REPORT_ARTIFACTS))
	@$(MAKE) --no-print-directory remove-data-link

remove-data-link:
	@if [ -L "$(DATA_LINK)" ] && \
		[ "$$(readlink "$(DATA_LINK)")" = "../data/student_responses.RDS" ]; then \
		rm -f "$(DATA_LINK)"; \
	elif [ -e "$(DATA_LINK)" ] || [ -L "$(DATA_LINK)" ]; then \
		echo "Preserving existing $(DATA_LINK)."; \
	fi
