## All Rmarkdown files in the working directory

SRCDIR = source
OUTDIR = docs
DATADIR = $(OUTDIR)/data


VIGDIR = vignettes
DOCDIR = doc

VIGRMD = $(wildcard $(VIGDIR)/*.Rmd)
TMP1  = $(VIGRMD:.Rmd=.html)
VIGHTML = ${subst $(VIGDIR),$(DOCDIR),$(TMP1)}
VIGHTMLOUT = ${subst $(VIGDIR),$(OUTDIR),$(TMP1)}


RMD = $(wildcard $(SRCDIR)/*.Rmd)
TMP2  = $(RMD:.Rmd=.html)
HTML = ${subst $(SRCDIR),$(OUTDIR),$(TMP2)}


#############

all: clean update web

#############

vignettes: $(VIGHTML)

$(VIGHTML): $(VIGRMD)
	@Rscript -e "devtools::build_vignettes()"

clean_vignettes:
	@Rscript -e "devtools::clean_vignettes()"

#############

html:	$(HTML)
# %.html: %.Rmd
$(OUTDIR)/%.html: $(SRCDIR)/%.Rmd
	@Rscript -e "rmarkdown::render('$<', output_format = 'prettydoc::html_pretty', output_dir = './$(OUTDIR)/')"

clean_html:
	rm -f $(HTML)

#############

web: html vignettes
	cp -f $(VIGHTML) $(VIGHTMLOUT)
	mkdir -p $(DATADIR)
	cp -f ./inst/emeScheme.xsd.xml $(DATADIR)/
	cp -f ./inst/emeScheme_example.xml $(DATADIR)/

clean_web:
	rm -f VIGHTMLOUT
	rm -rf $(DATADIR)

#############

update:
	@Rscript -e "devtools::load_all(here::here()); emeScheme:::updateFromGoogleSheet(token = './source/googlesheets_token.rds')"

#############

publish:
	git add DESCRIPTION data/emeScheme.rda data/emeScheme_gd.rda inst/googlesheet/emeScheme.xlsx docs/index.html
	git commit -m "Update From Googlesheets"
	git push

#############

clean: clean_vignettes clean_html clean_web

#############

list_files:
	@echo SRCDIR  : $(SRCDIR)
	@echo OUTDIR  : $(OUTDIR)
	@echo DATADIR : $(DATADIR)
	@echo
	@echo VIGDIR  : $(VIGDIR)
	@echo DOCDIR  : $(DOCDIR)
	@echo
	@echo VIGRMD  : $(VIGRMD)
	@echo TMP1    : $(TMP1)
	@echo VIGHTML : $(VIGHTML)
	@echo VIGHTMLOUT : $(VIGHTMLOUT)
	@echo
	@echo RMD     : $(RMD)
	@echo TMP2    : $(TMP2)
	@echo HTML    : $(HTML)

## from https://stackoverflow.com/a/26339924/632423
list: list_files
	@echo
	@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$' | xargs

#############

.PHONY: list files update clean clean_vignettes clean_web clean_html publish
