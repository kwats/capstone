purl=Rscript -e "knitr::purl('$(1)', '$(2)', quiet=TRUE, documentation=0)"

rfiles:=$(patsubst vignettes/%.Rmd,R/%.R,$(wildcard vignettes/*.Rmd))

all: $(rfiles)

R/%.R: vignettes/%.Rmd
	$(call purl,$^,$@)