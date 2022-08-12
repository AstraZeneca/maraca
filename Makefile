.PHONY: help test unittest build document namespace vignettes
help:
	@echo "make (env|test|unittest|build)"

test:
	Rscript -e "devtools::document()"
	Rscript -e "devtools::check()"
	Rscript -e "devtools::test()"

unittest:
	Rscript -e "devtools::test()"

document:
	Rscript -e "devtools::document()"

build: clean document namespace
	-mkdir dist
	Rscript -e "devtools::build('.', path='dist/')"

namespace:
	rm NAMESPACE
	Rscript -e "devtools::document('.', roclets=c('namespace'))"

vignettes:
	Rscript -e "devtools::build_vignettes(keep_md=FALSE)"

check: build
	R CMD check --as-cran dist/maraca_*.tar.gz

clean:
	-rm -rf dist
