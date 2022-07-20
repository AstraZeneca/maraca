.PHONY: help test unittest build document namespace
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

build: document namespace
	Rscript -e "devtools::build('.')"

namespace: 
	rm NAMESPACE
	Rscript -e "devtools::document('.', roclets=c('namespace'))"
