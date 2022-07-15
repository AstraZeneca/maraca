help:
	@echo "make (env|test|unittest|build)"

test:
	Rscript -e "devtools::document()"
	Rscript -e "devtools::check()"
	Rscript -e "devtools::test()"

unittest:
	Rscript -e "devtools::test()"

build:
	Rscript -e "devtools::document()"
	Rscript -e "devtools::build('.')"
