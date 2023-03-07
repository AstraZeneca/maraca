PACKAGE_NAME = $(shell grep Package: DESCRIPTION | awk '{print $$2}')
PACKAGE_VERSION = $(shell grep Version: DESCRIPTION | awk '{print $$2}')

.PHONY: help test unittest build document namespace vignettes
help:
	@echo "make (env|test|unittest|build)"

test:
	Rscript -e "devtools::document()"
	Rscript -e "devtools::check()"
	Rscript -e "devtools::test()"

unittest:
	Rscript -e "devtools::test(reporter=c('summary', 'fail'))"

document:
	Rscript -e "devtools::document()"

build: clean document namespace
	-mkdir dist
	@echo "** Building ${PACKAGE_NAME} ${PACKAGE_VERSION}"

	Rscript -e "devtools::build('.', path='dist/')"
	# Ensure that we don't leave our username in the description file
	TMPDIR=`mktemp -d` && \
		cp dist/${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz $$TMPDIR && \
		pushd $$TMPDIR && \
		tar xzf ${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz && \
		cd ${PACKAGE_NAME} && \
		cat DESCRIPTION | grep Packaged && \
		cp DESCRIPTION DESCRIPTION.old && \
		cat DESCRIPTION.old | sed  's/\(Packaged: [^;]*\);\(.*\)/\1; hidden/g' >DESCRIPTION && \
		rm -f DESCRIPTION.old && \
		cat DESCRIPTION | grep Packaged && \
		cd .. && \
		rm ${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz && \
		tar czf ${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz ${PACKAGE_NAME} && \
		popd && \
		mv $$TMPDIR/${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz dist/${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz

namespace:
	rm NAMESPACE
	Rscript -e "devtools::document('.', roclets=c('namespace'))"

vignettes:
	Rscript -e "devtools::build_vignettes(keep_md=FALSE)"

check: build
	R CMD check --as-cran dist/${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz

validate:
	Rscript -e "junit <- testthat::JunitReporter\$$new(file=\"../artifacts/junit-out.xml\"); devtools::test(reporter=junit)"
	sed -I '' 's/classname="[a-zA-Z]*"/classname="az.r.package.maraca"/' tests/artifacts/junit-out.xml

clean:
	-rm -rf dist
