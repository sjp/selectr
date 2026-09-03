PKG_VERSION = $(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME = $(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

INST_FILES := $(shell find inst -type f -print)
MAN_FILES := $(wildcard man/*.Rd)
R_FILES := $(wildcard R/*.R)
TEST_FILES := $(shell find tests -name '*.R')
PKG_FILES := DESCRIPTION NAMESPACE $(TEST_FILES) $(R_FILES) $(MAN_FILES) $(INST_FILES)

.PHONY: build check test lint install run clean

build: $(PKG_NAME)_$(PKG_VERSION).tar.gz

$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	R CMD build ./

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check --as-cran $<

test:
	Rscript -e 'testthat::test_local()'

lint:
	Rscript -e 'lintr::lint_package()'

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $<

run: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $<
	R

clean:
	-rm $(PKG_NAME)*.tar.gz
	-rm -rf $(PKG_NAME).Rcheck
