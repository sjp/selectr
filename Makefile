PKG_VERSION = $(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME = $(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

INST_FILES := $(shell find inst -type f -print)
MAN_FILES := $(wildcard man/*.Rd)
R_FILES := $(wildcard R/*.R)
TEST_FILES := $(wildcard tests/*.R)
PKG_FILES := DESCRIPTION NAMESPACE $(TEST_FILES) $(R_FILES) $(MAN_FILES) $(INST_FILES)

.PHONY: build check install run win clean 

build: $(PKG_NAME)_$(PKG_VERSION).tar.gz

$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	R CMD build ./

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $<

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $<

run: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $<
	R

# Unsafe! Does not generalise! May work for simple packages though
win: $(PKG_NAME)_$(PKG_VERSION).zip

$(PKG_NAME)_$(PKG_VERSION).zip: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	mkdir tmp
	R CMD INSTALL -l tmp $<
	cd tmp ; zip --quiet -r $@ $(PKG_NAME) ; mv $@ ../
	-rm -rf tmp

clean:
	-rm $(PKG_NAME)*.tar.gz
	-rm -rf $(PKG_NAME).Rcheck
	-rm $(PKG_NAME)*.zip
