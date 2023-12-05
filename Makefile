KODIAK_PATH ?= $(CURDIR)/Kodiak
KODIAK_LIBRARY_DIR = $(BUILD_FOLDER)/kodiak/
PRECISA_PATH ?= $(CURDIR)/PRECiSA
JOBS ?= 4
BUILD_FOLDER ?= $(CURDIR)/build
CABAL ?= cabal
FILIBPP_URL ?= http://www2.math.uni-wuppertal.de/wrswt/software/filib++/filibsrc-3.0.2.tar.gz

ifneq (, $(shell which wget))
	HTTPS_GET ?= wget
else ifneq (, $(shell which curl))
	HTTPS_GET ?= curl -O
else
 $(error "No lzop in $(PATH), consider doing apt-get install lzop")
endif

all: build-precisa
	@echo "all"

checkout-submodules:
	git submodule update --init --recursive

build-precisa: configure-precisa
	@( \
		cd $(PRECISA_PATH); \
		$(CABAL) v2-install --overwrite-policy=always; \
	)

configure-precisa: build-kodiak
	@( \
		cd $(PRECISA_PATH); \
		echo "package precisa" > cabal.project.local; \
		echo "  extra-lib-dirs: $(KODIAK_LIBRARY_DIR)" >> cabal.project.local; \
		echo "  ghc-options: -optl=-Wl,-rpath,$(KODIAK_LIBRARY_DIR)" >> cabal.project.local; \
	)

build-kodiak: checkout-submodules build-filib
	@( \
		cd $(BUILD_FOLDER); \
		mkdir kodiak; \
		cd kodiak; \
		FILIB_ROOT=$(BUILD_FOLDER)/filibsrc cmake $(KODIAK_PATH); \
		cmake --build . -- -j$(JOBS); \
	)

print-kodiak-library-dir: build-kodiak
	@echo $(KODIAK_LIBRARY_DIR)

.PHONY=build-filib
build-filib: build/filibsrc/libprim/.libs/libprim.a

build/filibsrc/libprim/.libs/libprim.a: build
	@( \
	  cd $(BUILD_FOLDER); \
		$(HTTPS_GET) http://www2.math.uni-wuppertal.de/wrswt/software/filib++/filibsrc-3.0.2.tar.gz; \
		tar xvfz filibsrc-3.0.2.tar.gz; \
		cd filibsrc; \
		./configure CFLAGS=-fPIC CPPFLAGS=-fPIC CXXFLAGS="-fPIC -std=c++03"; \
		make -j$(JOBS); \
		make install; \
	)

build:
	mkdir $(BUILD_FOLDER)