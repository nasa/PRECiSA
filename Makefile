KODIAK_PATH ?= $(CURDIR)/Kodiak
KODIAK_LIBRARY_DIR = $(BUILD_FOLDER)/kodiak/
PRECISA_PATH ?= $(CURDIR)/PRECiSA
JOBS ?= 4
BUILD_FOLDER ?= $(CURDIR)/build
CABAL ?= cabal
HASKELL_COMPILER ?= ghc
FILIBPP_URL ?= http://www2.math.uni-wuppertal.de/wrswt/software/filib++/filibsrc-3.0.2.tar.gz
CMD_EXISTS = $(shell command -v $(1) >/dev/null 2>&1 && $$(echo "echo") "yes" || echo "no")
VERSION_MATCH = $(shell test "$$(printf '%s\n' "$(1)" "$(2)" | sort -V | head -n1)" = "$(1)" && echo "yes" || echo "no")

ifeq (yes, $(call CMD_EXISTS,wget))
  HTTPS_GET := wget
else ifeq (yes, $(call CMD_EXISTS,curl))
  HTTPS_GET := curl -O
else
  $(error "No wget nor curl in $(PATH), consider installing any of them")
endif

ifneq (yes, $(call CMD_EXISTS,$(CABAL)))
  $(error "No cabal present")
endif

ifneq (yes, $(call CMD_EXISTS,$(HASKELL_COMPILER)))
  $(error "No Haskell compiler present")
endif

CABAL_REQUIRED_VERSION := 3.6.2
CABAL_VERSION := $(shell $(CABAL) --numeric-version | head -n1 | cut -d" " -f4)
ifneq (yes, $(call VERSION_MATCH,$(CABAL_REQUIRED_VERSION),$(CABAL_VERSION)))
  $(error "Cabal version $(CABAL_VERSION) < $(CABAL_REQUIRED_VERSION) is not supported")
endif

HASKELL_REQUIRED_VERSION := 8.10.7
HASKELL_VERSION := $(shell $(HASKELL_COMPILER) --numeric-version | head -n1 | cut -d" " -f4)
ifneq (yes, $(call VERSION_MATCH,$(HASKELL_REQUIRED_VERSION),$(HASKELL_VERSION)))
  $(error "Haskell version $(HASKELL_VERSION) < $(HASKELL_REQUIRED_VERSION) is not supported")
endif

all: build-precisa
	@echo "all"

clean:
	@rm -f PRECiSA/cabal.project.local
	@rm -fR PRECiSA/dist-newstyle/
	@rm -fR build/

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
	)

build:
	mkdir $(BUILD_FOLDER)
