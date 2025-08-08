SHELL = /bin/bash
NCPUS ?= 1

BASE := logger utils db settings visualization qaqc remote workflow

MODELS := basgra biocro clm45 dalec dvmdostem ed fates gday jules linkages \
				ldndc lpjguess maat maespa sibcasa sipnet stics template

MODULES := allometry assim.batch assim.sequential benchmark \
				 data.atmosphere data.land data.remote \
				 emulator meta.analysis \
				 photosynthesis priors rtm uncertainty

# Components not currently included in the build
# (Most need more development first)
# If you need one of these on your system, add it to the appropriate line above.
# MODELS: cable preles
# MODULES: data.mining DART

SHINY := $(dir $(wildcard shiny/*/.))
SHINY := $(SHINY:%/=%)

BASE := $(BASE:%=base/%)
MODELS := $(MODELS:%=models/%)
MODULES := $(MODULES:%=modules/%)
ALL_PKGS := $(BASE) $(MODULES) $(MODELS)

BASE_I := $(BASE:%=.install/%)
MODELS_I := $(MODELS:%=.install/%)
MODULES_I := $(MODULES:%=.install/%)
ALL_PKGS_I := $(BASE_I) $(MODULES_I) $(MODELS_I)
SHINY_I := $(SHINY:shiny/%=.shiny_depends/%)

BASE_C := $(BASE:%=.check/%)
MODELS_C := $(MODELS:%=.check/%)
MODULES_C := $(MODULES:%=.check/%)
ALL_PKGS_C := $(BASE_C) $(MODULES_C) $(MODELS_C)

BASE_T := $(BASE:%=.test/%)
MODELS_T := $(MODELS:%=.test/%)
MODULES_T := $(MODULES:%=.test/%)
ALL_PKGS_T := $(BASE_T) $(MODULES_T) $(MODELS_T)

BASE_D := $(BASE:%=.doc/%)
MODELS_D := $(MODELS:%=.doc/%)
MODULES_D := $(MODULES:%=.doc/%)
ALL_PKGS_D := $(BASE_D) $(MODULES_D) $(MODELS_D)

SRCS_TO_CLEAN := $(strip $(foreach d,$(ALL_PKGS),$(wildcard ${d}/src)))

SETROPTIONS := "options(Ncpus = ${NCPUS})"

EXPECTED_ROXYGEN_VERSION := 7.3.2
INSTALLED_ROXYGEN_VERSION := $(shell Rscript \
	-e "if (requireNamespace('roxygen2', quietly = TRUE)) {" \
	-e   "cat(as.character(packageVersion('roxygen2')))" \
	-e "}")

### Macros

# Generates a list of all files and subdirectories at any depth inside its argument
recurse_dir = $(foreach d, $(wildcard $1*), $(call recurse_dir, $d/) $d)

# Filters a list from recurse_dir to remove paths that are directories
# Caveat: Really only removes *direct parents* of other paths *in the list*:
# 	$(call drop_dirs,a a/b a/b/c) => 'a/b/c',
# 	but $(call drop_dirs,a a/b d d/e/f) => 'a/b d d/e/f'
# For output from recurse_dir this removes all dirs, but in other cases beware.
drop_parents = $(filter-out $(patsubst %/,%,$(dir $1)), $1)

# Generates a list of regular files at any depth inside its argument
files_in_dir = $(call drop_parents, $(call recurse_dir, $1))

# Git hash + clean status for this directory
git_rev = $(shell \
	CLEAN=$$([[ -n $$(git status -s $1) ]] && echo "+mod"); \
	echo $$(git rev-parse --short=10 HEAD)"$$CLEAN")

# HACK: NA vs TRUE switch on dependencies argument is an ugly workaround for
# a circular dependency between benchmark and data.land.
# When this is fixed, can go back to simple `dependencies = TRUE`
depends_R_pkg = ./scripts/time.sh "depends ${1}" ./scripts/confirm_deps.R ${1} \
	$(if $(findstring modules/benchmark,$(1)),NA,TRUE)
install_R_pkg = ./scripts/time.sh "install ${1}" Rscript \
	-e ${SETROPTIONS} \
	-e "Sys.setenv(PECAN_GIT_REV='$(call git_rev,$1)')" \
	-e "remotes::install_local('$(strip $(1))', force=TRUE, dependencies=FALSE, upgrade=FALSE)"
check_R_pkg = ./scripts/time.sh "check ${1}" Rscript scripts/check_with_errors.R $(strip $(1))
test_R_pkg = ./scripts/time.sh "test ${1}" Rscript \
	-e "devtools::test('$(strip $(1))'," \
	-e "stop_on_failure = TRUE," \
	-e "stop_on_warning = FALSE)" # TODO: Raise bar to stop_on_warning = TRUE when we can

doc_R_pkg = \
	$(if \
		$(filter ${EXPECTED_ROXYGEN_VERSION},${INSTALLED_ROXYGEN_VERSION}), \
		./scripts/time.sh "document ${1}" \
			Rscript -e "devtools::document('"$(strip $(1))"')", \
		$(error Roxygen2 version is ${INSTALLED_ROXYGEN_VERSION}, \
			but PEcAn package documentation must be built with exactly \
			version ${EXPECTED_ROXYGEN_VERSION}))


depends = .doc/$(1) .install/$(1) .check/$(1) .test/$(1)

### Rules

.PHONY: all install check test document clean shiny pkgdocs \
            check_base check_models check_modules help

all: install document

# Note: Installs base first as Modules has a circular dependency on base
check_base: $(BASE_C)
check_models: $(MODELS_C)
check_modules: $(BASE_I) $(MODULES_C)

pkgdocs:
	Rscript scripts/build_pkgdown.R $(ALL_PKGS) base/all || exit 1
	

# Allow optional package arguments with user-friendly aliases:
# - `make install modules/foo` builds `.install/modules/foo`
# - `make check models/bar` builds `.check/models/bar`
# - `make test modules/baz` builds `.test/modules/baz`
# - `make document modules/qux` builds `.doc/modules/qux`
# Also accept bare package names (e.g., `make document ed`, `make test data.land`).
# If no package is specified, fall back to the original "all packages" behavior.

# Resolve user-supplied package args (either full paths or bare names) to full paths.
VERB_TARGETS := all install check test document clean shiny pkgdocs check_base check_models check_modules book help
USER_ARGS := $(filter-out $(VERB_TARGETS),$(MAKECMDGOALS))
PKG_NAMES := $(notdir $(ALL_PKGS))
resolve_pkg = $(if $(filter base/% models/% modules/%,$1),$1,$(firstword $(foreach p,$(ALL_PKGS),$(if $(filter $(notdir $p),$1),$p,))))
USER_PKGS := $(strip $(foreach a,$(USER_ARGS),$(call resolve_pkg,$(a))))

ifeq ($(strip $(USER_PKGS)),)
install: $(ALL_PKGS_I) .install/base/all
else
install: $(USER_PKGS:%=.install/%)
endif

ifeq ($(strip $(USER_PKGS)),)
check: $(ALL_PKGS_C) .check/base/all
else
check: $(USER_PKGS:%=.check/%)
endif

ifeq ($(strip $(USER_PKGS)),)
test: $(ALL_PKGS_T) .test/base/all
else
test: $(USER_PKGS:%=.test/%)
endif

ifeq ($(strip $(USER_PKGS)),)
document: $(ALL_PKGS_D) .doc/base/all
else
document: $(USER_PKGS:%=.doc/%)
endif

shiny: $(SHINY_I)

book: 
	cd ./book_source && make build

# Make the timestamp directories if they don't exist yet
.doc .install .check .test .shiny_depends $(call depends,base) $(call depends,models) $(call depends,modules):
	mkdir -p $@

clean:
	rm -rf .install .check .test .doc
	for p in $(SRCS_TO_CLEAN); do \
		find "$$p" \( -name \*.mod -o -name \*.o -o -name \*.so \) -delete; \
	done

help:
	@echo "Usage: make [target] [package]"
	@echo ""
	@echo "Targets supporting [package]: install, check, test, document"
	@echo "  - [package] may be a full path (e.g., modules/data.land) or a bare name (e.g., data.land)"
	@echo ""
	@echo "Examples:"
	@echo "  make all"
	@echo "  make document"
	@echo "  make document data.land                # Generate docs for a specific package (bare name)"
	@echo "  make check ed                          # Run checks for a specific package (bare name)"
	@echo "  make install logger                    # Install a specific package (bare name)"
	@echo "  make test assim.sequential             # Run tests for a specific package (bare name)"
	@echo "  make document modules/data.land        # Full path also supported"
	@echo ""
	@echo "Notes:"
	@echo "  - The above commands are aliases for their dot-prefixed targets (e.g., .doc/<pkg>, .check/<pkg>, etc.)."
	@echo "  - Components not included by default: cable and preles (models), data.mining and DART (modules)."
	@echo "      To install any of these, see comments in the Makefile and be aware they may need code updates."
	@echo "  - Standard workflow: install packages, run checks, test, and document before submitting a PR."
	@echo "  - Before submitting a PR, please ensure that all tests pass, code is linted, and documentation is up-to-date."
	@echo ""
	@echo "Available targets:"
	@echo "  all            Install all packages and generate documentation"
	@echo "  check_base     Run R package checks on all in base/"
	@echo "  check_models   Run R package checks on all in models/"
	@echo "  check_modules  Run R package checks on all in modules/"
	@echo "  document       Generate function documentation (optionally for a single package)"
	@echo "  install        Install all packages or a specified package"
	@echo "  check          Run R package checks on all packages or a specified package"
	@echo "  test           Run unit tests on all packages or a specified package"
	@echo "  shiny          Install dependencies for Shiny apps"
	@echo "  book           Render the PEcAn bookdown documentation"
	@echo "  pkgdocs        Build package documentation websites using pkgdown"
	@echo "  clean          Remove build artifacts"
	@echo "  help           Show this help message"

### Dependencies

# models import Roxygen docs from *installed* version of template,
# so changes in template mean the models need to be redocumented
$(subst .doc/models/template,,$(MODELS_D)): .install/models/template

### Order-only dependencies
# (i.e. prerequisites must exist before building target, but
# target need not be rebuilt when a prerequisite changes)
include Makefile.depends

.install/devtools: | .install
	+ ./scripts/time.sh "devtools ${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
	echo `date` > $@

.install/roxygen2: | .install .install/devtools
	+ ./scripts/time.sh "roxygen2 ${1}" Rscript -e ${SETROPTIONS} \
		-e "if (!requireNamespace('roxygen2', quietly = TRUE)" \
		-e "    || packageVersion('roxygen2') != '"${EXPECTED_ROXYGEN_VERSION}"') {" \
		-e "  cran <- c(getOption('repos'), 'cloud.r-project.org')" \
		-e "  remotes::install_version('roxygen2', '"${EXPECTED_ROXYGEN_VERSION}"', repos = cran, upgrade = FALSE)" \
		-e "}"
	$(eval INSTALLED_ROXYGEN_VERSION := ${EXPECTED_ROXYGEN_VERSION})
	echo `date` > $@

.install/testthat: | .install
	+ ./scripts/time.sh "testthat ${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('testthat', quietly = TRUE)) install.packages('testthat')"
	echo `date` > $@

.install/mockery: | .install
	+ ./scripts/time.sh "mockery ${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('mockery', quietly = TRUE)) install.packages('mockery')"
	echo `date` > $@

$(ALL_PKGS_I) $(ALL_PKGS_C) $(ALL_PKGS_T) $(ALL_PKGS_D): | .install/devtools .install/roxygen2 .install/testthat

.SECONDEXPANSION:
.doc/%: $$(call files_in_dir, %) | $$(@D)
	+ $(call depends_R_pkg, $(subst .doc/,,$@))
	$(call doc_R_pkg, $(subst .doc/,,$@))
	echo `date` > $@

.install/%: $$(call files_in_dir, %) .doc/% | $$(@D)
	+ $(call install_R_pkg, $(subst .install/,,$@))
	echo `date` > $@

.check/%: $$(call files_in_dir, %) | $$(@D)
	+ $(call check_R_pkg, $(subst .check/,,$@))
	echo `date` > $@

.test/%: $$(call files_in_dir, %) | $$(@D)
	$(call test_R_pkg, $(subst .test/,,$@))
	echo `date` > $@

# User-facing alias pattern rules:
# 1) Allow 'make document/<pkg>', 'make install/<pkg>', 'make check/<pkg>', 'make test/<pkg>'
#    to forward to the corresponding dot-prefixed targets without typing them.
document/%: .doc/%
	@true

install/%: .install/%
	@true

check/%: .check/%
	@true

test/%: .test/%
	@true

# 2) Tolerate package args as separate MAKE goals (both full paths and bare names),
#    so 'make <verb> <pkg>' doesn't error on the extra goal. These are no-ops; the real work
#    is triggered by the corresponding verb target prerequisites resolved above.
.PHONY: $(PKG_NAMES)
base/% models/% modules/%:
	@true
$(PKG_NAMES):
	@true

# Install dependencies declared by Shiny apps
.shiny_depends/%: $$(call files_in_dir, %) | $$(@D)
	Rscript scripts/install_shiny_deps.R $(subst .shiny_depends/,shiny/,$@)
	echo `date` > $@
