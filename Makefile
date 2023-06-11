PACKAGEVERSION:=$(shell sh -c 'grep -F "Version: " DESCRIPTION | cut -f2 -d" "')
PACKAGE:=$(shell sh -c 'grep -F "Package: " DESCRIPTION | cut -f2 -d" "')
# FIXME: This Makefile only works with this BINDIR!
BINDIR=$(CURDIR)/..
RNODE=iridiacluster
RDIR=~/
INSTALL_FLAGS="--with-keep.source"
BUILD_FLAGS=
REALVERSION=$(PACKAGEVERSION).$(SVN_REV)
PACKAGEDIR=$(CURDIR)
PDFLATEX=pdflatex -shell-escape -file-line-error -halt-on-error -interaction=nonstopmode "\input"
RHUB_COMMON_ARGS= path='$(BINDIR)/$(PACKAGE)_$(PACKAGEVERSION).tar.gz', env_vars = c('_R_CHECK_FORCE_SUGGESTS_'='false', R_DEFAULT_SAVE_VERSION='2', R_DEFAULT_SERIALIZE_VERSION='2')
Reval=R --slave -e

Rversion=$(R --version | head -n -1 | grep -m 1 -o -e [0-9] | head -n 1)
ifeq ($(Rversion),2)
 NO_BUILD_VIGNETTES='--no-vignettes'
else
 NO_BUILD_VIGNETTES='--no-build-vignettes'
endif

define Rsed
	R --slave --vanilla -e 'f <- "$(1)"; txt <- sub($(2), $(3), perl=TRUE, readLines(f)); writeLines(txt, f)'
endef

## Do we have git?
ifeq ($(shell sh -c 'which git 1> /dev/null 2>&1 && echo y'),y)
  ## Is this a working copy?
  ifeq ($(shell sh -c 'LC_ALL=C  git describe --first-parent --always | grep -q "[0-9a-z]\+$$"  && echo y'),y)
    $(shell sh -c 'LC_ALL=C  git describe --dirty --first-parent --always --exclude "*" > git_version')
  endif
endif
## Set version information:
SVN_REV = $(shell sh -c 'cat git_version 2> /dev/null')
REVNUM = $(shell sh -c 'cat git_version 2> /dev/null')

.PHONY : help build check clean install pdf rsync version submit cran winbuild vignettes examples genoptions pkgdown

help:
	@echo "quick-install  install the package without rebuilding the vignettes or generating the documentation"
	@echo "setup          install required packages and software to build"
	@echo "install        install the package"
	@echo "build          build the package as a tar.gz file"
	@echo "check          build the package and run 'R CMD check'"
	@echo "check TEST=x   run test called test-x.R"
	@echo "rsync          copy the package and install it on $(RNODE)"
	@echo "cran           build the package and run 'R CMD check --as-cran'"
	@echo "winbuild       submit the package to the WINDOWS builder service"
	@echo "macbuild       submit the package to the MacOS builder service"
	@echo "examples       regenerate the examples used by vignettes"
	@echo "vignettes      generate PDF of the vignettes"
	@echo "submit         submit the package to CRAN (read DEVEL-README first)"

setup:
	./scripts/setup.sh

install: build
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGE)_$(PACKAGEVERSION).tar.gz

quick-install: version
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) $(NO_BUILD_VIGNETTES) $(PACKAGEDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGE)_$(PACKAGEVERSION).tar.gz


gendoc:
	$(Reval) 'devtools::document()'

pkgdown: gendoc
	$(Reval) 'pkgdown::build_site(run_dont_run = TRUE)'
	@$(MAKE) clean

build: gendoc
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) $(PACKAGEDIR)
	@$(MAKE) clean

closeversion:
	git diff-index --quiet HEAD || git ci -a -q -m "Prepare for version $(PACKAGEVERSION)"
	git push origin
	git push origin :refs/tags/v$(PACKAGEVERSION) # Remove any existing tag
	git tag -f -a v$(PACKAGEVERSION) -m "Version $(PACKAGEVERSION)"
	git push --tags
	@cp cran-comments-template.md cran-comments.md
	@sed -i 's/VERSION/$(PACKAGEVERSION)/' cran-comments.md
	@cat cran-comments.md
	@echo "-----> If the above is correct, then do make submit <----"


releasebuild: BUILD_FLAGS=--compact-vignettes=qpdf
releasebuild: gendoc 
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) $(PACKAGEDIR) && tar -atvf $(PACKAGE)_$(PACKAGEVERSION).tar.gz

cran: releasebuild
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false _R_CHECK_CRAN_INCOMING_=0 R CMD check --as-cran $(PACKAGE)_$(PACKAGEVERSION).tar.gz

releasecheck: cran
	$(Reval) 'urlchecker::url_check()'
	$(MAKE) winbuild
	$(MAKE) macbuild

check: build
ifdef TEST
	_R_CHECK_FORCE_SUGGESTS_=false NOT_CRAN=true $(Reval) 'devtools::test(filter="$(TEST)", stop_on_failure = TRUE)'
else
	cd $(BINDIR) && (_R_CHECK_FORCE_SUGGESTS_=false NOT_CRAN=true R CMD check --run-donttest --timings $(PACKAGE)_$(PACKAGEVERSION).tar.gz; cat $(PACKAGE).Rcheck/$(PACKAGE)-Ex.timings)
endif

clean: 
	#cd $(PACKAGEDIR) && (./cleanup; make -C src -f Makevars clean)

## FIXME: building the vignettes is a bit complicated and sometimes fails.
# If \setboolean{Release}{false}, entries are taken from optbib and everything
# should work. However, we cannot build the package like this because we cannot
# distribute optbib. So when \setboolean{Release}{true}, we take entries from
# irace-package.bib, to obtain irace-package.bib, we need to build with
# \setboolean{Release}{false}, use aux2bib on the .aux file, then set
# \setboolean{Release}{true}, then rebuild again. Ideally:
#
# make vignettes' or 'make pdf' should build in whatever Release value we have
# and do whatever is necessary to get it working.
#
# make build/check/cran/install/releasebuild should build with Release as true
# and do whatever is necessary to get it working, but avoid doing extra
# work. For example, if irace-package.bib is non-empty, then do not build two
# times with Release false and then with Release true.
#
# make nonreleasevignette should build with Release as false (which is faster and should always work).
#
# It is ok to fail if something is missing or needs to be done and give a nice error. For example, if optbib is missing.
#vignettes: version vignettes/$(PACKAGE)-package.Rmd
# FIXME: How to display the output of the latex and bibtex commands with R CMD?
# FIXME: How to halt on warning?

pdf: build
	R CMD Rd2pdf --force --no-preview --batch --output=$(BINDIR)/$(PACKAGE)_$(PACKAGEVERSION).pdf $(PACKAGEDIR)/

rsync:
ifndef RDIR
	@echo "ERROR: You must specify a remote dir (e.g., RDIR=~/)"
	@exit 1
endif
ifndef RNODE
	@echo "ERROR: You must specify a remote node (e.g., RNODE=majorana)"
	@exit 1
else
	rsync -rlp -CIzc -L --delete --copy-unsafe-links --exclude=.svn --exclude=/examples/ --progress --relative \
	.     \
	$(RNODE):$(RDIR)/$(PACKAGE)/
	ssh $(RNODE) '. ~/.profile && cd $(RDIR)/$(PACKAGE) && make quick-install'
endif

submit:
	$(Reval) 'devtools::submit_cran()'

postrelease:
	$(Reval) 'writeLines(con = "DESCRIPTION", sub(pattern = "Version:[[:space:]]+([0-9.-]+)[[:space:]]*$$", replace = "Version: \\1.9000", x = readLines("DESCRIPTION"), perl=TRUE))'
	$(MAKE) build
	@rm -f cran-comments.md CRAN-SUBMISSION
	git ci -a -m "Version $(PACKAGEVERSION) released.  Bump to development version."

remotecran: releasebuild
	$(Reval) "rhub::check_for_cran($(RHUB_COMMON_ARGS), show_status = TRUE)"

macbuild: releasebuild
	$(Reval) "devtools::check_mac_release()"

winbuild: releasebuild
	$(Reval) "devtools::check_win_release()"
	$(Reval) "devtools::check_win_devel()"
	$(Reval) "rhub::check_on_windows($(RHUB_COMMON_ARGS))"

examples: quick-install
	@echo "*** Makefile: Regenerating data for vignettes and examples. This will take time..."
	cd examples/vignette-example/ && R --vanilla --slave --file=create-example-file.R
	mv -t vignettes/ examples/vignette-example/*.Rdata examples/vignette-example/irace-acotsp-stdout.txt 
	$(MAKE) vignettes
	$(MAKE) check

covr: build
	$(Reval) "Sys.setenv(NOT_CRAN='true');covr::report(covr::package_coverage(type='all'))"
