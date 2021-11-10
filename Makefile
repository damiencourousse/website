PDFS := $(wildcard mespubli/pdf/*.pdf)
LINKS := $(patsubst mespubli/%,%,$(PDFS))

.PHONY: build rebuild watch
build: stack_build update_pdf_links
	stack exec site -- $@
rebuild: stack_build update_pdf_links
	stack exec site -- $@
watch: stack_build
	stack exec site -- $@

.PHONY: stack_build
stack_build:
	stack build

.PHONY: update_pdf_links
update_pdf_links: $(LINKS)

%.pdf:
	cd pdf && ln -s ../mespubli/pdf/$(shell basename $@) .

deploy: check-upstream git-clean-status rebuild
	rsync -avr --delete --exclude='.git'  _site/ site/
	cd site \
		&& git checkout master \
		&& git add . \
		&& GIT_AUTHOR_EMAIL='3405965+damiencourousse@users.noreply.github.com' git commit -m 'site update' \
		&& git push --recurse-submodules=check origin master
	git add site
	git commit -m 'site update'
	git push origin master
	git push github master

check-upstream:
	# dependency to the build rule 'git-clean-status': abort if we are in a dirty state.  Do this a first time before updating submodules: the update way take a few seconds and we want to abort quickly if need be.

	# Check that we are either up to date, or ahead of the default
	# upstream branch, and merge if need be.
	git submodule update --recursive --remote --merge
.PHONY: check-upstream

git-clean-status:
	# MAYBE This is fragile!  Any better options for making sure
	# that we are up to date or ahead of upstream?
	LANG=C git status --porcelain --untracked-files=no --ignore-submodules=untracked | wc -l | grep -q -e '0' || (git status --untracked-files=no --ignore-submodules=untracked; echo "\n\nERROR. The repository is in a dirty state.\nPlease commit changes and delete all untracked changes."; return 1)
.PHONY: git-clean-status

debug:
	@echo $(LINKS)
	@echo $(PDFS)
