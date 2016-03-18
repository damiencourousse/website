.PHONY: build rebuild watch
build: stack_build
	stack exec site -- $@
rebuild: stack_build
	stack exec site -- $@
watch: stack_build
	stack exec site -- $@

.PHONY: stack_build
stack_build:
	stack build

push: rebuild
	git submodule update --remote --merge
	rsync -avr --delete --exclude='.git'  _site/ site/
	cd site \
		&& git checkout master \
		&& git add . \
		&& git commit -m 'site update' \
		&& git push origin master
	git add site
	git commit -m 'site update'
	git push origin master


