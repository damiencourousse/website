.PHONY: build rebuild watch
build:
	cabal run $@
rebuild:
	cabal run $@
watch:
	cabal run $@

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


