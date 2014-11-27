.PHONY: build rebuild watch
build:
	cabal run $@
rebuild:
	cabal run $@
watch:
	cabal run $@

push: rebuild
	git submodule update --remote --merge
	cd site \
		&& git checkout master \
		&& rsync -avr --delete ../_site/* ./ \
		&& git add . \
		&& git commit -m 'site update' \
		&& git push origin master
	git add site
	git commit -m 'site update'


