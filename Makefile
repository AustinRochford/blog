clean: site
	./site clean

post:
	touch posts/`date +%Y-%m-%d`-${TITLE}.mkd

preview: rebuild
	./site preview

rebuild: site
	./site rebuild

site: site.hs
	ghc --make site.hs
