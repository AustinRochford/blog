clean: site
	./site clean

preview: rebuild
	./site preview

rebuild: site _site
	./site rebuild

site: site.hs
	ghc --make site.hs
