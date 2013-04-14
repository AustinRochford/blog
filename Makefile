clean: site
	./site clean

preview: site
	./site preview

rebuild: site
	./site rebuild

site: site.hs
	ghc --make site.hs
