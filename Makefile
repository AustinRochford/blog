LIVE=/home/austin/www/live

clean: site
	./site clean

deploy: clean rebuild
	rsync --recursive --delete --checksum _site/ ${LIVE}

post:
	touch posts/`date +%Y-%m-%d`-${TITLE}.mkd

preview: rebuild
	./site preview

rebuild: site
	./site rebuild

site: site.hs
	ghc --make site.hs
