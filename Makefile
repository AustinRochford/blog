LIVE_URI=daisy:~/www/live

CABAL=/Users/austin/Library/Haskell/bin/cabal
EXECUTABLE=./dist/build/blog/blog

clean: site
	${EXECUTABLE} clean

deploy: clean rebuild
	rsync --recursive --delete --checksum _site/ ${LIVE_URI}

preview: rebuild
	${EXECUTABLE} watch

rebuild: site
	${EXECUTABLE} rebuild

site: site.hs
	${CABAL} build
