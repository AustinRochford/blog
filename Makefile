LIVE_URI=daisy:~/www/live

CABAL=/Users/austin/Library/Haskell/bin/cabal
EXECUTABLE=./dist/build/blog/blog

clean: ${EXECUTABLE}
	${EXECUTABLE} clean

deploy: clean rebuild
	rsync --recursive --delete --checksum _site/ ${LIVE_URI}

preview: rebuild
	${EXECUTABLE} watch

rebuild: ${EXECUTABLE}
	${EXECUTABLE} rebuild

${EXECUTABLE}: site.hs
	${CABAL} build
