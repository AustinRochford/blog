LIVE_URI=daisy:~/www/live

CABAL=cabal
EXECUTABLE=./dist/build/blog/blog

clean: ${EXECUTABLE}
	${EXECUTABLE} clean

deploy:
	rsync -a --checksum _site/ ${LIVE_URI}

preview: rebuild
	${EXECUTABLE} watch -h "0.0.0.0"

rebuild: ${EXECUTABLE}
	${EXECUTABLE} rebuild

${EXECUTABLE}: site.hs
	${CABAL} build
