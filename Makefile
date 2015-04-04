LIVE_URI=s3://austinrochford.com/
AWS_IAM_USER=blog

CABAL=cabal
EXECUTABLE=./dist/build/blog/blog

clean: ${EXECUTABLE}
	${EXECUTABLE} clean

deploy:
	aws --profile ${AWS_IAM_USER} s3 sync _site/ ${LIVE_URI}

preview: rebuild
	${EXECUTABLE} watch -h "0.0.0.0"

rebuild: ${EXECUTABLE}
	${EXECUTABLE} rebuild

${EXECUTABLE}: site.hs
	${CABAL} build
