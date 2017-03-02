EMACS ?= emacs
CASK ?= cask

all: test

test:
	${MAKE} clean
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean

unit:
	${CASK} exec ert-runner

compile:
	${CASK} build

clean:
	${CASK} clean-elc

.PHONY: test
