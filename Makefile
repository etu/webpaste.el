EMACS ?= emacs
CASK ?= cask

all: test

test:
	${MAKE} clean
	${MAKE} unit
	${MAKE} build
	${MAKE} unit
	${MAKE} clean

unit:
	${CASK} exec ert-runner

build:
	${CASK} build

clean:
	${CASK} clean-elc

.PHONY: test
