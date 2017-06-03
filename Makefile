EMACS ?= emacs
CASK ?= cask

all: test

test:
	${MAKE} clean
	${MAKE} unit
	${MAKE} build
	${MAKE} unit
	${MAKE} clean

# Run all tests in tests/unit/
unit:
	${CASK} exec buttercup -L . tests/unit/

build:
	${CASK} build

clean:
	${CASK} clean-elc
