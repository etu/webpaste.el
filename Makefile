TRAVIS ?= false
EMACS ?= emacs
CASK ?= cask

ifeq ($(TRAVIS),true)
	PATTERN=--pattern '\[ci\]'
endif

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

# Run all tests in tests/integration/
integration:
	${CASK} exec buttercup -L . tests/integration/ ${PATTERN}

build:
	${CASK} build

clean:
	${CASK} clean-elc
