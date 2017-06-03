TRAVIS_EVENT_TYPE ?= push
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

	@if [ "$(TRAVIS_EVENT_TYPE)" = "cron" ]; then \
		${MAKE} integration;                      \
	fi

# Run all tests in tests/integration/
integration:
	${CASK} exec buttercup -L . tests/integration/

build:
	${CASK} build

clean:
	${CASK} clean-elc
