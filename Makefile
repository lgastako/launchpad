help:
	@cat Makefile

build:
	stack build

clean:
	stack clean

dist-clean:
	\rm -rf .stack-work

hlint:
	stack exec hlint ./src
	stack exec hlint ./test

longboye-all:
	longboye imports src
	longboye imports test
	longboye pragmas src
	longboye pragmas test

test:
	stack test

watch:
	stack build --fast --file-watch

watch-test:
	 stack test --fast --file-watch

b: build
hl: hlint
i: install
lba: longboye-all
w: watch
t: test
wt: watch-test

.PHONY: test
