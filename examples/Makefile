all: build

test: build
	pandoc -M xxx=YYY \
	--filter=../_build/default/examples/inspect.exe \
	--filter=../_build/default/examples/replace.exe \
	--filter=../_build/default/examples/inspect.exe \
	test.md

build:
	@dune build
