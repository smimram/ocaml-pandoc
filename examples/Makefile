all: build

test: build
	pandoc \
	--filter=../_build/default/examples/inspect.exe \
	--filter=../_build/default/examples/comment.exe \
	--filter=../_build/default/examples/inspect.exe \
	test.md

build:
	@dune build
