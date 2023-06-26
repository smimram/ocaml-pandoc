all: build

build clean:
	@dune $@

doc:
	@dune build @doc
