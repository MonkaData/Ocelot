.PHONY: all test clean

all:
	opam exec -- dune build

test:
	opam exec -- dune runtest

clean:
	opam exec -- dune clean
