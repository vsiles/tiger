all:
	dune build src/tigercc.exe src/tigercc.bc

test:
	dune runtest

clean:
	dune clean


.PHONY: all test clean
