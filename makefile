all:
	stack setup
	stack build
	stack install --local-bin-path ../bin/
