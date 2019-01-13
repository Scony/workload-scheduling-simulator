all:
	stack setup
	stack build
	stack install --local-bin-path ../bin/

profiling:
	stack setup
	stack install --profile --trace --local-bin-path ../bin/
