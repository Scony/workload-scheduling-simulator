all:
	stack setup
	stack build
	stack install --local-bin-path bin/

profiling:
	stack setup
	stack install --profile --trace --local-bin-path bin/

hlint:
	hlint app/ src/ -r

clean:
	rm -rf bin
	rm -f report.html
	rm -f simulator-exe.prof
