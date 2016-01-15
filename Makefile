all: **/*.hs
	stack build
	stack exec main
