all:
	stack build
	# ./microml compile tests/test.ml | ./microml run --trace -
	./microml eval --show tests/test.ml
