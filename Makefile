all:
	stack build
	# ./microml compile tests/test.ml | ./microml run --trace -
	# ./microml type tests/test.ml
	# ./microml compile tests/test.ml | ./microml run -
	./microml eval --show tests/test.ml
