all: build

build:
	stack build

eval: build
	./microml eval --show tests/test.ml

run: build
	./microml type tests/test.ml
	./microml compile tests/test.ml | ./microml run -

trace: build
	./microml type tests/test.ml
	./microml compile tests/test.ml | ./microml run --trace -

clean:
	stack clean
