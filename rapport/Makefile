all: rapport.pdf

%.pdf: build/%.tex
	mkdir -p build
	xelatex -output-directory=build $<
	ln -sf "build/$@" "$@"

RAPPORT_FILES=rapport.md

build/rapport.tex: meta.yaml $(RAPPORT_FILES)
	pandoc -S -s --standalone --latex-engine=xelatex -o "$@" meta.yaml $(RAPPORT_FILES)

clean:
	rm -rf build

mkproper: clean
	rm *.pdf
