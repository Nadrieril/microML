all: slides.pdf

%.pdf: build/%.tex
	mkdir -p build
	xelatex -output-directory=build $<
	ln -sf "build/$@" "$@"

RAPPORT_FILES=slides.md

build/slides.tex: meta.yaml $(RAPPORT_FILES)
	pandoc -S -s --standalone -t beamer --latex-engine=xelatex -o "$@" meta.yaml $(RAPPORT_FILES)

clean:
	rm -rf build

mkproper: clean
	rm *.pdf
