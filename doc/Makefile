DOT_FILES_SRC := $(wildcard dot_files/*.dot)
DOT_FILES_SVG := $(addsuffix .svg, $(basename $(DOT_FILES_SRC)))
DOT_FILES_PNG := $(addsuffix .png, $(basename $(DOT_FILES_SRC)))

# Default target first; build SVGs
svg: $(DOT_FILES_SVG)
png: $(DOT_FILES_PNG)

dot_files/%.svg: dot_files/%.dot
	dot -Tsvg $^ -o $(basename $^).svg

dot_files/%.png: dot_files/%.dot
	dot -Tpng $^ -o $(basename $^).png

clean:
	rm -f $(DOT_FILES_PNG) $(DOT_FILES_SVG)

.PHONY: png svg clean
