ELM_FILES = $(shell find . -path ./elm.json -prune -o -type f -name '*.elm' ! -wholename './src/Main.elm')

all: build $(ELM_FILES) build/js/Main.js

.PHONY: clean build

build/js/%.js: src/%.elm
	elm make $< --optimize --output $@

build:
	mkdir -p build
	cp -r static/* build/

clean:
	rm -rf build
