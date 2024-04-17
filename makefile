all: refs, refs_test, streamstrees, music

refs: refs.ml
	ocamlbuild -use-ocamlfind refs.byte

refs_test: refs_test.ml
	ocamlbuild -use-ocamlfind refs_test.byte

streamstrees: streamstrees.ml
	ocamlbuild -use-ocamlfind streamstrees.byte

music: music.ml
	ocamlbuild -use-ocamlfind music.byte

clean:
	rm -rf _build *.byte