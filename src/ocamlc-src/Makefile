
include ../../config/Makefile

OBUILD= ../../_obuild

BYTECOMP=$(OBUILD)/bytecomp
UTILS=$(OBUILD)/utils
PARSING=$(OBUILD)/parsing
TYPING=$(OBUILD)/typing
DRIVER=$(OBUILD)/ocamlc-src

PKG_OCAMLC=ocamlc-src

PKG_TOPLEVEL= toplevel-src

DIR_TOPLEVEL= ../toplevel-src

JSFLAGS= -pretty -noinline
JSINCLUDES= -I $(DIR_TOPLEVEL)/cmicomp -I $(UTILS) -I $(PARSING) -I $(TYPING) \
 -I $(BYTECOMP) -I $(DRIVER) -I $(JS_COMPILER_LIBS)

OCAMLC= $(OBUILD)/$(PKG_OCAMLC)/$(PKG_OCAMLC)

all: progmagic.byte ocamlc

ocamlc: 
	ocp-build build ocamlc-src
	$(MAKE) ocamlc-js

ocamlc-js: ocamlc.js

ocamlc.js: $(OCAMLC).byte ocp-runtime.js stdlib_cma.js
	js_of_ocaml $(JSFLAGS) -toplevel $(JSINCLUDES) \
	   -o ocamlc.js \
	   -I . \
	   ocp-filesystem.js ocp-runtime.js \
	   stdlib_cma.js $(OCAMLC).byte

stdlib_cma.js: $(OBUILD)/my-stdlib/my-stdlib.cma progmagic.byte	
	cp  $(OBUILD)/my-stdlib/my-stdlib.cma stdlib/stdlib.cma
	./progmagic.byte -js stdlib_cma.js stdlib/stdlib.cma $(OBUILD)/my-stdlib/graphics.cmi

progmagic.byte: progmagic.ml
	ocamlc -o $@ $<

$(OCAMLC).byte: ocpbuild

ocpbuild:


clean:
	rm -rf progmagic.byte
	rm -rf stdlib_cma.js
	make -C stdlib clean
	ocp-build clean
