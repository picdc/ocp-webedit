OCAMLDEP=ocamldep

INCLUDES=  -I ../../_obuild/common
CAMLFLAGS= -thread -w @a-29 $(INCLUDES)
CAMLOPTFLAGS= $(CAMLFLAGS) -inline 10000
CAMLLIBS= unix.cma
CFLAGS= -arch x86_64 -O3 -Wall -Wextra
PKGS= -package threads,pcre,netplex,netcgi2,nethttpd,shell,netclient,ssl,equeue-ssl,yojson,netstring,sqlite3,sha,admin

SRC= serverConfig.ml serverMain.ml
OBJ= $(SRC:.ml=.cmx)

EXEC= server.asm

.SUFFIXES: .ml .mli .cmi .cmo .cmx

all: $(EXEC)


$(EXEC): $(OBJ)
	ocamlfind ocamlopt $(CAMLOPTFLAGS) $(PKGS) -linkpkg $(INCLUDES) common.cmxa $(OBJ) -o $(EXEC)

.mli.cmi:
	ocamlfind ocamlc $(CAMLFLAGS) $(PKGS) -c $<

.ml.cmo:
	ocamlfind ocamlc $(CAMLFLAGS) $(PKGS) -c $<

.ml.cmx:
	ocamlfind ocamlopt $(CAMLOPTFLAGS) $(PKGS) -c $<

.c.o:
	ocamlfind ocamlc -ccopt "$(CFLAGS)" -c $<

clean:
	rm -rf *.cm* *.o *.a *.so *.log *~ \#*\#
	rm -rf $(EXEC)

# Cannot be automatically infered
serverConfig.cmo : ../../_obuild/common/commonConfig.cmo
serverConfig.cmx : ../../_obuild/common/commonConfig.cmx

depend:
	$(OCAMLDEP) -one-line $(INCLUDES) *.ml *.mli > .depend

include .depend

.PHONY: clean all
