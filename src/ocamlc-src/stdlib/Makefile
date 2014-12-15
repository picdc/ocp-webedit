#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../LICENSE.      #
#                                                                       #
#########################################################################

# $Id: Makefile 11156 2011-07-27 14:17:02Z doligez $

CAMLC=ocamlc
COMPILER=$(CAMLC)
CAMLDEP= ocamldep

OBJS=pervasives.cmo $(OTHERS)
OTHERS=array.cmo list.cmo char.cmo string.cmo sys.cmo \
  sort.cmo marshal.cmo obj.cmo \
  int32.cmo nativeint.cmo int64.cmo \
  lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo \
  camlinternalLazy.cmo lazy.cmo stream.cmo \
  buffer.cmo printf.cmo \
  arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo hashtbl.cmo format.cmo scanf.cmo callback.cmo \
  camlinternalOO.cmo oo.cmo camlinternalMod.cmo \
  genlex.cmo weak.cmo \
  filename.cmo complex.cmo \
  arrayLabels.cmo listLabels.cmo stringLabels.cmo moreLabels.cmo stdLabels.cmo std_exit.cmo

MLI=$(OBJS:.cmo=.cmi)
MYNATIVEINT= -I ../_obuild/utils/ utils.cma 


COMPFLAGS=-strict-sequence -g -warn-error A -nostdlib $(MYNATIVEINT) 

stdlib.cma: depend $(OBJS)
	$(CAMLC) -nostdlib -a -o stdlib.cma $(OBJS)


.SUFFIXES: .mli .ml .cmi .cmo .cmx .p.cmx


.mli.cmi:
	$(CAMLC) $(COMPFLAGS) `./Compflags $@` -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) `./Compflags $@` -c $<


# # Dependencies on the compiler
# $(OBJS) std_exit.cmo: $(COMPILER)
# $(OBJS:.cmo=.cmi) std_exit.cmi: $(COMPILER)
# $(OBJS:.cmo=.cmx) std_exit.cmx: $(OPTCOMPILER)
# $(OBJS:.cmo=.p.cmx) std_exit.p.cmx: $(OPTCOMPILER)

# Dependencies on Pervasives (not tracked by ocamldep)
$(OBJS) std_exit.cmo: pervasives.cmi
$(OTHERS:.cmo=.cmi) std_exit.cmi: pervasives.cmi
$(OBJS:.cmo=.cmx) std_exit.cmx: pervasives.cmi
$(OBJS:.cmo=.p.cmx) std_exit.p.cmx: pervasives.cmi
$(OTHERS:.cmo=.cmx) std_exit.cmx: pervasives.cmx
$(OTHERS:.cmo=.p.cmx) std_exit.p.cmx: pervasives.cmx


-include .depend

clean:
	rm -rf *.cm[iaot]

depend:
	$(CAMLDEP) *.mli *.ml > .depend
	$(CAMLDEP) *.ml | sed -e 's/\.cmx/.p.cmx/g' >>.depend
