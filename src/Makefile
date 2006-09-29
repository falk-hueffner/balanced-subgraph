EXECS	  = ulp

SOURCES   = \
	Util.ml		\
	IntMap.ml	\
	IntSet.ml	\
	Graph.ml

OCAMLC    = ocamlc -g -warn-error A $(INCLUDES)
OCAMLOPT  = ocamlopt -warn-error A $(INCLUDES)
OCAMLDEP  = ocamldep $(INCLUDES)

#PROFILE  = -p

all: depend $(EXECS) doc/index.html

OBJS = $(SOURCES:.ml=.cmx)
DBG_OBJS = $(SOURCES:.ml=.cmo)
MLIS := $(wildcard *.mli)
DBG_LIBS = $(LIBS:.cmxa=.cma)

ulp: $(OBJS) ulp.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

ulp-dbg: $(DBG_OBJS) ulp.cmo
	$(OCAMLC) $(PROFILE) -o $@ $(DBG_LIBS) $^

doc/index.html: $(MLIS)
	mkdir -p doc
	ocamldoc -html -d doc $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) $(PROFILE) -c $<

clean:
	rm -f $(EXECS) ecc-dbg core gmon.out
	rm -f *.cm[iox] *.o $(PROJ)

realclean: clean
	rm -f *~ *.old *.bak doc

.depend: depend

depend: $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend
