EXECS	  = ulp gen-ulp

SOURCES   = \
	Util.ml		\
	Queue.ml	\
	IntMap.ml	\
	IntSet.ml	\
	UnionFind.ml	\
	Graph.ml	\
	Digraph.ml	\
	ELGraph.ml	\
	ELDigraph.ml	\
	Diflow.ml	\
	Cut.ml		\
	Flow.ml		\
	Ulp.ml

C_SOURCES = \
	flow.c

LIBS      = unix.cmxa

#PROFILE  = -p

OCAMLC    = ocamlc -g -warn-error A $(INCLUDES)
OCAMLOPT  = ocamlopt -warn-error A $(INCLUDES)
OCAMLDEP  = ocamldep $(INCLUDES)

CFLAGS    = -std=c99 -g -I $(HOME)/include
CFLAGS   += -O3 $(shell gcc-arch) $(PROFILE) -funroll-all-loops -ffast-math
CFLAGS   += -W -Wall -Wno-unused -Werror
#CFLAGS  += -DNDEBUG

all: depend $(EXECS) doc/index.html

OBJS = $(SOURCES:.ml=.cmx)
C_OBJS = $(C_SOURCES:.c=.o)
DBG_OBJS = $(SOURCES:.ml=.cmo)
MLIS := $(wildcard *.mli)
DBG_LIBS = $(LIBS:.cmxa=.cma)

ulp: $(OBJS) $(C_OBJS) main.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

#ulp-dbg: $(C_OBJS) $(DBG_OBJS) main.cmo
ulp-dbg: $(C_OBJS) $(DBG_OBJS) main.cmo
	$(OCAMLC) $(PROFILE) -o $@ $(DBG_LIBS) $^

gen-ulp: $(OBJS) $(C_OBJS) gen-ulp.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

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
	rm -rf doc
	rm -f *~ *.old *.bak

.depend: depend

depend: $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend
	$(CC) $(CFLAGS) -MM *.c >> .depend

include .depend
