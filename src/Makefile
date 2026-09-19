EXECS	  = bsg gen-bsg tanglegram

SOURCES   = \
	Util.ml		\
	IntMap.ml	\
	IntSet.ml	\
	Graph.ml	\
	ELGraph.ml	\
	Cut.ml		\
	Bsg.ml		\
	Gadgets.ml	\
	Solve.ml

C_SOURCES = \
	flow.c

LIBS      = unix.cmxa

#PROFILE  = -p

OCAMLC    = ocamlc.opt -g $(INCLUDES)
OCAMLOPT  = ocamlopt.opt $(INCLUDES)
OCAMLDEP  = ocamldep $(INCLUDES)

CC	  = gcc
CFLAGS    = -std=c99 -g -I $(shell ocamlc -where)
CFLAGS   += -O3 -march=native $(PROFILE) -funroll-all-loops -ffast-math
CFLAGS   += -W -Wall
#CFLAGS  += -DNDEBUG

all: $(EXECS) doc/index.html

OBJS	  = $(SOURCES:.ml=.cmx)
C_OBJS	  = $(C_SOURCES:.c=.o)
DBG_OBJS  = $(SOURCES:.ml=.cmo)
MLIS	 := $(SOURCES:.ml=.mli)
DBG_LIBS  = $(LIBS:.cmxa=.cma)

bsg: $(OBJS) $(C_OBJS) main.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

bsg-dbg: $(C_OBJS) $(DBG_OBJS) main.cmo
	$(OCAMLC) -custom $(PROFILE) -o $@ $(DBG_LIBS) $^

gen-bsg: $(OBJS) $(C_OBJS) gen_bsg.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

find-gadgets: $(OBJS) $(C_OBJS) find_gadgets.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

tanglegram: $(OBJS) $(C_OBJS) tanglegram.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

tanglegram-dbg: $(C_OBJS) $(DBG_OBJS) tanglegram.cmo 
	$(OCAMLC) -custom $(PROFILE) -o $@ $(DBG_LIBS) $^

doc/index.html: $(MLIS)
	mkdir -p doc
	ocamldoc -html -charset utf-8 -d doc $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) $(PROFILE) -c $<

clean:
	rm -f $(EXECS) find-gadgets bsg-dbg tanglegram-dbg core gmon.out
	rm -f *.cm[iox] *.o .depend

realclean: clean
	rm -rf doc
	rm -f *~ *.old *.bak

.depend: $(wildcard *.ml *.mli *.c)
	$(OCAMLDEP) *.mli *.ml > .depend
	$(CC) $(CFLAGS) -MM *.c >> .depend

depend:
	rm -f .depend
	$(MAKE) .depend

.PHONY: all clean realclean depend

# Don't generate .depend just to delete it again.
ifneq ($(if $(MAKECMDGOALS),$(filter-out clean realclean,$(MAKECMDGOALS)),all),)
include .depend
endif
