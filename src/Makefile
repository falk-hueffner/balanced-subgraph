PACKAGE	  = bsg
VER	  = 2.0
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
CFLAGS    = -std=c99 -g -I $(HOME)/include
CFLAGS   += -O3 $(shell gcc-arch) $(PROFILE) -funroll-all-loops -ffast-math
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

gen-bsg: $(OBJS) $(C_OBJS) gen-bsg.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

find-gadgets: $(OBJS) $(C_OBJS) find-gadgets.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

tanglegram.cmx: tanglegram.ml
	$(OCAMLOPT) -pp camlp4o $(PROFILE) -c $<

tanglegram.cmo: tanglegram.ml
	$(OCAMLC) -pp camlp4o $(PROFILE) -c $<

tanglegram: $(OBJS) $(C_OBJS) tanglegram.cmx
	$(OCAMLOPT) -pp camlp4o $(PROFILE) -o $@ $(LIBS) $^

tanglegram-dbg: $(C_OBJS) $(DBG_OBJS) tanglegram.cmo 
	$(OCAMLC) -custom -pp camlp4o $(PROFILE) -o $@ $(DBG_LIBS) $^

doc/index.html: $(MLIS)
	mkdir -p doc
	ocamldoc -html -d doc $^

dist: all
	rm -rf $(PACKAGE)-$(VER)/
	mkdir $(PACKAGE)-$(VER)/
	cp COPYING README $(PACKAGE)-$(VER)/
	cp $(SOURCES) $(MLIS) $(PACKAGE)-$(VER)/
	cp main.ml gen-bsg.ml tanglegram.ml $(PACKAGE)-$(VER)/
	cp $(C_SOURCES) $(PACKAGE)-$(VER)/
	cp Makefile .depend $(PACKAGE)-$(VER)/
	mkdir $(PACKAGE)-$(VER)/test/
	cp test/* $(PACKAGE)-$(VER)/test/
	cp test.sh $(PACKAGE)-$(VER)/
	mkdir $(PACKAGE)-$(VER)/data/
	cp -r ../data/* $(PACKAGE)-$(VER)/data/
	cp $(PACKAGE)-lp $(PACKAGE)-$(VER)/
	GZIP=--best tar -cvzf $(PACKAGE)-$(VER).tar.gz $(PACKAGE)-$(VER)

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

depend: $(SOURCES)
	$(OCAMLDEP) -pp camlp4o *.mli *.ml > .depend
	$(CC) $(CFLAGS) -MM *.c >> .depend

include .depend
