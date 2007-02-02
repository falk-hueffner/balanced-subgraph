PACKAGE	  = scs
VER	  = 1.0
EXECS	  = scs gen-scs

SOURCES   = \
	Util.ml		\
	IntMap.ml	\
	IntSet.ml	\
	Graph.ml	\
	ELGraph.ml	\
	Cut.ml		\
	Scs.ml		\
	Gadgets.ml	\
	Solve.ml

C_SOURCES = \
	flow.c

LIBS      = unix.cmxa

#PROFILE  = -p

OCAMLC    = ocamlc -g -warn-error A $(INCLUDES)
OCAMLOPT  = ocamlopt.opt -warn-error A $(INCLUDES)
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

scs: $(OBJS) $(C_OBJS) main.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

#scs-dbg: $(C_OBJS) $(DBG_OBJS) main.cmo
scs-dbg: $(C_OBJS) $(DBG_OBJS) main.cmo
	$(OCAMLC) -custom $(PROFILE) -o $@ $(DBG_LIBS) $^

gen-scs: $(OBJS) $(C_OBJS) gen-scs.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

find-gadgets: $(OBJS) $(C_OBJS) find-gadgets.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

doc/index.html: $(MLIS)
	mkdir -p doc
	ocamldoc -html -d doc $^

dist: all
	rm -rf $(PACKAGE)-$(VER)/
	mkdir $(PACKAGE)-$(VER)/
	cp COPYING README $(PACKAGE)-$(VER)/
	cp $(SOURCES) $(MLIS) $(PACKAGE)-$(VER)/
	cp main.ml gen-scs.ml $(PACKAGE)-$(VER)/
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

.depend: depend

depend: $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend
	$(CC) $(CFLAGS) -MM *.c >> .depend

include .depend
