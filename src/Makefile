###########################################################################
##                                                                       ##
##                              OByteLib                                 ##
##                                                                       ##
##                            Benoit Vaugon                              ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file ../LICENSE-en.                                            ##
##                                                                       ##
###########################################################################

include ../etc/Makefile.conf

LIB = ../lib
MAN = ../man
SRCS = $(wildcard *.ml *.mli *.mlpack)
TARGS = $(LIB)/oByteLib.cmi $(LIB)/oByteLib.cmx \
	$(LIB)/obytelib.cma $(LIB)/obytelib.a   \
	$(LIB)/obytelib.cmxa $(MAN)/OByteLib.3o

all: $(TARGS)

_build/%: $(SRCS) prim.ml config.ml
	@rm -f $@
	$(OCAMLBUILD) -cflag -g -lflag -g $*

$(LIB)/oByteLib.cmi: _build/oByteLib.cmi
	cp $< $@

$(LIB)/oByteLib.cmx: _build/oByteLib.cmx
	cp $< $@

$(LIB)/obytelib.cma: _build/oByteLib.cma
	cp $< $@

$(LIB)/obytelib.cmxa: _build/oByteLib.cmxa
	cp $< $@

$(LIB)/obytelib.a: _build/oByteLib.a
	cp $< $@

_build/OByteLib.3o: _build/oByteLib.cmi
	cd _build/; ocamldoc -man -man-mini oByteLib.mli

$(MAN)/OByteLib.3o: _build/OByteLib.3o
	cp _build/*.3o ../man/

config.ml: ../etc/config.ml
	cp $< $@

prim.ml: ../etc/prim.ml
	cp $< $@

clean:
	@$(OCAMLBUILD) -clean
	@rm -f $(TARGS) $(MAN)/*.3o prim.ml config.ml

.PHONY: all clean
