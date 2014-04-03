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
TARGS = $(LIB)/oByteLib.cmi $(LIB)/obytelib.cma \
	$(LIB)/obytelib.cmxa $(LIB)/obytelib.a \
	$(MAN)/OByteLib.3o

all: $(TARGS)

_build/%: $(SRCS) config.ml
	@rm -f $@
	$(OCAMLBUILD) -cflag -g -lflag -g $*

$(LIB)/oByteLib.cmi: _build/oByteLib.cmi
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

clean:
	@$(OCAMLBUILD) -clean
	@rm -f $(TARGS) $(MAN)/*.3o config.ml

.PHONY: all clean
