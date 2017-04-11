###########################################################################
##                                                                       ##
##                              OByteLib                                 ##
##                                                                       ##
##                            Benoit Vaugon                              ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file LICENSE-en.                                               ##
##                                                                       ##
###########################################################################

include etc/Makefile.conf

all: config
	@make --no-print-directory -C src

config:
	@if [ etc/Makefile.conf -ot VERSION -o \
             etc/Makefile.conf -ot configure ]; then \
          echo 'Configuration files are not up to date.' 1>&2; \
	  echo 'Please run `./configure` (with right options).' 1>&2; \
          exit 1; \
	fi

install: all
	mkdir -p "$(LIBDIR)"
	mkdir -p "$(MAN3DIR)"
	cp lib/oByteLib.cmi  "$(LIBDIR)/"
	cp lib/obytelib.cma  "$(LIBDIR)/"
	cp lib/obytelib.cmxa "$(LIBDIR)/"
	cp lib/obytelib.a    "$(LIBDIR)/"
	cp man/OByteLib*.3o "$(MAN3DIR)/"

uninstall:
	rm -f "$(LIBDIR)/oByteLib.cmi"
	rm -f "$(LIBDIR)/obytelib.cma"
	rm -f "$(LIBDIR)/obytelib.cmxa"
	rm -f "$(LIBDIR)/obytelib.a"
	rm -f "$(MAN3DIR)/OBytelib*.3o"

etc/Makefile.conf:
	@echo "You must run ./configure before" 1>&2
	@exit 1

clean:
	@make --no-print-directory -C src clean

.PHONY: all config install uninstall clean
