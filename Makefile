CC=erlc
PY=python

TARGET=/usr/local/bin
SDIR=src
LDIR=$(SDIR)/code_generator
EXDIR=examples

SRCS= base.erl common.erl builtins.erl
OBJS= ${SRCS:.erl=.beam}
EXAMPLES= `ls $(EXDIR)/*.py`
PYC=`find . -name *.pyc`
BEAM=`find $(SDIR) -name *.beam`
DUMP=`find . -name *.dump`

.PHONY: all clean distclean test

all:
	@for i in $(SRCS); do \
	$(CC) -o $(LDIR) $(LDIR)/$$i; done

install: all
	mkdir -p $(TARGET)/py2beam
	cp py2beam.py $(TARGET)/
	cp -r $(SDIR)/* $(TARGET)/py2beam
	$(TARGET)/py2beam.py -i 0
	chmod 777 $(TARGET)/py2beam/frontend/parsetab.py

clean: clean_pyc

clean_pyc: clean_beam
	@for i in $(PYC); do \
	rm -f $$i; done

clean_beam: clean_dump
	@for i in $(BEAM); do \
	rm -f $$i; done

clean_dump:
	@for i in $(DUMP); do \
	rm -f $$i; done


distclean: clean
	rm -f $(TARGET)/py2beam.py
	rm -rf $(TARGET)/py2beam/

test: build_examples

build_examples:
	@for i in $(EXAMPLES); do \
	$(PY) $(SDIR)/py2beam.py $$i; done
