CC=erlc
PY=python
SRCS= base.erl common.erl builtins.erl
OBJS= ${SRCS:.erl=.beam}
SDIR=src
LDIR=$(SDIR)/code_generator
EXAMPLESDIR=examples
EXAMPLES= `ls examples/*.py`
FILES = 


.PHONY: all clean distclean examples

libs:
	$(CC) -o $(LDIR) $(LDIR)/base.erl $(LDIR)/common.erl $(LDIR)/builtins.erl

clean:
	rm -f $(LDIR)/*.beam

distclean: clean

examples:
	@for i in $(EXAMPLES); do \
	$(PY) $(SDIR)/py2beam.py $$i; done
