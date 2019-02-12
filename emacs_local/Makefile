.SUFFIXES: .elc .el
.el.elc:	
	emacs -batch -f batch-byte-compile $*.el

all:	*.elc

clean:
	rm -f *~ .*~ #*
