# TEST for C! stdlib

# Global configuration

STDPATH=..
STD= ${STDPATH}/std.cbi

# CBFLAGS= -stdpath ${STDPATH}
CBFLAGS= ${CBCFLAGS}

CBC=../../Compiler/cbc

SRC= null.cb types.cb cst.cb

CSRC=${SRC:.cb=.c}
HSRC=${SRC:.cb=.h}

TESTS= ${SRC:.cb=}

all: ${TESTS}


# tools

${CBC}::
	@cd ../../Compiler && make

${STD}::
	@cd ${STDPATH} && make

clean::
	rm -f *~ *.o
	rm -f ${TESTS}

distclean:: clean
	rm -f ${CSRC} ${HSRC}

# Generic rules

.SUFFIXES: .cb .cbi

.cb.c: ${STD}
	${CBC} ${CBFLAGS} $<

.cbi.h: ${STD}
	${CBC} ${CBFLAGS} $<

include ../../config.mk

# END
