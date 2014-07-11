# check.mk
# Basic features probably used in other Makefiles

all: clean check

check: ${PROG}
	./${PROG} > /dev/null

CBC=../../Compiler/cbc
CBFLAGS= ${CBCFLAGS}

CFLAGS+= -Wno-pointer-sign ${CBINCLUDES}

genclean:
	rm -f *~ *.o
	rm -f ${PROG}

.SUFFIXES: .cb .cbi

.cb.c:
	${CBC} ${CBFLAGS} $<

.cbi.h:
	${CBC} ${CBFLAGS} $<

include ../../config.mk

# END
