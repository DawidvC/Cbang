# BASE ...

CB= simple_allocator.cb page_allocator.cb recycler.cb
CBI=
CFILES=${CB:.cb=.c}
HFILES=${CB:.cb=.h} ${CBI:.cbi=.h}
OBJ=${CFILES:.c=.o}
CBC=../../../Compiler/cbc

recycler: ${OBJ}
	${CC} -o $@ ${OBJ} ${LDFLAGS}

.SUFFIXES: .cb .cbi

.cb.c:
	${CBC} ${CBCFLAGS} $<

.cbi.h:
	${CBC} ${CBCFLAGS} $<

clean::
	rm -f *~ *.o ${CFILES} ${HFILES}
	rm -f recycler

page_allocator.h: page_allocator.c
simple_allocator.h: simple_allocator.c
recycler.o: page_allocator.h simple_allocator.h

include ../../../config.mk

# END
