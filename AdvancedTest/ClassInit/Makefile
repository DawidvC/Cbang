# Testing BIT indexed access

PROG=test
OBJ=test.o basicObj.o

test: ${OBJ}
	${CC} ${OBJ} ${LDFLAGS} -o $@

basicObj.c: basicObj.cb
test.o: basicObj.c

clean: genclean
	rm -f basicObj.c basicObj.h

include ../check.mk

# END
