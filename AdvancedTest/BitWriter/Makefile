# Testing BIT indexed access

PROG=testIntIndexedWriting
OBJ= testIntIndexedWriting.o intIndexedWriting.o

testIntIndexedWriting: ${OBJ}
	${CC} ${OBJ} ${LDFLAGS} -o $@

intIndexedWriting.h: intIndexedWriting.c
testIntIndexedWriting.o: intIndexedWriting.h


clean: genclean
	rm -f intIndexedWriting.c intIndexedWriting.h

include ../check.mk

# END
