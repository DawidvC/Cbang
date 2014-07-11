# Testing BIT indexed access

PROG=testIntIndexedReading
OBJ= testIntIndexedReading.o intIndexedReading.o

testIntIndexedReading: ${OBJ}
	${CC} ${OBJ} ${LDFLAGS} -o $@

intIndexedReading.h: intIndexedReading.c
testIntIndexedReading.o: intIndexedReading.h


clean: genclean
	rm -f intIndexedReading.c intIndexedReading.h

include ../check.mk

# END
