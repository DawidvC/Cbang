# Testing BIT indexed access

PROG=test
OBJ= test.o meaninglessMaskTest.o

test: ${OBJ}
	${CC} ${OBJ} ${LDFLAGS} -o $@

meaninglessMaskTest.h: meaninglessMaskTest.c
test.o: meaninglessMaskTest.h

clean: genclean
	rm -f meaninglessMaskTest.h meaninglessMaskTest.c meaninglessMaskTest.o

include ../check.mk

# END
