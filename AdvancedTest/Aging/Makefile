# Testing aging algorithm

PROG=testAging
OBJ= testAging.o aging.o

${PROG}: ${OBJ}
	${CC} ${OBJ} ${LDFLAGS} -o $@

aging.c: aging.cb
aging.o: aging.c allocbind.h
testAging.o: aging.c allocbind.h

clean: genclean
	rm -f aging.c aging.h allocbind.h allocbind.c

include ../check.mk

# END
