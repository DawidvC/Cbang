# Testing the 'as' construct

PROG=testAsName
OBJ= asname.o cbind.o

${PROG}: ${OBJ}
	${CC} ${OBJ} ${LDFLAGS} -o $@

cbind.c: cbind.cb
cbind.o: cbind.c
asname.o: cbind.c

clean: genclean
	rm -f cbind.c cbind.h

include ../check.mk

#END
