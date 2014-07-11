# Testing the 'as' construct

PROG= interface_cmp
OBJ= interface_cmp.o

${PROG}: ${OBJ}
	${CC} ${OBJ} ${LDFLAGS} -o $@

interface_cmp.c: interface_cmp.cb

clean: genclean
	rm -f interface_cmp.c interface_cmp.h

include ../check.mk

#END
