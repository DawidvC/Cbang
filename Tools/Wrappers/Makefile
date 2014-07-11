# Copyright (c) 2010-2012, Marwan Burelle, the LSE Team and contributors
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#   * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#   * Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#   * Neither the name of the <organization> nor the
#     names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL MARWAN BURELLE BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

# Makefile build test for wrappers

CBC=../../Compiler/cbc
CBFLAGS= ${CBCFLAGS}

WRAPPERS= libc_alloc.cbi libc_assert.cbi libc_minimal_IO.cbi
HWRAPPERS= ${WRAPPERS:.cbi=.h}

TESTFILES= malloc-test.cb assert-test.cb minimal_IO-test.cb
CFILES= ${TESTFILES:.cb=.c}
HFILES= ${TESTFILES:.cb=.h} ${HWRAPPERS}
TESTPRG= ${TESTFILES:.cb=}

all: ${HWRAPPERS}

tests: ${TESTPRG}

assert-test: assert-test.o
	${CC} -o $@ assert-test.o ${LDFLAGS}

malloc-test: malloc-test.o
	${CC} -o $@ malloc-test.o ${LDFLAGS}

minimal_IO-test: minimal_IO-test.o
	${CC} -o $@ minimal_IO-test.o ${LDFLAGS}

minimal_IO-test.h: minimal_IO-test.c
minimal_IO-test.o: minimal_IO-test.c minimal_IO-test.h libc_minimal_IO.h

malloc-test.h: malloc-test.c
malloc-test.o: malloc-test.c malloc-test.h libc_alloc.h

assert-test.h: assert-test.c
assert-test.o: assert-test.c assert-test.h libc_assert.h

.SUFFIXES: .cb .cbi

.cb.c:
	${CBC} ${CBFLAGS} $<

.cbi.h:
	${CBC} ${CBFLAGS} $<

clean::
	rm -f *~ *.o

distclean:: clean
	rm -f ${CFILES} ${HFILES} ${TESTPRG}

install: all
	mkdir -p ${PREFIX}/lib/cbang
	chmod 755 ${PREFIX}/lib/cbang
	cp -f ${WRAPPERS} ${HWRAPPERS} ${PREFIX}/lib/cbang

desinstall::
	cd ${PREFIX}/lib/cbang && rm -f ${WRAPPERS} ${HWRAPPERS}

include ../../config.mk

# END
