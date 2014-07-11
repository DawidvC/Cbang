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

# Simplest Makefile for C! using ocamlbuild

BUILD=ocamlbuild.native -j 0 -no-links

DISPMETHOD=`[ -t 0 ] || echo "-quiet"`

CBC=cbc
BTARG=main

all: ${CBC}

debug: ${CBC}.debug

.PHONY: ${CBC}.debug

${CBC}: _build/${BTARG}.native
	cp -f _build/${BTARG}.native ${CBC}

_build/${BTARG}.native::
	${BUILD} ${BTARG}.native

bullshit: ${CBC}
	@echo "<-- BULSHIT ! -->"
	@echo "Because its cool to have a compiler named c!c !"
	@echo "<-- BULSHIT ! -->"
	cp -f ${CBC} "c!c"

${CBC}.debug:
	${BUILD} -tag debug -lflag -g ${BTARG}.byte
	ln -sf _build/${BTARG}.byte ${CBC}.debug)

clean::
	rm -f *~ ~.o
	rm -f ${CBC}
	rm -f "c!c"
	rm -f ${CBC}.debug
	${BUILD} -clean && echo ""

include ../config.mk

#
# END
#