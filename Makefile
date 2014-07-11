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

# Global Makefile for C!

all: cbc stdlib/std.cbi wrappers

cbc: Compiler/cbc
	cp Compiler/cbc .

debug: cbc.debug

cbc.debug: Compiler/cbc.debug
	cp Compiler/cbc.debug cbc.debug

Compiler/cbc.debug::
	@cd Compiler && ${MAKE} debug

Compiler/cbc::
	@cd Compiler && ${MAKE}

clean::
	@cd Compiler && ${MAKE} clean
	@cd stdlib && ${MAKE} clean
	@cd Tools/Wrappers && ${MAKE} clean
	rm -f *~

distclean: clean
	@cd Tools/Wrappers && ${MAKE} distclean
	rm -f cbc cbc.debug config.mk Compiler/conf.ml
	rm -f Tools/Builds/cbang.mk

.PHONY: check wrappers

check: Compiler/cbc stdlib/std.cbi
	env CBCFLAGS=" -stdpath ${PWD}/stdlib/" ./check.py

stdlib/std.cbi: Compiler/cbc
	@cd stdlib && ${MAKE}

wrappers: cbc stdlib/std.cbi
	@cd Tools/Wrappers && ${MAKE} all

install: all
	@cd stdlib && ${MAKE} install
	@cd Tools/Wrappers && ${MAKE} install
	cp -f Tools/Builds/cbang.mk ${PREFIX}/lib/cbang
	chmod -R +r ${PREFIX}/lib/cbang
	mkdir -p ${PREFIX}/bin
	cp -f cbc ${PREFIX}/bin/
	chmod 755 ${PREFIX}/bin/cbc

desinstall:
	@cd stdlib && ${MAKE} desinstall
	@cd Tools/Wrappers && ${MAKE} desinstall
	rm -f ${PREFIX}/lib/cbang/cbang.mk
	@(rmdir ${PREFIX}/lib/cbang 2> /dev/null) \
	  || echo "${PREFIX}/lib/cbang is not empty, I'll keep it."
	rm -f ${PREFIX}/bin/cbc


include config.mk

#
# END
#
