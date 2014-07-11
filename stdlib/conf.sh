#!/bin/sh

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


SRC=$1
TARGET=$2

cat ${SRC} \
    | perl -pe "s#ARCH_WORD_SIZE#${ARCH_WORD_SIZE}#g" \
    | perl -pe "s#INT_SIZE#${INT_SIZE}#g" \
    | perl -pe "s#LONG_SIZE#${LONG_SIZE}#g" \
    | perl -pe "s#MAX_INT#${MAX_INT}#g" \
    | perl -pe "s#MIN_INT#${MIN_INT}#g" \
    | perl -pe "s#MAX_UINT#${MAX_UINT}#g" \
    | perl -pe "s#MAX_SIZE_T#${MAX_SIZE_T}#g" \
    | perl -pe "s#MAX_64INT#${MAX_64INT}#g" \
    > ${TARGET}

# END
