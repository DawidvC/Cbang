" Vim syntax file
" Heavily based on C syntax file
" Language:	C!
" Maintainer:	Nicolas Hureau <kalenz@lse.epita.fr>
" Last Change:	2011 May 06

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword	cbangStatement		goto break return continue asm this import
syn keyword	cbangLabel		case default
syn keyword	cbangConditional	if else switch
syn keyword	cbangRepeat		while for do
syn keyword	cbangStructure		struct class enum typedef union macro packed
syn keyword	cbangType		int float void

syn keyword	cbangConstant		NULL
syn keyword	cbangTodo		contained TODO FIXME XXX

syn keyword	cbangOperator		sizeof

" Match types after colon
syn region	cbangTypeRegion		start=":" skip="\v\\\\|\\" end="\v\)|\=|,|$" contains=cbangTypeName,cbangTypeSize
syn match	cbangTypeName		contained "\v\w+"
syn match	cbangTypeSizeInt	contained "\v\d+"
syn match	cbangTypeSize		contained "\v\<\+?\d+\>" contains=cbangTypeSizeInt

" Function/method call/def
"syn match	cbangFunctionProto	"\v\i\I*\s*\(.*\)" contains=cbangFunctionName
"syn match	cbangFunctionName	contained "\v\i\I*\("

" It's easy to accidentally add a space after a backslash that was intended
" for line continuation.  Some compilers allow it, which makes it
" unpredicatable and should be avoided.
syn match	cbangBadContinuation	contained "\v\\\s+$"

" cCommentGroup allows adding matches for special things in comments
syn cluster	cbangCommentGroup	contains=cbangTodo,cbangBadContinuation

" String and Character constants
" Highlight special characters (those which have a backslash) differently
syn match	cbangSpecial		display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
" UTF
syn match	cbangSpecial		display contained "\\\(u\x\{4}\|U\x\{8}\)"
" ISO C99 string
syn match	cbangFormat		display "%\(\d\+\$\)\=[-+' #0*]*\(\d*\|\*\|\*\d\+\$\)\(\.\(\d*\|\*\|\*\d\+\$\)\)\=\([hlLjzt]\|ll\|hh\)\=\([aAbdiuoxXDOUfFeEgGcCsSpn]\|\[\^\=.[^]]*\]\)" contained
syn match	cbangFormat		display "%%" contained
syn region	cbangString		start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=cbangSpecial,cbangFormat,@Spell

syn match	cbangCharacter		"L\='[^\\]'"
syn match	cbangCharacter		"L'[^']*'" contains=cbangSpecial
syn match	cbangSpecialError	"L\='\\[^'\"?\\abfnrtv]'"
syn match	cbangSpecialCharacter	"L\='\\['\"?\\abfnrtv]'"
syn match	cbangSpecialCharacter	display "L\='\\\o\{1,3}'"
syn match	cbangSpecialCharacter	display "'\\x\x\{1,2}'"
syn match	cbangSpecialCharacter	display "L'\\x\x\+'"

" highlight trailing white space
syn match	cbangSpaceError		display " \+\t"me=e-1
syn match	cbangSpaceError		display excludenl "\s\+$"

" This should be before cbangErrInParen to avoid problems with #define ({ xxx })
"syntax match	cbangCurlyError		"}"
syntax region	cbangBlock		start="{" end="}" contains=ALLBUT,cbangCurlyError,@cbangParenGroup,cbangErrInParen,cbangErrInBracket,@Spell fold

" catch errors caused by wrong parenthesis and brackets
" also accept <% for {, %> for }, <: for [ and :> for ] (C99)
" But avoid matching <::.
syn cluster	cbangParenGroup		contains=cbangParenError,cbangIncluded,cbangSpecial,cbangCommentSkip,cbangCommentString,cbangComment2String,@cbangCommentGroup,cbangCommentStartError,cbangUserCont,cbangUserLabel,cbangBitField,cbangOctalZero,cbangFormat,cbangNumber,cbangFloat,cbangOctal,cbangOctalError,cbangNumbersCom,cbangTypeName,cbangTypeSize,cbangTypeSizeInt
syn region	cbangParen		transparent start='(' end=')' contains=ALLBUT,@cbangParenGroup,cbangErrInBracket,@Spell
"syn match	cbangParenError		display "[\])]"
"syn match	cbangErrInParen		display contained "[\]{}]\|<%\|%>"
syn region	cbangBracket		transparent start='\[\|<::\@!' end=']\|:>' contains=ALLBUT,@cbangParenGroup,cbangErrInParen,@Spell
"syn match	cbangErrInBracket	display contained "[);{}]\|<%\|%>"

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match	cbangNumbers		display transparent "\<\d\|\.\d" contains=cbangNumber,cbangFloat,cbangOctalError,cbangOctal
" Same, but without octal error (for comments)
syn match	cbangNumbersCom		display contained transparent "\<\d\|\.\d" contains=cbangNumber,cbangFloat,cbangOctal
syn match	cbangNumber		display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match	cbangNumber		display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match	cbangOctal		display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=cbangOctalZero
syn match	cbangOctalZero		display contained "\<0"
syn match	cbangFloat		display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match	cbangFloat		display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match	cbangFloat		display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	cbangFloat		display contained "\d\+e[-+]\=\d\+[fl]\=\>"
"hexadecimal floating point number, optional leading digits, with dot, with exponent
syn match	cbangFloat		display contained "0x\x*\.\x\+p[-+]\=\d\+[fl]\=\>"
"hexadecimal floating point number, with leading digits, optional dot, with exponent
syn match	cbangFloat		display contained "0x\x\+\.\=p[-+]\=\d\+[fl]\=\>"

" flag an octal number with wrong digits
syn match	cbangOctalError		display contained "0\o*[89]\d*"
syn case match

" A comment can contain cString, cCharacter and cNumber.
" But a "*/" inside a cString in a cComment DOES end the comment!  So we
" need to use a special type of cString: cCommentString, which also ends on
" "*/", and sees a "*" at the start of the line as comment again.
" Unfortunately this doesn't very well work for // type of comments :-(
syntax match	cbangCommentSkip	contained "^\s*\*\($\|\s\+\)"
syntax region	cbangCommentString	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+\*/+me=s-1 contains=cbangSpecial,cbangCommentSkip
syntax region	cbangComment2String	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end="$" contains=cbangSpecial
syntax region	cbangCommentL		start="//" skip="\\$" end="$" keepend contains=@cbangCommentGroup,cbangComment2String,cbangCharacter,cbangNumbersCom,cbangSpaceError,@Spell
syntax region	cbangComment		matchgroup=cbangCommentStart start="/\*" end="\*/" contains=@cbangCommentGroup,cbangCommentStartError,cbangCommentString,cbangCharacter,cbangNumbersCom,cbangSpaceError,@Spell extend

" keep a // comment separately, it terminates a preproc. conditional
syntax match	cbangCommentError	display "\*/"
syntax match	cbangCommentStartError	display "/\*"me=e-1 contained

" Highlight User Labels
syn cluster	cbangMultiGroup		contains=cbangIncluded,cbangSpecial,cbangCommentSkip,cbangCommentString,cbangComment2String,@cbangCommentGroup,cbangCommentStartError,cbangUserCont,cbangUserLabel,cbangBitField,cbangOctalZero,cbangFormat,cbangNumber,cbangFloat,cbangOctal,cbangOctalError,cbangNumbersCom,cbangTypename,cbangTypeSize,cbangTypeSizeInt
syn region	cbangMulti		transparent start='?' skip='::' end=':' contains=ALLBUT,@cbangMultiGroup,@Spell
" Avoid matching foo::bar() in C++ by requiring that the next char is not ':'
syn cluster	cbangLabelGroup	contains=cUserLabel
syn match	cbangUserCont	display "^\s*\I\i*\s*:$" contains=@cbangLabelGroup
syn match	cbangUserCont	display ";\s*\I\i*\s*:$" contains=@cbangLabelGroup
syn match	cbangUserCont	display "^\s*\I\i*\s*:[^:]"me=e-1 contains=@cbangLabelGroup
syn match	cbangUserCont	display ";\s*\I\i*\s*:[^:]"me=e-1 contains=@cbangLabelGroup

syn match	cbangUserLabel	display "\I\i*" contained

" Avoid recognizing most bitfields as labels
syn match	cbangBitField	display "^\s*\I\i*\s*:\s*[1-9]"me=e-1 contains=cbangType
syn match	cbangBitField	display ";\s*\I\i*\s*:\s*[1-9]"me=e-1 contains=cbangType

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link cbangStatement		Statement
hi def link cbangLabel			Label
hi def link cbangConditional		Conditional
hi def link cbangRepeat			Repeat
hi def link cbangStructure		Structure
hi def link cbangType			Type

hi def link cbangConstant		Constant
hi def link cbangTodo			Todo

hi def link cbangBadContinuation	cbangError

hi def link cbangTypeName		cbangType
hi def link cbangTypeSizeInt		cbangNumber

hi def link cbangFunctionName		cbangFunction

hi def link cbangSpecial		cbangSpecialChar
hi def link cbangFormat			cbangSpecial
hi def link cbangString			String
hi def link cbangSpecialError		cbangError
hi def link cbangSpecialCharacter	cbangSpecial

hi def link cbangSpaceError		cbangError

hi def link cbangNumber			Number
hi def link cbangOctal			cbangNumber
hi def link cbangOctalZero		cbangError
hi def link cbangOctalZero		cbangError
hi def link cbangFloat			Float

hi def link cbangCommentSkip		cbangComment
hi def link cbangCommentString		cbangString
hi def link cbangComment2String		cbangString
hi def link cbangCommentL		cbangComment
hi def link cbangComment		Comment
hi def link cbangCommentError		cbangError
hi def link cbangCommentStartError	cbangError

hi def link cbangCurlyError		cbangError
hi def link cbangParenError		cbangError
hi def link cbangErrInParen		cbangError
hi def link cbangErrInBracket		cbangError

hi def link cUserLabel			cbangLabel

" Misc
hi def link cbangError			Error
hi def link cbangFunction		Function

" Not yet implemented
hi def link cbangCharacter		Character
hi def link cbangOperator		Operator
hi def link cbangPreProc		PreProc
hi def link cbangMacro			Macro
hi def link cbangSpecialChar		SpecialChar
hi def link cbangPreCondit		PreCondit
hi def link cbangStorageClass		StorageClass

" Default highlighting
let b:current_syntax = "cb"

" vim: ts=8
