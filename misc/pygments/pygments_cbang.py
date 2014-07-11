# -*- coding: utf-8 -*-

import re
from string import Template

from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, \
     this, combined
from pygments.util import get_bool_opt, get_list_opt
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
     Number, Punctuation, Error, Literal
from pygments.scanner import Scanner

__all__ = ['CBangLexer']


class CBangLexer(RegexLexer):
    """
    For C! source code without any preprocessor directive.
    """
    name = 'C!'
    aliases = ['cb', 'cbang']
    filenames = ['*.cb', '*.cbi']

    #: optional Comment or Whitespace
    _ws = r'(?:\s|//.*?\n|/[*].*?[*]/)+'

    tokens = {
        'whitespace': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
        ],
        'statements': [
            (r'"', String, 'string'),
            (r"'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'0[0-7]+', Number.Oct),
            (r'0b[01]+', Number),
            (r'\d+', Number.Integer),
            (r'\*/', Error),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'[()\[\],.]', Punctuation),
            (r'\b(case)(.+?)(:)', bygroups(Keyword, using(this), Text)),
            ('(int|float)(<)(\+?[0-9]+)(>)',
                bygroups(Keyword.Type, Punctuation, Number, Punctuation)
            ),
            (r'this', Name.Builtin.Pseudo),
            (r'(auto|break|case|const|continue|default|do|else|enum|extern|'
             r'for|goto|if|register|restricted|return|sizeof|static|struct|'
             r'switch|typedef|union|volatile|virtual|while|asm|local)\b',
             Keyword),
            (r'(int|long|float|short|double|char|unsigned|signed|void)\b',
             Keyword.Type),
            (r'(_{0,2}inline|naked|restrict|thread|typename)\b', Keyword.Reserved),
            (r'__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|'
             r'declspec|finally|int64|try|leave)\b', Keyword.Reserved),
            (r'(true|false|NULL)\b', Name.Builtin),
            (r'del\b', Keyword),
            (r'([a-zA-Z_][a-zA-Z0-9_]*)(\s*::\s*)([a-zA-Z_][a-zA-Z0-9_]*)',
                bygroups(Name.Namespace, Punctuation, Name)),
            (r'([a-zA-Z_][a-zA-Z0-9_]*)(\s*:\s*)',
                bygroups(Name.Variable, Punctuation)),
            ('(#)([a-zA-Z_][a-zA-Z0-9_]*)', bygroups(Punctuation, Name.Constant)),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'function_decl': [
            # functions
            (r'(#)?(\b[a-zA-Z_][a-zA-Z0-9_]*)'          # method name
            r'(\s*\([^;]*?\)\s*(?:const)?\s*)'          # signature
             r'(?:(:)([^/{]+))?'                        # return arguments
             r'(' + _ws + r')({)',
             bygroups(
                 Punctuation, Name.Function,
                 using(this), Punctuation, using(this),
                 using(this), Punctuation
             ),
             'function'),
            # function declarations
            (r'(#)?(\b[a-zA-Z_][a-zA-Z0-9_]*)'          # method name
             r'(\s*\([^;{]*?\)\s*(?:const)?\s*)'        # signature
             r'(?:(:)([^/{]+))?'                        # return arguments
             r'(' + _ws + r')(;)',
             bygroups(
                Punctuation, Name.Function,
                using(this), Punctuation, using(this),
                using(this), Punctuation
             )),
            (r'(\bimport)(\s*)([a-zA-Z_][a-zA-Z0-9_]*)(;)',
             bygroups(
                Keyword.Namespace, Text, Name.Namespace, Punctuation
             )
            ),
        ],
        'root': [
            include('whitespace'),
            include('function_decl'),
            # (macro) class declaration
            (r'(\b(?:macro\s+)?class)'
             r'(\s+)([a-zA-Z_][a-zA-Z0-9_]*)'
             r'(?:(\s*:)([^/{]+))?'
             r'(' + _ws + r')({)',
             bygroups(
                 Keyword, Text, Name.Class,
                 Punctuation, using(this),
                 using(this), Punctuation
             ), 'class'),
            ('', Text, 'statement'),
        ],
        'statement' : [
            include('whitespace'),
            include('statements'),
            ('{', Punctuation),
            ('[};]', Punctuation, '#pop'),
        ],
        'function': [
            include('whitespace'),
            include('statements'),
            (';', Punctuation),
            ('{', Punctuation, '#push'),
            ('}', Punctuation, '#pop'),
        ],
        'class': [
            include('whitespace'),
            ('}', Punctuation, '#pop'),
            include('function_decl'),
            include('statements'),
            (';', Punctuation),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
    }

    def get_tokens_unprocessed(self, text):
        for index, token, value in (
            RegexLexer.get_tokens_unprocessed(self, text)
        ):
            if token is Name.Function and value in ('init', 'del'):
                token = Keyword.Pseudo
            yield index, token, value
