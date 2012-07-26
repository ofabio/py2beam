#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
scanner.py
"""

from ply import lex
from ply.lex import TOKEN

# List of reserved tokens.
reserved = {
   'if' : 'IF',
   'elif' : 'ELIF',
   'else' : 'ELSE',
   'while' : 'WHILE',
   'for' : 'FOR',
   'in' : 'IN',
   'and' : 'AND',
   'del' : 'DEL',
   'from' : 'FROM',
   'not' : 'NOT',
   'as' : 'AS',
   'global' : 'GLOBAL',
   'or' : 'OR',
   'with' : 'WITH',
   'assert' : 'ASSERT',
   'pass' : 'PASS',
   'yield' : 'YIELD',
   'break' : 'BREAK',
   'except' : 'EXCEPT',
   'import' : 'IMPORT',
   'print' : 'PRINT',
   'class' : 'CLASS',
   'exec' : 'EXEC',
   'raise' : 'RAISE',
   'continue' : 'CONTINUE',
   'finally' : 'FINALLY',
   'is' : 'IS',
   'return' : 'RETURN',
   'def' : 'DEF',
   'lambda' : 'LAMBDA',
   'try' : 'TRY',
}

# List of token names.   This is always required
tokens = [#operators
          'PLUS',
          'MINUS',
          'MULT',
          'POWER',
          'DIVIDE',
          'FLOORDIVIDE',
          'REMAINDER',
          'LEFTSHIFT',
          'RIGHTSHIFT',
          'BITWISEAND',
          'BITWISEOR',
          'BITWISEXOR',
          'NOTBITS',
          'LESSTHAN',
          'MORETHAN',
          'LESSEQTHAN',
          'MOREEQTHAN',
          'EQUAL',
          'NOTEQUAL',
          #delimiters
          'LPAREN',
          'RPAREN',
          'LSQUAREBKT',
          'RSQUAREBKT',
          'LCURLYBKT',
          'RCURLYBKT',
          'AT',
          'COMMA',
          'COLON',
          'PERIOD',
          #'ELLIPSIS',
          'BACKTICK',
          'ASSIGN',
          'SEMICOLON',
          #operators & delimiters
          'SELFPLUS',
          'SELFMINUS',
          'SELFMULT',
          'SELFPOWER',
          'SELFDIVIDE',
          'SELFFLOORDIVIDE',
          'SELFREMAINDER',
          'SELFLEFTSHIFT',
          'SELFRIGHTSHIFT',
          'SELFBITWISEAND',
          'SELFBITWISEOR',
          'SELFBITWISEXOR',
          #literals
          'NUMBER',
          # 'INTLONGLITERAL',
          # 'FLOATLITERAL',
          'STRING',

          'NAME',
          'NEWLINE',
          'WS',
          'INDENT',
          'DEDENT',
          'ENDMARKER',
          ] + list(reserved.values())

digit            = '[0-9]'
uppercase        = '[A-Z]'
lowercase        = '[a-z]'
letter           = lowercase + '|' + uppercase
identifier       = "(" + letter + "|_)(" + letter + "|" + digit + "|_)*"

exponent        = "(e|E)[+|-]" + digit + "+"
fraction        = "\." + digit + "+"
intpart         = digit + "+"
pointfloat      = intpart + fraction + "|" + intpart + "\."
exponentfloat   = "(" + intpart + "|" + pointfloat + ")" + exponent
floatnumber     = "(" + pointfloat + ")|(" + exponentfloat + ")"
#    floatnumber     = "([0-9]+[.][0-9]+)"
#floatnumber     = "([0-9]*\.[0-9]+)"   #"[0-9]+\.[0-9]+|[0-9]+.|([0-9]+|[[0-9]+].[0-9]+|[0-9]+.)(e|E)[+|-][0-9]+"
#floatnumber     = "[0-9]+\.[0-9]+|[0-9]+\.|([0-9]+|[0-9]+\.[0-9]+|[0-9]+\.)(e|E)[+|-][0-9]+"

# escapeseq           = "\\" + letter
# longstringchar      = "[^\\\\]"
# shortstringchar     = r"[^\\n']"
# longstringitem      = longstringchar + "|" + escapeseq
# shortstringitem     = shortstringchar + "|" + escapeseq
# longstring          = "'''" + longstringitem + "*'''|\"\"\"" + longstringitem + "*\"\"\""
# shortstring         = "'" + shortstringitem + "*'|\"" + shortstringitem + "*\""
# stringprefix        = "r|u|ur|R|U|UR|Ur|uR|b|B|br|Br|bR|BR"
# stringliteral       = "[" + stringprefix + "]" + "(" + shortstring + "|" + longstring + ")"
#stringliteral       = r"('[^\\n']|\*'|\"[^\n\']|\*\"|'''[^\]|\*'''|\"\"\"[^\]|\*\"\"\")"
# stringliteral       = "'([^\\\n']|\\[a-zA-Z0-9\\])*'|\"([^\\\n']|\\[a-zA-Z0-9\\])*\""
stringliteral = (r"""
[uU]?[rR]?
  (?:              # Single-quote (') strings
  '''(?:                 # Triple-quoted can contain...
      [^'\\]             | # a non-quote, non-backslash
      \\.                | # a backslash followed by something
      '{1,2}(?!')          # one or two quotes
    )*''' |
  '(?:                   # Non-triple quoted can contain...
     [^'\n\\]            | # non-quote, non-backslash, non-NL
     \\.                   # a backslash followed by something
  )*' | """+
r'''               # Double-quote (") strings
  """(?:                 # Triple-quoted can contain...
      [^"\\]             | # a non-quote, non-backslash
      \\.                | # a backslash followed by something
      "{1,2}(?!")          # one or two quotes
    )*""" |
  "(?:                   # Non-triple quoted can contain...
     [^"\n\\]            | # non-quote, non-backslash, non-NL
     \\.                   # a backslash followed by something
  )*"
)''')

# stringliteral = (r"""
# [uU]?[rR]?
#   (?:              # Single-quote (') strings
#   '(?:                   # Non-triple quoted can contain...
#      [^'\n\\]            | # non-quote, non-backslash, non-NL
#      \\.                   # a backslash followed by something
#   )*' | """+
# r'''               # Double-quote (") strings
#   "(?:                   # Non-triple quoted can contain...
#      [^"\n\\]            | # non-quote, non-backslash, non-NL
#      \\.                   # a backslash followed by something
#   )*"
# )''')


hexdigit        = r"\d|[a-f]|[A-F]"
bindigit        = r"0|1"
octdigit        = r"[0-7]"
nonzerodigit    = r"[1-9]"
bininteger      = r"0(b|B)" + bindigit + r"+"
hexinteger      = r"0(x|X)" + hexdigit + r"+"
octinteger      = r"0(o|O)" + octdigit + r"+|0" + octdigit + r"+"
decimalinteger  = nonzerodigit + digit + r"*|0"
integer         = decimalinteger + r"|" + octinteger + r"|" + hexinteger + r"|" + bininteger
longinteger     = integer + r"(l|L)"

def t_NUMBER(t):
    r"""(\d+(\.\d*)?|\.\d+)([eE][-+]? \d+)?"""
    t.value = eval(t.value)
    return t

@TOKEN(stringliteral)
def t_STRING(t):
    # r"'([^\\']+|\\'|\\\\)*'"
    t.value = t.value[1:-1].decode("string-escape")
    return t

# @TOKEN(integer)
# def t_INTLONGLITERAL(t):
#     t.value = eval(t.value)
#     return t
#
# @TOKEN(floatnumber)
# def t_FLOATLITERAL(t):
#     t.value = float(t.value)
#     return t
#
# def t_IMGLITERAL(t):
#     '''
#     imagnumber ::=  (floatnumber | intpart) ("j" | "J")
#     '''
#     pass

# Regular expression rules for simple tokens
''' Operators
+       -       *       **      /       //      %
<<      >>      &       |       ^       ~
<       >       <=      >=      ==      !=      <>
'''

t_LSQUAREBKT    = r'\['
t_RSQUAREBKT    = r'\]'
t_LCURLYBKT     = r'{'
t_RCURLYBKT     = r'}'
t_AT            = r'@'
t_COMMA         = r','
t_COLON         = r':'
t_PERIOD        = r'\.'
t_BACKTICK      = r'`'
#t_ELLIPSIS      = r'...'
t_ASSIGN        = r'='
t_SEMICOLON     = r';'


t_PLUS          = r'\+'
t_MINUS         = r'-'
t_MULT          = r'\*'
t_POWER         = r'\*\*'
t_DIVIDE        = r'/'
t_FLOORDIVIDE   = r'//'
t_REMAINDER     = r'%'
t_LEFTSHIFT     = r'<<'
t_RIGHTSHIFT    = r'>>'
t_BITWISEAND    = r'&'
t_BITWISEOR     = r'\|'
t_BITWISEXOR    = r'\^'
t_NOTBITS       = r'~'
t_LESSTHAN      = r'<'
t_MORETHAN      = r'>'
t_LESSEQTHAN    = r'<='
t_MOREEQTHAN    = r'>='
t_EQUAL         = r'=='
t_NOTEQUAL      = r'\!=|<>'

""" Delimiters
(       )       [       ]       {       }      @
,       :       .       `       =       ;
+=      -=      *=      /=      //=     %=
&=      |=      ^=      >>=     <<=     **=
"""

t_SELFPLUS          = r'\+='
t_SELFMINUS         = r'-='
t_SELFMULT         = r'\*='
t_SELFPOWER         = r'\*\*='
t_SELFDIVIDE        = r'/='
t_SELFFLOORDIVIDE   = r'//='
t_SELFREMAINDER     = r'%='
t_SELFLEFTSHIFT     = r'<<='
t_SELFRIGHTSHIFT    = r'>>='
t_SELFBITWISEAND    = r'&='
t_SELFBITWISEOR     = r'\|='
t_SELFBITWISEXOR    = r'\^='


# t_NEWLINE           = r'\\n'
# t_ENDMARKER         = r'EOF'

# @TOKEN(identifier)
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    # Look up symbol table information and return a tuple
    # t.value = (t.value, symbol_lookup(t.value))
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

# Putting this before t_WS let it consume lines with only comments in
# them so the latter code never sees the WS part.  Not consuming the
# newline.  Needed for "if 1: #comment"
def t_comment(t):
    r"[ ]*\043[^\n]*"  # \043 is '#'
    pass


# Whitespace
def t_WS(t):
    r' [ ]+ '
    if t.lexer.at_line_start and t.lexer.paren_count == 0:
        return t

# Don't generate newline tokens when inside of parenthesis, eg
#   a = (1,
#        2, 3)
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    t.type = "NEWLINE"
    if t.lexer.paren_count == 0:
        return t

def t_LPAREN(t):
    r'\('
    t.lexer.paren_count += 1
    return t

def t_RPAREN(t):
    r'\)'
    # check for underflow?  should be the job of the parser
    t.lexer.paren_count -= 1
    return t

def t_error(t):
    raise SyntaxError("Unknown symbol %r" % (t.value[0],))
    print "Skipping", repr(t.value[0])
    t.lexer.skip(1)

# Python's syntax has three INDENT states
#  0) no colon hence no need to indent
#  1) "if 1: go()" - simple statements have a COLON but no need for an indent
#  2) "if 1:\n  go()" - complex statements have a COLON NEWLINE and must indent
NO_INDENT = 0
MAY_INDENT = 1
MUST_INDENT = 2

# only care about whitespace at the start of a line
def track_tokens_filter(lexer, tokens):
    lexer.at_line_start = at_line_start = True
    indent = NO_INDENT
    saw_colon = False
    for token in tokens:
        token.at_line_start = at_line_start

        if token.type == "COLON":
            at_line_start = False
            indent = MAY_INDENT
            token.must_indent = False

        elif token.type == "NEWLINE":
            at_line_start = True
            if indent == MAY_INDENT:
                indent = MUST_INDENT
            token.must_indent = False

        elif token.type == "WS":
            assert token.at_line_start == True
            at_line_start = True
            token.must_indent = False

        else:
            # A real token; only indent after COLON NEWLINE
            if indent == MUST_INDENT:
                token.must_indent = True
            else:
                token.must_indent = False
            at_line_start = False
            indent = NO_INDENT

        yield token
        lexer.at_line_start = at_line_start

def _new_token(type, lineno):
    tok = lex.LexToken()
    tok.type = type
    tok.value = None
    tok.lineno = lineno
    tok.lexpos = 0
    return tok

# Synthesize a DEDENT tag
def DEDENT(lineno):
    return _new_token("DEDENT", lineno)

# Synthesize an INDENT tag
def INDENT(lineno):
    return _new_token("INDENT", lineno)

# Track the indentation level and emit the right INDENT / DEDENT events.
def indentation_filter(tokens):
    # A stack of indentation levels; will never pop item 0
    levels = [0]
    token = None
    depth = 0
    prev_was_ws = False
    for token in tokens:
##        if 1:
##            print "Process", token,
##            if token.at_line_start:
##                print "at_line_start",
##            if token.must_indent:
##                print "must_indent",
##            print

        # WS only occurs at the start of the line
        # There may be WS followed by NEWLINE so
        # only track the depth here.  Don't indent/dedent
        # until there's something real.
        if token.type == "WS":
            assert depth == 0
            depth = len(token.value)
            prev_was_ws = True
            # WS tokens are never passed to the parser
            continue

        if token.type == "NEWLINE":
            depth = 0
            if prev_was_ws or token.at_line_start:
                # ignore blank lines
                continue
            # pass the other cases on through
            yield token
            continue

        # then it must be a real token (not WS, not NEWLINE)
        # which can affect the indentation level

        prev_was_ws = False
        if token.must_indent:
            # The current depth must be larger than the previous level
            if not (depth > levels[-1]):
                raise IndentationError("expected an indented block")

            levels.append(depth)
            yield INDENT(token.lineno)

        elif token.at_line_start:
            # Must be on the same level or one of the previous levels
            if depth == levels[-1]:
                # At the same level
                pass
            elif depth > levels[-1]:
                raise IndentationError("indentation increase but not in new block")
            else:
                # Back up; but only if it matches a previous level
                try:
                    i = levels.index(depth)
                except ValueError:
                    raise IndentationError("inconsistent indentation")
                for _ in range(i+1, len(levels)):
                    yield DEDENT(token.lineno)
                    levels.pop()

        yield token

    ### Finished processing ###

    # Must dedent any remaining levels
    if len(levels) > 1:
        assert token is not None
        for _ in range(1, len(levels)):
            yield DEDENT(token.lineno)

# The top-level filter adds an ENDMARKER, if requested.
# Python's grammar uses it.
def filter(lexer, add_endmarker = True):
    token = None
    tokens = iter(lexer.token, None)
    tokens = track_tokens_filter(lexer, tokens)
    for token in indentation_filter(tokens):
        yield token

    if add_endmarker:
        lineno = 1
        if token is not None:
            lineno = token.lineno
        yield _new_token("ENDMARKER", lineno)

class PyScanner(object):
    def __init__(self, debug=0, optimize=0, lextab='lextab', reflags=0):
        self.lexer = lex.lex(debug=debug, optimize=optimize, lextab=lextab, reflags=reflags)
        self.token_stream = None
    def input(self, s, add_endmarker=True):
        self.lexer.paren_count = 0
        self.lexer.input(s)
        self.token_stream = filter(self.lexer, add_endmarker)
    def token(self):
        try:
            return self.token_stream.next()
        except StopIteration:
            return None

if __name__ == '__main__':
    scanner = PyScanner()
    code =r'''
def factorial(n):
    if n == 0:
        return 1
    elif n > 0:
        f = 1
        for i in xrange(1, n+1):
            f *= i
        return f
    else:
        return None'''
    print code
    scanner.input(code)
    while True:
        tok = scanner.token()
        if not tok: break
        print tok



