#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
parser.py
"""

import sys
import os
dirname = os.path.dirname
ROOT_FOLDER = dirname(dirname(__file__))
sys.path.append(ROOT_FOLDER)

import ply.yacc as yacc
from scanner import tokens
from code_generator.structures import *

## NB: compound_stmt in single_input is followed by extra NEWLINE!
# file_input: (NEWLINE | stmt)* ENDMARKER
def p_file_input_end(p):
    """file_input_end : file_input ENDMARKER"""
    p[0] = p[1]

def p_file_input(p):
    """file_input : file_input NEWLINE
                  | file_input stmt
                  | NEWLINE
                  | stmt"""
    if isinstance(p[:-1], basestring):
        if len(p) == 3:
            p[0] = p[1]
        else:
            p[0] = [] # p == 2 --> only a blank line
    else:
        if len(p) == 3:
            p[0] = p[1] + p[2]
        else:
            p[0] = p[1]

# funcdef: [decorators] 'def' NAME parameters ':' suite
#FIXME: ignoring decorators
def p_funcdef(p):
    "funcdef : DEF NAME parameters COLON suite"
    p[0] = [Def(p[2], p[3], p[5])]

# parameters: '(' [varargslist] ')'
def p_parameters(p):
    """parameters : LPAREN RPAREN
                  | LPAREN varargslist RPAREN"""
    if len(p) == 3:
        p[0] = []
    else:
        p[0] = p[2]

# varargslist: (fpdef ['=' test] ',')* ('*' NAME [',' '**' NAME] | '**' NAME) |
#FIXME: highly simplified
def p_varargslist(p):
    """varargslist : varargslist COMMA NAME
                   | NAME"""
    if len(p) == 4:
        p[0] = p[1] + p[3]
    else:
        p[0] = [p[1]]

# stmt: simple_stmt | compound_stmt
def p_stmt_simple(p):
    """stmt : simple_stmt"""
    # simple_stmt is a list
    p[0] = p[1]

def p_stmt_compound(p):
    """stmt : compound_stmt"""
    p[0] = p[1]

# simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
def p_simple_stmt(p):
    """simple_stmt : small_stmts NEWLINE
                   | small_stmts SEMICOLON NEWLINE"""
    p[0] = p[1]

def p_small_stmts(p):
    """small_stmts : small_stmts SEMICOLON small_stmt
                   | small_stmt"""
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# small_stmt: expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt |
#    import_stmt | global_stmt | exec_stmt | assert_stmt
def p_small_stmt(p):
    """small_stmt : flow_stmt
                  | expr_stmt
                  | print_stmt"""
    p[0] = p[1]

# NEW
def p_print_stmt(p):
    """print_stmt : PRINT testlist
                  | PRINT """
    print p[2]
    if len(p) == 3:
        p[0] = Print(p[2])
    else:
        p[0] = Print() #FIXME

# expr_stmt: testlist (augassign (yield_expr|testlist) |
#                      ('=' (yield_expr|testlist))*)
# augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' |
#             '<<=' | '>>=' | '**=' | '//=')

def p_expr_stmt(p):
    """expr_stmt : testlist ASSIGN testlist
                 | testlist """
    if len(p) == 2:
        # a list of expressions
        p[0] = p[1]
    else:
        p[0] = Assign(p[1], p[3])

def p_flow_stmt(p):
    "flow_stmt : return_stmt"
    p[0] = p[1]

# return_stmt: 'return' [testlist]
def p_return_stmt(p):
    "return_stmt : RETURN testlist"
    pass
    #p[0] = "return %s" % str(p[2])


def p_compound_stmt(p):
    """compound_stmt : if_stmt
                     | funcdef"""
    p[0] = p[1]

def p_if_stmt(p):
    'if_stmt : IF test COLON suite'
    pass
    #p[0] = "if %s: %s" % (p[2], p[4])

def p_suite(p):
    """suite : simple_stmt
             | NEWLINE INDENT stmts DEDENT"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[3]


def p_stmts(p):
    """stmts : stmts stmt
             | stmt"""
    if len(p) == 3:
        # print p[0], p[1], p[2]
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]

## No using Python's approach because Ply supports precedence

# comparison: expr (comp_op expr)*
# arith_expr: term (('+'|'-') term)*
# term: factor (('*'|'/'|'%'|'//') factor)*
# factor: ('+'|'-'|'~') factor | power
# comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'


# def make_lt_compare((left, right)):
#     return ast.Compare(left, [('<', right),])
# def make_gt_compare((left, right)):
#     return ast.Compare(left, [('>', right),])
# def make_eq_compare((left, right)):
#     return ast.Compare(left, [('==', right),])

# binary_ops = {
#     "+": ast.Add,
#     "-": ast.Sub,
#     "*": ast.Mul,
#     "/": ast.Div,
#     "<": make_lt_compare,
#     ">": make_gt_compare,
#     "==": make_eq_compare,
# }
# unary_ops = {
#     "+": ast.UnaryAdd,
#     "-": ast.UnarySub,
#     }

precedence = (
    ("left", "EQUAL", "MORETHAN", "LESSTHAN"),
    ("left", "PLUS", "MINUS"),
    ("left", "MULT", "DIVIDE"),
    )

def p_comparison(p):
    """comparison : comparison PLUS comparison
                  | comparison MINUS comparison
                  | comparison MULT comparison
                  | comparison DIVIDE comparison
                  | comparison LESSTHAN comparison
                  | comparison EQUAL comparison
                  | comparison MORETHAN comparison
                  | PLUS comparison
                  | MINUS comparison
                  | power"""
    if len(p) == 4:
        pass
        #p[0] = "%s %s %s" % (p[1], p[2], p[3])
    elif len(p) == 3:
        pass
        #p[0] = "%s %s" % (p[1], p[2])
    else:
        p[0] = p[1]

# power: atom trailer* ['**' factor]
# trailers enables function calls. FIXME: I only allow one level of calls
# so this is 'trailer'
def p_power(p):
    """power : atom
             | atom trailer"""
    if len(p) == 2: # power : atom
        p[0] = p[1]
    else:           # power : atom trailer
        if p[2][0] == "CALL":
            p[0] = Call(p[1], p[2][1])
        else:
            raise AssertionError("not implemented")

def p_atom_name(p):
    """atom : NAME"""
    p[0] = [Var(p[1])]

def p_atom_number(p):
    """atom : NUMBER
            | STRINGLITERAL"""
    p[0] = p[1]

def p_atom_tuple(p):
    """atom : LPAREN testlist RPAREN"""
    p[0] = p[2]

# trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
def p_trailer(p):
    """trailer : LPAREN arglist RPAREN
               | LPAREN RPAREN"""
    if len(p) == 3:
        p[0] = ("CALL", [])
    else:
        p[0] = ("CALL", p[2])

# testlist: test (',' test)* [',']
# Contains shift/reduce error
def p_testlist(p):
    """testlist : testlist_multi COMMA
                | testlist_multi """
    print "testlist:", p[1]
    if len(p) == 2:
        p[0] = p[1]
    else:
        # May need to promote singleton to tuple
        if isinstance(p[1], list):
            p[0] = p[1]
        else:
            p[0] = [p[1]]
    # Convert into a tuple?
    # if isinstance(p[0], list):
        # p[0] = tuple(p[0])

def p_testlist_multi(p):
    """testlist_multi : testlist_multi COMMA test
                      | test"""
    if len(p) == 2:
        # singleton
        p[0] = p[1]
    else:
        if isinstance(p[1], list):
            p[0] = p[1] + [p[3]]
        else:
            # singleton -> tuple
            p[0] = [p[1], p[3]]

# test: or_test ['if' or_test 'else' test] | lambdef
# FIXME: as I don't support 'and', 'or', and 'not' this works down to 'comparison'
def p_test(p):
    "test : comparison"
    p[0] = p[1]

# arglist: (argument ',')* (argument [',']| '*' test [',' '**' test] | '**' test)
# FIXME: this doesn't allow the trailing comma
def p_arglist(p):
    """arglist : arglist COMMA argument
               | argument"""
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# argument: test [gen_for] | test '=' test  # Really [keyword '='] test
def p_argument(p):
    "argument : test"
    p[0] = p[1]

def p_error(p):
    raise SyntaxError(p)

class PyParser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.parser = yacc.yacc(start="file_input_end", outputdir=dirname(__file__), debug=0)

    def parse(self, source):
        self.lexer.input(source)
        result = self.parser.parse(lexer=self.lexer, debug=0)
        tree = Module(result)
        return tree

from scanner import PyScanner
if __name__ == '__main__':
    scanner = PyScanner()
    code = r"""
def factorial(n):
    if n == 0:
        return 1
    if n > 0:
        return factorial(n-1) * n
    if n < 0:
        return None
"""
    code = r"""
def factorial(n):
    if n == 0:
        return 1
    if n > 0:
        f = 1
        f = i * f
        return f
    if n < 0:
        return None
"""
    code = r"""
def factorial(n):
    print n
"""
    code = r"""
a = 4
def call_go(a):
    print a
    #def go(b):
        #print b
    #go(b)
call_go(a)
"""
    print code
    scanner.input(code)
    while True:
        tok = scanner.token()
        if not tok: break
        # print tok
    parser = PyParser(lexer=scanner)
    parser.parse(code)





