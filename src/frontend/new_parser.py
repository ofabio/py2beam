#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
new_parser.py
"""

import sys
import os
dirname = os.path.dirname
ROOT_FOLDER = dirname(dirname(__file__))
sys.path.append(ROOT_FOLDER)
sys.path.append("../")
import ply.yacc as yacc
from new_scanner import tokens
from code_generator.structures import *

# Start symbols for the grammar:
#       single_input is a single interactive statement;
#       file_input is a module or sequence of commands read from an input file;
#       eval_input is the input for the eval() and input() functions.
# NB: compound_stmt in single_input is followed by extra NEWLINE!
def p_single_input(p):
    """single_input : NEWLINE
                    | simple_stmt
                    | compound_stmt NEWLINE"""
    if p[1] != "\n":
        p[0] = p[1]

def p_file_input(p):
    """file_input : newline_stmt_star ENDMARKER"""
    p[0] = p[1]

def p_newline_stmt_star(p):
    """newline_stmt_star : NEWLINE newline_stmt_star
                         | stmt newline_stmt_star
                         | empty"""
    if len(p) == 3:
        if p[1] == "\n":
            p[0] = p[2]
        else:
            p[0] = p[1] + p[2]
    else:
        p[0] = p[1]

def p_eval_input(p):
    """eval_input : testlist newline_star ENDMARKER"""
    p[0] = p[1] + p[2]

def p_newline_star(p):
    """newline_star : NEWLINE newline_star
                    | empty"""
    if len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = p[1]

def p_decorator(p):
    """decorator : AT dotted_name NEWLINE
                 | AT dotted_name LPAREN RPAREN NEWLINE
                 | AT dotted_name LPAREN arglist RPAREN NEWLINE"""
    raise NotImplementedError

def p_decorators(p):
    """decorators : decorator_plus"""
    raise NotImplementedError

def p_decorator_plus(p):
    """decorator_plus : decorator
                      | decorator decorator_plus"""
    raise NotImplementedError

def p_decorated(p):
    """decorated : decorators classdef
                 | decorators funcdef"""
    raise NotImplementedError

def p_funcdef(p):
    """funcdef : DEF NAME parameters COLON suite"""
    p[0] = Def(str(p[2]), p[3], p[5])

def p_parameters(p):
    """parameters : LPAREN RPAREN
                  | LPAREN varargslist RPAREN"""
    if len(p) == 3:
        p[0] = []
    else:
        p[0] = p[2]

def p_varargslist(p):
    """varargslist : fpdef_assign_test_opt_comma_star MULT NAME
                   | fpdef_assign_test_opt_comma_star MULT NAME COMMA POWER NAME
                   | fpdef_assign_test_opt_comma_star POWER NAME
                   | fpdef assign_test_opt comma_fpdef_assign_test_opt_star comma_opt"""
    if len(p) == 5:
        if p[2] != []:
            if isinstance(p[1], Var):
                p[1] = p[1].name
            tmp = Assign(p[1], p[2])
        else:
            tmp = p[1]
        p[0] = [tmp] + p[3]
    else:
        raise NotImplementedError

def p_comma_fpdef_assign_test_opt_star(p):
    """comma_fpdef_assign_test_opt_star : COMMA fpdef assign_test_opt comma_fpdef_assign_test_opt_star
                                        | empty"""
    if len(p) == 5:
        if p[3] != []:
            if isinstance(p[2], Var):
                p[2] = p[2].name
            tmp = Assign(p[2], p[3])
        else:
            tmp = p[2]
        p[0] = [tmp] + p[4]
    else:
        p[0] = p[1]

def p_fpdef_assign_test_opt_comma_star(p):
    """fpdef_assign_test_opt_comma_star : fpdef assign_test_opt COMMA fpdef_assign_test_opt_comma_star
                                        | empty"""
    if len(p) == 5:
        if p[2] != []:
            if isinstance(p[1], Var):
                p[1] = p[1].name
            tmp = Assign(p[1], p[2])
        else:
            tmp = p[1]
        p[0] = [tmp] + p[4]
    else:
        p[0] = p[1]

def p_assign_test_opt(p):
    """assign_test_opt : ASSIGN test
                       | empty"""
    if len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = p[1]

def p_fpdef(p):
    """fpdef : NAME
             | LPAREN fplist RPAREN"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_fplist(p):
    """fplist : fpdef comma_fpdef_star comma_opt"""
    p[0] = [p[1]] + p[2]

def p_comma_fpdef_star(p):
    """comma_fpdef_star : COMMA fpdef comma_fpdef_star
                        | empty"""
    if len(p) == 3:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = p[1]

def p_stmt(p):
    """stmt : simple_stmt
            | compound_stmt"""
    p[0] = p[1]

def p_simple_stmt(p):
    """simple_stmt : small_stmt semicolon_small_stmt_star semicolon_opt NEWLINE"""
    if p[1] == []:
        stmt = p[1]
    else:
        stmt = [p[1]]
    if p[2] != []:
        p[0] = stmt + p[2]
    else:
        p[0] = stmt

def p_semicolon_opt(p):
    """semicolon_opt : SEMICOLON
                     | empty"""
    p[0] = p[1]

def p_semicolon_small_stmt_star(p):
    """semicolon_small_stmt_star : SEMICOLON small_stmt semicolon_small_stmt_star
                                 | empty"""
    if len(p) == 4:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = p[1]

def p_small_stmt(p):
    """small_stmt : expr_stmt
                  | print_stmt
                  | del_stmt
                  | pass_stmt
                  | flow_stmt
                  | import_stmt
                  | global_stmt
                  | exec_stmt
                  | assert_stmt"""
                  # | assign_stmt"""
    p[0] = p[1]

# def p_assign_stmt(p): # FIXME: masking standard assign production
#     """assign_stmt : NAME ASSIGN testlist"""
#                    # | power ASSIGN testlist"""
#     p[0] = Assign(str(p[1]), p[3])

def p_expr_stmt(p):
    """expr_stmt : testlist assign_yield_testlist_or_star
                 | testlist augassign yield_testlist_or"""
    if p[2] != []:
        if isinstance(p[1], Var):
            p[1] = p[1].name
        p[0] = Assign(p[1], p[2])
    else:
        p[0] = p[1]

def p_assign_yield_testlist_or_star(p):
    """assign_yield_testlist_or_star : ASSIGN yield_testlist_or assign_yield_testlist_or_star
                                     | empty"""
    if len(p) == 4: # = qualcosa = qualcosa = 5
        if p[3] != []:
            if isinstance(p[2], Var):
                p[2] = p[2].name
            p[0] = Assign(p[2], p[3])
        else:
            p[0] = p[2]
    else:
        p[0] = p[1]

def p_yield_testlist_or(p):
    """yield_testlist_or : yield_expr
                         | testlist"""
    p[0] = p[1]

def p_augassign(p):
    """augassign : SELFPLUS
                 | SELFMINUS
                 | SELFMULT
                 | SELFDIVIDE
                 | SELFREMAINDER
                 | SELFBITWISEAND
                 | SELFBITWISEOR
                 | SELFBITWISEXOR
                 | SELFLEFTSHIFT
                 | SELFRIGHTSHIFT
                 | SELFPOWER
                 | SELFFLOORDIVIDE"""
    raise NotImplementedError
    # p[0] = p[1]

# For normal assignments, additional restrictions enforced by the interpreter
def p_print_stmt(p):
    """print_stmt : PRINT
                  | PRINT test comma_test_star comma_opt
                  | PRINT RIGHTSHIFT test
                  | PRINT RIGHTSHIFT test comma_test_plus comma_opt"""
    if len(p) == 2:
        p[0] = Print("")
    elif len(p) == 5:
        if p[3] == [] and p[4] == []:
            p[0] = Print(p[2])
    elif len(p) == 4:
        raise NotImplementedError
    elif len(p) == 6:
        raise NotImplementedError

def p_comma_test_plus(p):
    """comma_test_plus : COMMA test
                       | COMMA test comma_test_plus"""
    if len(p) == 3:
        p[0] = [p[2]]
    else:
        if p[3] != []:
            p[0] = [p[2]] + p[3]

def p_del_stmt(p):
    """del_stmt : DEL exprlist"""
    pass

def p_pass_stmt(p):
    """pass_stmt : PASS"""
    p[0] = []

def p_flow_stmt(p):
    """flow_stmt : break_stmt
                 | continue_stmt
                 | return_stmt
                 | raise_stmt
                 | yield_stmt"""
    p[0] = p[1]

def p_break_stmt(p):
    """break_stmt : BREAK"""
    pass

def p_continue_stmt(p):
    """continue_stmt : CONTINUE"""
    pass

def p_return_stmt(p):
    """return_stmt : RETURN
                   | RETURN testlist"""
    if len(p) == 2:
        p[0] = Return()
    else:
        p[0] = Return(p[2])

def p_yield_stmt(p):
    """yield_stmt : yield_expr"""
    pass

def p_raise_stmt(p):
    """raise_stmt : RAISE
                  | RAISE test
                  | RAISE test COMMA test
                  | RAISE test COMMA test COMMA test"""
    pass

def p_import_stmt(p):
    """import_stmt : import_name
                   | import_from"""
    pass

def p_import_name(p):
    """import_name : IMPORT dotted_as_names"""
    pass

def p_import_from(p):
    """import_from : FROM period_or IMPORT MULT
                   | FROM period_or IMPORT LPAREN import_as_names RPAREN
                   | FROM period_or IMPORT import_as_names"""
    pass

def p_period_or(p):
    """period_or : period_star dotted_name
                 | period_plus"""
    pass

def p_period_star(p):
    """period_star : PERIOD period_star
                   | empty"""
    pass

def p_period_plus(p):
    """period_plus : PERIOD
                   | PERIOD period_plus"""
    pass

def p_import_as_name(p):
    """import_as_name : NAME
                      | NAME AS NAME"""
    pass

def p_dotted_as_name(p):
    """dotted_as_name : dotted_name
                      | dotted_name AS NAME"""
    pass

def p_import_as_names(p):
    """import_as_names : import_as_name comma_import_as_name_star comma_opt"""
    pass

def p_comma_import_as_name_star(p):
    """comma_import_as_name_star : COMMA import_as_name comma_import_as_name_star
                                 | empty"""
    pass

def p_dotted_as_names(p):
    """dotted_as_names : dotted_as_name comma_dotted_as_name_star"""
    pass

def p_comma_dotted_as_name_star(p):
    """comma_dotted_as_name_star : COMMA dotted_as_name comma_dotted_as_name_star
                                 | empty"""
    pass

def p_dotted_name(p):
    """dotted_name : NAME period_name_star"""
    pass

def p_period_name_star(p):
    """period_name_star : PERIOD NAME period_name_star
                        | empty"""
    pass

def p_global_stmt(p):
    """global_stmt : GLOBAL NAME comma_name_star"""
    pass

def p_comma_name_star(p):
    """comma_name_star : COMMA NAME comma_name_star
                       | empty"""
    pass

def p_exec_stmt(p):
    """exec_stmt : EXEC expr
                 | EXEC expr IN test
                 | EXEC expr IN test COMMA test"""
    pass

def p_assert_stmt(p):
    """assert_stmt : ASSERT test
                   | ASSERT test COMMA test"""
    pass

def p_compound_stmt(p):
    """compound_stmt : if_stmt
                     | while_stmt
                     | for_stmt
                     | try_stmt
                     | with_stmt
                     | funcdef
                     | classdef
                     | decorated"""
    p[0] = [p[1]]

def p_if_stmt(p):
    """if_stmt : IF test COLON suite elif_test_colon_suite_star else_colon_suite_opt"""
    p[0] = If([p[2]], [p[4], p[6]])

def p_elif_test_colon_suite_star(p):
    """elif_test_colon_suite_star : ELIF test COLON suite elif_test_colon_suite_star
                                  | empty"""
    if len(p) == 2:
        p[0] = None
    else:
        raise NotImplementedError

def p_while_stmt(p):
    """while_stmt : WHILE test COLON suite
                  | WHILE test COLON suite ELSE COLON suite"""
    pass

def p_for_stmt(p):
    """for_stmt : FOR exprlist IN testlist COLON suite
                | FOR exprlist IN testlist COLON suite ELSE COLON suite"""
    if len(p) == 7:
        if isinstance(p[2], Var):
            p[2] = p[2].name
        p[0] = For(p[2], p[4], p[6])
    else:
        raise NotImplementedError

# def p_for_stmt(p): # FIXME: modified to accept only NAME instead of exprlist
#     """for_stmt : FOR NAME IN testlist COLON suite
#                 | FOR NAME IN testlist COLON suite ELSE COLON suite"""
#     if len(p) == 7:
#         p[0] = For(str(p[2]), p[4], p[6])
#     else:
#         raise NotImplementedError

def p_try_stmt(p):
    """try_stmt : TRY COLON suite except_clause_colon_suite_plus else_colon_suite_opt finally_colon_suite_opt
                | TRY COLON suite FINALLY COLON suite"""
    raise NotImplementedError

def p_finally_colon_suite_opt(p):
    """finally_colon_suite_opt : FINALLY COLON suite
                               | empty"""
    pass

def p_else_colon_suite_opt(p):
    """else_colon_suite_opt : ELSE COLON suite
                            | empty"""
    if len(p) == 4:
        p[0] = p[3]
    else:
        p[0] = None

def p_except_clause_colon_suite_plus(p):
    """except_clause_colon_suite_plus : except_clause COLON suite
                                      | except_clause COLON suite except_clause_colon_suite_plus"""
    pass

def p_with_stmt(p):
    """with_stmt : WITH with_item comma_with_item_star  COLON suite"""
    pass

def p_comma_with_item_star(p):
    """comma_with_item_star : COMMA with_item comma_with_item_star
                            | empty"""
    pass

def p_with_item(p):
    """with_item : test
                 | test AS expr"""
    pass

def p_except_clause(p):
    """except_clause : EXCEPT
                     | EXCEPT test
                     | EXCEPT test AS test
                     | EXCEPT test COMMA test"""
    pass

def p_suite(p):
    """suite : simple_stmt
             | NEWLINE INDENT stmt_plus DEDENT"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[3]

def p_stmt_plus(p):
    """stmt_plus : stmt
                 | stmt stmt_plus"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2]

# Backward compatibility cruft to support:
# [ x for x in lambda: True, lambda: False if x() ]
# even while also allowing:
# lambda x: 5 if x else 2
# (But not a mix of the two)
def p_testlist_safe(p):
    """testlist_safe : old_test
                     | old_test comma_old_test_plus comma_opt"""
    pass

def p_comma_old_test_plus(p):
    """comma_old_test_plus : COMMA old_test
                           | COMMA old_test comma_old_test_plus"""
    pass

def p_old_test(p):
    """old_test : or_test
                | old_lambdef"""
    p[0] = p[1]

def p_old_lambdef(p):
    """old_lambdef : LAMBDA COLON old_test
                   | LAMBDA varargslist COLON old_test"""
    if len(p) == 4:
        raise NotImplementedError
        # p[0] = Lambda(None, p[3])
    else:
        raise NotImplementedError
        # p[0] = Lambda(p[2], p[4])

def p_test(p):
    """test : or_test
            | or_test IF or_test ELSE test
            | lambdef"""
    if len(p) == 2: # or_test, lambdef
        p[0] = p[1]
    else:
        raise NotImplementedError

def p_or_test(p):
    """or_test : and_test or_and_test_star"""
    if p[2] != []:
        raise NotImplementedError
        # p[0] = Or(p[1], p[2])
    else:
        p[0] = p[1]

def p_or_and_test_star(p):
    """or_and_test_star : OR and_test or_and_test_star
                        | empty"""
    if len(p) == 4: 
        if p[3] != []: # OR qualcosa OR qualcosa OR qualcosa
            raise NotImplementedError
            # p[0] = Or(p[2], p[3])
        else:
            p[0] = p[2]
    else:
        p[0] = p[1]

def p_and_test(p):
    """and_test : not_test and_not_test_star"""
    if p[2] != []:
        raise NotImplementedError
        # p[0] = And(p[1], p[2])
    else:
        p[0] = p[1]

def p_and_not_test_star(p):
    """and_not_test_star : AND not_test and_not_test_star
                         | empty"""
    if len(p) == 4:
        if p[3] != []: # AND qualcosa AND qualcosa AND qualcosa
            raise NotImplementedError
            # p[0] = And(p[2], p[3])
        else:
            p[0] = p[2]
    else:
        p[0] = p[1]

def p_not_test(p):
    """not_test : NOT not_test
                | comparison"""
    if len(p) == 3:
        raise NotImplementedError
        # p[0] = Not(p[2])
    else:
        p[0] = p[1]

# a < b < c should be a < b and b < c
def p_comparison(p):
    """comparison : expr comp_op_expr_star"""
    if p[2] != []:
        p[0] = p[2][0](p[1], p[2][1])
        # raise NotImplementedError
        # p[0] = Comp(p[1], p[2]) # Comp(e1, [op, e2])
    else:
        p[0] = p[1]

def p_comp_op_expr_star(p):
    """comp_op_expr_star : comp_op expr comp_op_expr_star
                         | empty"""
    if len(p) == 4:
        if p[3] != []:
            raise NotImplementedError
            # p[0] = [p[1], p[3][0](p[2], p[3][1])]
        else:
            p[0] = [p[1], p[2]]
    else:
        p[0] = p[1]

def p_comp_op(p):
    """comp_op : LESSTHAN
               | MORETHAN
               | EQUAL
               | MOREEQTHAN
               | LESSEQTHAN
               | NOTEQUAL
               | IN
               | NOT IN
               | IS
               | IS NOT"""
    if len(p) == 2:
        if p[1] == ">":
            p[0] = Gt
        elif p[1] == "<":
            p[0] = Lt
        elif p[1] == "==":
            p[0] = Eq
        else:
            raise NotImplementedError
    else:
        raise NotImplementedError
        # p[0] = "%s %s" % (p[1], p[2])

def p_expr(p):
    """expr : xor_expr bitwiseor_xor_expr_star"""
    if p[2] != []:
        raise NotImplementedError
        # p[0] = Xor(p[1], p[2])
    else:
        p[0] = p[1]

def p_bitwiseor_xor_expr_star(p):
    """bitwiseor_xor_expr_star : BITWISEOR xor_expr bitwiseor_xor_expr_star
                               | empty"""
    if len(p) == 4:
        raise NotImplementedError
    else:
        p[0] = p[1]

def p_xor_expr(p):
    """xor_expr : and_expr bitwisexor_and_expr_star"""
    if p[2] != []:
        raise NotImplementedError
    else:
        p[0] = p[1]

def p_bitwisexor_and_expr_star(p):
    """bitwisexor_and_expr_star : BITWISEXOR and_expr bitwisexor_and_expr_star
                                | empty"""
    if len(p) == 4:
        raise NotImplementedError
    else:
        p[0] = p[1]

def p_and_expr(p):
    """and_expr : shift_expr bitwiseand_shift_expr_star"""
    if p[2] != []:
        raise NotImplementedError
    else:
        p[0] = p[1]

def p_bitwiseand_shift_expr_star(p):
    """bitwiseand_shift_expr_star : BITWISEAND shift_expr bitwiseand_shift_expr_star
                                  | empty"""
    if len(p) == 4:
        raise NotImplementedError
    else:
        p[0] = p[1]

def p_shift_expr(p):
    """shift_expr : arith_expr leftshift_rightshift_arith_expr_star"""
    if p[2] != []:
        raise NotImplementedError
    else:
        p[0] = p[1]

def p_leftshift_rightshift_arith_expr_star(p):
    """leftshift_rightshift_arith_expr_star : LEFTSHIFT arith_expr leftshift_rightshift_arith_expr_star
                                            | RIGHTSHIFT arith_expr leftshift_rightshift_arith_expr_star
                                            | empty"""
    if len(p) == 4:
        raise NotImplementedError
    else:
        p[0] = p[1]

def p_arith_expr(p):
    """arith_expr : term plus_minus_term_star"""
    if p[2] != []:
        p[0] = p[2][0](p[1], p[2][1])
    else:
        p[0] = p[1]

def p_plus_minus_term_star(p):
    """plus_minus_term_star : PLUS term plus_minus_term_star
                            | MINUS term plus_minus_term_star
                            | empty"""
    if len(p) == 4:
        if p[1] == '+':
            if p[3] != []:
                p[0] = [Add, p[3][0](p[2], p[3][1])]
            else:
                p[0] = [Add, p[2]]
        else:
            if p[3] != []:
                p[0] = [Sub, p[3][0](p[2], p[3][1])]
            else:
                p[0] = [Sub, p[2]]
    else:
        p[0] = p[1]

def p_term(p):
    """term : factor mult_divide_remainder_floordivide_factor_star"""
    if p[2] != []:
        # p[0] = p[2][0](p[1], p[2][1])
        prev = p[1]
        for fa in p[2]:
            # print "inside:", tr[0], prev, tr[1]
            prev = fa[0](prev, fa[1])
        p[0] = prev
        
    else:
        p[0] = p[1]

def p_mult_divide_remainder_floordivide_factor_star(p):
    """mult_divide_remainder_floordivide_factor_star : MULT factor mult_divide_remainder_floordivide_factor_star
                                                     | DIVIDE factor mult_divide_remainder_floordivide_factor_star
                                                     | REMAINDER factor mult_divide_remainder_floordivide_factor_star
                                                     | FLOORDIVIDE factor mult_divide_remainder_floordivide_factor_star
                                                     | empty"""
    if len(p) == 4:
        if p[1] == '*':
            p[0] = [[Mul, p[2]]] + p[3]
            # if p[3] != []:
            #     # p[0] = [Mul, p[3][0](p[2], p[3][1])]
            # else:
            #     p[0] = [[Mul, p[2]]]
        elif p[1] == '/':
            p[0] = [[Div, p[2]]] + p[3]
            # if p[3] != []:
            #     # p[0] = [Div, p[3][0](p[2], p[3][1])]
            # else:
            #     p[0] = [[Div, p[2]]]
        else:
            raise NotImplementedError
    else:
        p[0] = p[1]

# unary arithmetic and bitwise operations
def p_factor(p):
    """factor : PLUS factor
              | MINUS factor
              | NOTBITS factor
              | power"""
    if len(p) == 3:
        raise NotImplementedError
        # p[0] = FactOp(p[1], p[2])
    else:
        p[0] = p[1]

# ex: 2**3 = 8 (right side holds precedence)
def p_power(p):
    """power : atom trailer_star power_factor_opt"""
    if p[2] == [] and p[3] == []:
        p[0] = p[1]
    elif p[2] != [] and p[3] == []:
        # print "Power:", p[2]
        # p[0] = p[2]
        # prev = "p[1]"
        prev = p[1]
        for tr in p[2]:
            # prev = "%s(%s, %s)" % (tr[0], prev, tr[1])
            # print "inside:", tr[0], prev, tr[1]
            prev = tr[0](prev, tr[1])
        # print "Power:", prev
        # p[0] = eval(prev)
        p[0] = prev
    else:
        raise NotImplementedError

def p_trailer_star(p):
    """trailer_star : trailer trailer_star
                    | empty"""
    if len(p) == 3:
        p[0] = [p[1]] + p[2]
        # print "Trailer star:", p[1], p[2]
        # p[0] = p[1]
    else:
        p[0] = p[1]

def p_power_factor_opt(p):
    """power_factor_opt : POWER factor
                        | empty"""
    if len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = p[1]

def p_atom(p):
    """atom : NUMBER
            | string_plus
            | LPAREN RPAREN
            | LSQUAREBKT RSQUAREBKT
            | LCURLYBKT RCURLYBKT
            | LPAREN yield_expr RPAREN
            | LPAREN testlist_comp RPAREN
            | LSQUAREBKT listmaker RSQUAREBKT
            | LCURLYBKT dictorsetmaker RCURLYBKT
            | BACKTICK testlist BACKTICK"""
            # | BACKTICK testlist1 BACKTICK"""
    if len(p) == 2:
        if isinstance(p[1], int):
            p[0] = Int(p[1])
        elif isinstance(p[1], float):
            raise NotImplementedError
            # p[0] = Float(p[1])
        elif isinstance(p[1], complex):
            raise NotImplementedError
            # p[0] = Complex(p[1])
        elif isinstance(p[1], basestring):
            p[0] = Str(p[1])
    elif len(p) == 3:
        if p[1] == "(":
            raise NotImplementedError
            # p[0] = Tuple()
        elif p[1] == "[":
            raise NotImplementedError
            # p[0] = List()
        elif p[1] == "{":
            raise NotImplementedError
            # p[0] = Dict()
    else:
        if p[1] == "(": # FIXME: use Tuple class instead
            p[0] = p[2]
        elif p[1] == "[":
            raise NotImplementedError
            # p[0] = List(p[2])
        elif p[1] == "{":
            raise NotImplementedError
            # p[0] = Dict(p[2])

def p_atom_name(p):
    """atom : NAME"""
    p[0] = Var(p[1])

def p_string_plus(p):
    """string_plus : STRING
                   | STRING string_plus"""
    if len(p) == 2:
        p[0] = str(p[1])
    else:
        p[0] = str(p[1]) + p[2]

def p_listmaker(p):
    """listmaker : test list_for
                 | test comma_test_star comma_opt"""
    pass

def p_testlist_comp(p):
    """testlist_comp : test comp_for
                     | test comma_test_star comma_opt"""
    if len(p) == 3:
        raise NotImplementedError
    else:
        if p[2] != []:
            p[0] = [p[1]] + p[2]
        else:
            p[0] = p[1]


def p_lambdef(p):
    """lambdef : LAMBDA varargslist_opt COLON test"""
    raise NotImplementedError

def p_varargslist_opt(p):
    """varargslist_opt : varargslist
                       | empty"""
    pass

def p_trailer(p):
    """trailer : PERIOD NAME
               | LPAREN arglist_opt RPAREN
               | LSQUAREBKT subscriptlist RSQUAREBKT"""
               # | LPAREN power_comma_star RPAREN"""
    if len(p) == 3:
        # print "Trailer:", p[2].__class__
        # if isinstance(p[-1], basestring):
        #     print "Dot(Var('%s'), '%s')" % (p[-1], p[2])
        #     p[0] = Dot(Var(str(p[-1])), str(p[2]))
        # else:
        #     print "Dot(%s, '%s')" % (p[-1], p[2])
        #     p[0] = Dot(p[-1], str(p[2]))
        # p[0] = ["Dot", "'%s'" % p[2]]
        # if not isinstance(p[2], list):
        #     p[2] = [p[2]]
        p[0] = [Dot, str(p[2])]
    else:
        if p[1] == '(':
            # print "Trailer:", p[2]
            # print "Call(%s, %s)" % (p[-1], p[2])
            # p[0] = Call(p[-1], p[2])
            # p[0] = ["Call", p[2]]
            if not isinstance(p[2], list):
                p[2] = [p[2]]
            p[0] = [Call, p[2]]
        else:
            raise NotImplementedError

# def p_power_comma_star(p):
#     """power_comma_star : power
#                         | power COMMA power_comma_star
#                         | empty"""
#     if len(p) == 2:
#         p[0] = [p[1]]
#     if len(p) == 4:
#         p[0] = [p[1]] + [p[3]]
#     else:
#         p[0] = p[1]

def p_arglist_opt(p): # FIXME: modified
    """arglist_opt : argument
                   | argument COMMA arglist_opt
                   | empty"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[3] != []:
            p[0] = [p[1]] + [p[3]]
        else:
            p[0] = [p[1]]

# def p_arglist_opt(p):
#     """arglist_opt : arglist
#                    | empty"""
#     p[0] = p[1]

def p_subscriptlist(p):
    """subscriptlist : subscript comma_subscript_star comma_opt"""
    pass

def p_comma_subscript_star(p):
    """comma_subscript_star : COMMA subscript comma_subscript_star
                            | empty"""
    pass

def p_subscript(p):
    """subscript : PERIOD PERIOD PERIOD
                 | test
                 | test_opt COLON test_opt sliceop_opt"""
    pass

def p_test_opt(p):
    """test_opt : test
                | empty"""
    p[0] = p[1]

def p_sliceop_opt(p):
    """sliceop_opt : sliceop
                   | empty"""
    pass

def p_sliceop(p):
    """sliceop : COLON
               | COLON test"""
    pass

def p_exprlist(p):
    """exprlist : expr comma_expr_star comma_opt"""
    if p[2] != []:
        raise NotImplementedError
    else:
        p[0] = p[1]

def p_comma_expr_star(p):
    """comma_expr_star : COMMA expr comma_expr_star
                       | empty"""
    if len(p) == 4:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = p[1]

def p_testlist(p):
    """testlist : test comma_test_star comma_opt"""
    if len(p[2]) == 4:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = p[1]

def p_dictorsetmaker(p):
    """dictorsetmaker : test COLON test comp_for
                      | test COLON test comma_test_colon_test_star comma_opt
                      | test comp_for
                      | test comma_test_star comma_opt"""
    pass

def p_comma_test_colon_test_star(p):
    """comma_test_colon_test_star : COMMA test COLON test comma_test_colon_test_star
                                  | empty"""
    pass

def p_comma_opt(p):
    """comma_opt : COMMA
                 | empty"""
    p[0] = p[1]

def p_classdef(p):
    """classdef : CLASS NAME COLON suite
                | CLASS NAME LPAREN RPAREN COLON suite
                | CLASS NAME LPAREN testlist RPAREN COLON suite"""
    if len(p) == 5:
        p[0] = Class(str(p[2]), p[4])
    elif len(p) == 7:
        p[0] = Class(str(p[2]), p[6])
    else:
        p[0] = Class(str(p[2]), p[7], p[4].name)

def p_arglist(p):
    """arglist : argument_comma_star argument
               | argument_comma_star argument COMMA
               | argument_comma_star MULT test comma_argument_star
               | argument_comma_star MULT test comma_argument_star COMMA POWER test
               | argument_comma_star POWER test"""
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        raise NotImplementedError

def p_argument_comma_star(p):
    """argument_comma_star : argument COMMA argument_comma_star
                           | empty"""
    if len(p) == 4:
        if p[3] != []:
            p[0] = [p[1]] + p[3]
        else:
            p[0] = [p[1]]
    else:
        p[0] = p[1]

def p_comma_argument_star(p):
    """comma_argument_star : COMMA argument comma_argument_star
                           | empty"""
    pass

# The reason that keywords are test nodes instead of NAME is that using NAME
# results in an ambiguity. ast.c makes sure it's a NAME.
def p_argument(p):
    """argument : test
                | test comp_for
                | test ASSIGN test"""
    if len(p) == 4:
        p[0] = Assign(p[1], p[3])
    elif len(p) == 2:
        p[0] = p[1]

def p_list_iter(p):
    """list_iter : list_for
                 | list_if"""
    pass

def p_list_for(p):
    """list_for : FOR exprlist IN testlist_safe
                | FOR exprlist IN testlist_safe list_iter"""
    pass

def p_list_if(p):
    """list_if : IF old_test
               | IF old_test list_iter"""
    pass

def p_comp_iter(p):
    """comp_iter : comp_for
                 | comp_if"""
    p[0] = p[1]

def p_comp_for(p):
    """comp_for : FOR exprlist IN or_test
                | FOR exprlist IN or_test comp_iter"""
    if len(p) == 5:
        p[0] = For(p[2], p[4])
    else:
        p[0] = For(p[2], p[4], p[5])

def p_comp_if(p):
    """comp_if : IF old_test
               | IF old_test comp_iter"""
    if len(p) == 3:
        p[0] = If(p[2])
    else:
        p[0] = If(p[2], p[3])

# def p_testlist1(p):
#     """testlist1 : test comma_test_star"""
#     p[0] = [p[1]] + p[2]

def p_comma_test_star(p):
    """comma_test_star : COMMA test comma_test_star
                       | empty"""
    if len(p) == 4:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = p[1]

# not used in grammar, but may appear in "node" passed from Parser to Compiler
def p_encoding_decl(p):
    """encoding_decl : NAME"""
    raise NotImplementedError

def p_yield_expr(p):
    """yield_expr : YIELD
                  | YIELD testlist"""
    raise NotImplementedError

def p_empty(p):
    """empty : """
    p[0] = []

def p_error(p):
    if p.value == ")":
        print "Mistery Error"
    else:
        raise SyntaxError(p)

class PyParser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.parser = yacc.yacc(start="file_input", outputdir=dirname(__file__), debug=0)

    def parse(self, source):
        self.lexer.input(source)
        result = self.parser.parse(lexer=self.lexer, debug=0)
        tree = Module(result)
        print "Result:", tree
        return tree

from new_scanner import PyScanner
if __name__ == '__main__':
    scanner = PyScanner()
    code = """ 
a = 6
print a > 7
# if a > 7:
#     print ">7"
# else:
#     print "<7"
#     print "else!"
"""
# For('i', Range(Int(0), Int(2)), [Print(Var('i'))])
    print code
    scanner.input(code)
    while True:
        tok = scanner.token()
        if not tok: break
        print tok
    parser = PyParser(lexer=scanner)
    parser.parse(code)

