#!/usr/bin/env python
# encoding: utf-8
"""
code_generator.py

Created by Fabio Pricoco on 2012-03-11.
Copyright (c) 2012 ofabio. All rights reserved.
"""

import sys
import os
dirname = os.path.dirname
P2B_FOLDER = dirname(dirname(__file__))
sys.path.append(P2B_FOLDER)
from beam.assembler.beam_encoder import *

# python
# def go():
#     a = range(1, 6)
#     for(l in a):
#         print l
# ---
# erlang
# go() ->
#     A = lists:seq(1, 5),
#     for(A).

# for([]) -> ok;
# for([H|T]) ->
#     io:format("~w~n", [H]),
#     for(T).


class CodeGenerator:
    def __init__(self):
        self.code = []
        
    def do(self):
        code = []
        code.append(
            self.open_fun()
            self.range_()
            self.open_for()
            self.print_()
            self.close_for()
            self.close_fun()
        
    def fun(self, body):
        fun_ = [
            ('label', [4]),
            ('func_info', [('atom', 'prova'), ('atom', 'go'), 0]),
            ('label', [5])
        ]
            
        for instr in body:
            fun_ += self.instr()
            
        fun_ += [
            ('call_last', [1, ('prova', 'for/1'), 0]),
            ('return', []),
        ]
        return fun_
        
    def range_(self):
        range_ = [
            ('move', [('integer', 5), ('x', 1)]),
            ('move', [('integer', 1), ('x', 0)]),
            ('allocate', [0, 0]),
            ('call_ext', [2, ('extfunc', 'lists:seq/2')]),
        }
    
    def for_(self, body):
        for_ = [
            ('label', [1]),
            ('func_info', [('atom', 'prova'), ('atom', 'for'), 1]),
            ('label', [2]),
            ('is_nonempty_list', [('f', 3), ('x', 0)]),
            ('allocate_heap', [2, 2, 1]),
            ('get_list', [('x', 0), ('y', 0), ('y', 1)]),
            ]
            
        for instr in body:
            for_ += self.instr()
                
        for_ += [
            ('trim', [1, 1]),
            ('move', [('y', 0), ('x', 0)]),
            ('call_last', [1, ('prova', 'for/1'), 1]),
            ('label', [3]),
            ('is_nil', [('f', 3), ('x', 0)]),
            ('move', [('atom', 'ok'), ('x', 0)]),
            ('return', []),
            ],
            
        return for_fun
            
    def print_(self):
        print_fun = [
            ('put_list', [('y', 0), ('nil', None), ('x', 1)]),
            ('move', [('literal', None), 0]),
            ('int_code_end', []),
            ('call_ext', [2, ('extfunc', 'io:format/2')]),
        ]
        return print_fun
        

if __name__ == '__main__':
    
    attributes = ExternalTerm(List(Tuple(Atom('vsn'), List(Big(10355834843103504983582285655618377565L)))))
    compile_info = ExternalTerm(List(Tuple(Atom('options'),Nil()),Tuple(Atom('version'),String('4.8')),Tuple(Atom('time'),Tuple(Integer(2012),Integer(3),Integer(8),Integer(21),Integer(1),Integer(28))),Tuple(Atom('source'),String('/Users/fabio/Programmazione/ErlangWorld/beam exploration/code/8/prova.erl'))))
    #literals = [ExternalTerm(String('yes')), ExternalTerm(List(List(Integer(5000), Tuple(Integer(1), Integer(2), Integer(3))))), ExternalTerm(String('~w~n'))]
    literals = [ExternalTerm(String('~w~n')), ExternalTerm(Integer(0)), ExternalTerm(Integer(10))]

    atoms = ['prova', 'go', 'lists', 'seq', 'for', 'io', 'format', 'ok', 'erlang', 'module_info',
             'erlang', 'get_module_info']
    imports = ['lists:seq/2', 'io:format/2', 'erlang:get_module_info/1', 'erlang:get_module_info/2']
    exports = ['go/0']
    locals_ = []

    code = [
    [
        ('label', [1]),
        ('func_info', [('atom', 'prova'), ('atom', 'for'), 1]),
        ('label', [2]),
        ('is_nonempty_list', [('f', 3), ('x', 0)]),
        ('allocate_heap', [2, 2, 1]),
        ('get_list', [('x', 0), ('y', 0), ('y', 1)]),
        
        ('put_list', [('y', 0), ('nil', None), ('x', 1)]),
        ('move', [('literal', None), 0]),
        ('int_code_end', []),
        ('call_ext', [2, ('extfunc', 'io:format/2')]),
        #('put_list', [('y', 0), ('nil', None), ('x', 1)]),
        #('move', [('literal', None), 0]),
        #('int_code_end', []),
        #('call_ext', [2, ('extfunc', 'io:format/2')]),
        
        ('trim', [1, 1]),
        ('move', [('y', 0), ('x', 0)]),
        ('call_last', [1, ('prova', 'for/1'), 1]),
        ('label', [3]),
        ('is_nil', [('f', 3), ('x', 0)]),
        ('move', [('atom', 'ok'), ('x', 0)]),
        ('return', []),
    ],
    [
        ('label', [4]),
        ('func_info', [('atom', 'prova'), ('atom', 'go'), 0]),
        ('label', [5]),
        ('move', [('integer', 5), ('x', 1)]),
        ('move', [('integer', 1), ('x', 0)]),
        ('allocate', [0, 0]),
        ('call_ext', [2, ('extfunc', 'lists:seq/2')]),
        ('call_last', [1, ('prova', 'for/1'), 0]),
        ('return', []),
    ],
    ]

    te = BeamEncoder(atoms, code, imports, exports, locals_, literals, attributes, compile_info)
    te.make_binary()
    te.write('prova.beam')