#!/usr/bin/env python
# encoding: utf-8
"""
test_asdisasm.py

Created by Fabio Pricoco on 2012-03-10.
Copyright (c) 2012 ofabio. All rights reserved.
"""

import sys
import os
sys.path.append("../..")
from beam.disassembler.beam_decoder import BeamDecoder
from beam.assembler.beam_encoder import *

def assembles():
    atoms = ['prova', 'for', 'io', 'format', 'ok', 'module_info', 'erlang', 'get_module_info']
    # deve essere mantenuto l'ordine degli import!
    imports = ['io:format/2', 'erlang:get_module_info/1', 'erlang:get_module_info/2']
    exports = ['for/1']
    locals_ = []
    #literals = [ExternalTerm(String('yes')), ExternalTerm(List(List(Integer(5000), Tuple(Integer(1), Integer(2), Integer(3))))), ExternalTerm(String('~w~n'))]
    literals = [ExternalTerm(String('~w~n'))]
    attributes = ExternalTerm(List(Tuple(Atom('vsn'), List(Big(10355834843103504983582285655618377565L)))))
    compile_info = ExternalTerm(List(Tuple(Atom('options'),Nil()),Tuple(Atom('version'),String('4.8')),Tuple(Atom('time'),Tuple(Integer(2012),Integer(3),Integer(8),Integer(21),Integer(1),Integer(28))),Tuple(Atom('source'),String('/Users/fabio/Programmazione/ErlangWorld/beam exploration/code/8/prova.erl'))))

    code = [
            [ # for/1
                ('label', [1]),
                ('func_info', [('atom', 'prova'), ('atom', 'for'), 1]),
                ('label', [2]),
                ('is_nonempty_list', [('f', 3), ('x', 0)]),
                ('allocate_heap', [1, 2, 1]),
                
                ('get_list', [('x', 0), ('x', 1), ('y', 0)]),
                
                
                #('move', [('y', 0), ('x', 1)]),                
                ('put_list', [('x', 1), ('nil', None), ('x', 1)]), # da val a [val|[]]=[val]

                ('move', [('literal', None), 0]),
                ('int_code_end', []),
                ('call_ext', [2, ('extfunc', 'io:format/2')]),
                
                #('move', [('literal', None), 0]),
                #('int_code_end', []),
                #('call_ext', [2, ('extfunc', 'io:format/2')]),
                
                
                ('move', [('y', 0), ('x', 0)]),
                ('call_last', [1, ('prova', 'for/1'), 1]),
                #('call_only', [1, ('prova', 'for/1')]),
                ('label', [3]),
                ('is_nil', [('f', 1), ('x', 0)]),
                ('move', [('atom', 'ok'), ('x', 0)]),
                ('return', []),
                #('int_code_end', []),
            ],
    ]

    te = BeamEncoder(atoms, code, imports, exports, locals_, literals, attributes, compile_info)
    te.make_binary()
    te.write('test_asdisasm.beam')

def disassembles():
    beam_decoder = BeamDecoder('test_asdisasm.beam')
    beam_decoder.decode()
    beam_decoder.print_all()


if __name__ == '__main__':
    assembles()
    print '\n'
    disassembles()