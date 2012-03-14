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
        #self.code = []
        pass
        
    def generate(self):
        module = []
        
        #tree = Fun(module, 'go', [Range(1, 6), For(module, 'l', [Print('l')])])
        #tree = Fun(module, 'go', [Range(1, 6)])
        #tree = Fun(module, 'go', [Range(1, 6), Print()])
        tree = Fun(module, 'go', [Print()])
        
        module.append(tree.generate())
        
        module = self.assign_labels(module)
        
        return module

    def assign_labels(self, module):
        """assegna ai label dei valori incrementali e contemporaneamente salva i label riferiti
        alle destinazioni delle jump. In una seconda passata assegna quei label alle jump"""
        labels = dict()
        l = 0
        for fun in module:
            for instr in fun:
                if instr[0] == 'label':
                    l += 1
                    li = instr[1]
                    if instr[1]:
                        labels[li[0]] = l
                        li.pop()
                    li.append(l)
        for fun in module:
            for instr in fun:
                params = instr[1]
                for param in params:
                    if type(param) == tuple and param[0] == 'f':
                        new_param = (param[0], labels[param[1]])
                        params[params.index(param)] = new_param
        return module

class Class:
    pass

class Fun:
    def __init__(self, module, name, body):
        self.module = module
        self.name = name
        self.body = body
        self.context = []
    
    def generate(self):
        # eredita il contesto
        context = list(self.context)
        
        code = [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', self.name), 0]),
            ('label', []),
            ('allocate', [10, 0]),  # alloco 10 y!
        ]
        
        body = self.body
        for b in body:
            # dà il proprio contesto alle istruzioni contenute
            b.context = context
            is_last = body.index(b) == len(body) - 1
            if b.__class__ == For:
                self.module.append(b.generate())
                if is_last:
                    code += [('call_last', [1, ('prova', b.name + '/1'), 10])]
                else:
                    code += [('call_only', [1, ('prova', b.name + '/1')])]
            else:
                if is_last:
                    b.last = True
                code += b.generate()
        
        code += [
            ('return', []),
        ]
        return code
    
class Range:
    def __init__(self, start, stop):
        self.start = start
        self.stop = stop - 1
        self.last = False
        self.context = []
    
    def generate(self):
        code = [
            ('move', [('integer', self.start), ('x', 0)]),
            ('move', [('integer', self.stop), ('x', 1)]),
        ]
        if self.last:
            code += [('call_ext_last', [2, ('extfunc', 'lists:seq/2'), 10])]
        else:
            code += [('call_ext', [2, ('extfunc', 'lists:seq/2')])]
        return code

class For:
    label = 1
    name_num = 1
    
    def __init__(self, module, item, body):
        self.module = module
        self.name = "for%d" % For.name_num
        For.name_num += 1
        self.body = body
        self.item = item
        self.context = []
    
    def generate(self):
        # usa lo stesso contesto della funzione
        context = self.context
        y_item = len(context)
        context.append(self.item)
        y_tail = len(context)
        context.append('%s_%s' % (self.name, 0))
        
        code = [
            ('label', [For.label]),
            ('func_info', [('atom', 'prova'), ('atom', self.name), 1]),
            ('label', []),
            ('is_nonempty_list', [('f', For.label + 1), ('x', 0)]),
            #('allocate_heap', [2, 2, 1]),
            ('allocate', [10, 0]),
            ('get_list', [('x', 0), ('y', y_item), ('y', y_tail)]),
            ]
        
        body = self.body
        for b in body:
            b.context = context
            is_last = body.index(b) == len(body) - 1
            if b.__class__ == For:
                self.module.append(b.generate())
                if is_last:
                    code += [('call_last', [1, ('prova', b.name + '/1'), 10])]
                else:
                    code += [('call_only', [1, ('prova', b.name + '/1')])]
            else:
                code += b.generate()
                    
        code += [
            ('move', [('y', y_tail), ('x', 0)]),
            ('call_last', [1, ('prova', self.name + '/1'), 10]),
            ('label', [For.label + 1]),
            ('is_nil', [('f', For.label), ('x', 0)]),
            ('move', [('atom', 'ok'), ('x', 0)]),
            ('return', []),
            ]
        
        For.label += 2
        
        return code
        
class Print:
    def __init__(self, value=None):
        self.value = value
        self.last = False
        self.context = []
        
    def generate(self):
        if self.value == None:
            code = [('put_list', [('x', 0), ('nil', None), ('x', 1)])]
        else:
            y = self.context.index(self.value)
            code = [('put_list', [('y', y), ('nil', None), ('x', 1)])]
        
        code += [
            ('move', [('literal', None), 0]),
            ('int_code_end', []),
        ]
        if self.last:
            code += [('call_ext_last', [2, ('extfunc', 'io:format/2'), 10])]
        else:
            code += [('call_ext', [2, ('extfunc', 'io:format/2')])]
        return code
        

def ciao():  
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
        ('allocate', [2, 2]),
        ('get_list', [('x', 0), ('y', 0), ('y', 1)]),
        
        ('put_list', [('y', 0), ('nil', None), ('x', 1)]),
        ('move', [('literal', None), 0]),
        ('int_code_end', []),
        ('call_ext', [2, ('extfunc', 'io:format/2')]),
        #('put_list', [('y', 0), ('nil', None), ('x', 1)]),
        #('move', [('literal', None), 0]),
        #('int_code_end', []),
        #('call_ext', [2, ('extfunc', 'io:format/2')]),
        
        ('trim', [1, 1]),                                   # (carica in 1)?, (ne rimane uno)
        ('move', [('y', 0), ('x', 0)]),
        ('call_last', [1, ('prova', 'for/1'), 1]),
        ('label', [3]),
        ('is_nil', [('f', 1), ('x', 0)]),
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
    
def ciao_like():
    attributes = ExternalTerm(List(Tuple(Atom('vsn'), List(Big(10355834843103504983582285655618377565L)))))
    compile_info = ExternalTerm(List(Tuple(Atom('options'),Nil()),Tuple(Atom('version'),String('4.8')),Tuple(Atom('time'),Tuple(Integer(2012),Integer(3),Integer(8),Integer(21),Integer(1),Integer(28))),Tuple(Atom('source'),String('/Users/fabio/Programmazione/ErlangWorld/beam exploration/code/8/prova.erl'))))
    literals = [ExternalTerm(String('~w~n')), ExternalTerm(Integer(0)), ExternalTerm(Integer(10))]

    atoms = ['prova', 'go', 'lists', 'seq', 'for1', 'io', 'format', 'ok', 'erlang', 'module_info',
             'erlang', 'get_module_info']
    imports = ['lists:seq/2', 'io:format/2', 'erlang:get_module_info/1', 'erlang:get_module_info/2']
    exports = ['go/0']
    locals_ = [] #'for1/1'
    
    cg = CodeGenerator()
    code = cg.generate()
    for i in code:
        for i2 in i:
            print '%s,' % (i2,)
        print
        
    te = BeamEncoder(atoms, code, imports, exports, locals_, literals, attributes, compile_info)
    te.make_binary()
    te.write('prova.beam')
    
def test():
    attributes = ExternalTerm(List(Tuple(Atom('vsn'), List(Big(10355834843103504983582285655618377565L)))))
    compile_info = ExternalTerm(List(Tuple(Atom('options'),Nil()),Tuple(Atom('version'),String('4.8')),Tuple(Atom('time'),Tuple(Integer(2012),Integer(3),Integer(8),Integer(21),Integer(1),Integer(28))),Tuple(Atom('source'),String('/Users/fabio/Programmazione/ErlangWorld/beam exploration/code/8/prova.erl'))))
    literals = [ExternalTerm(String('~w~n')), ExternalTerm(Integer(0)), ExternalTerm(Integer(10))]

    atoms = ['prova', 'go', 'lists', 'seq', 'for1', 'io', 'format', 'ok', 'erlang', 'module_info',
             'erlang', 'get_module_info']
    imports = ['lists:seq/2', 'io:format/2', 'erlang:get_module_info/1', 'erlang:get_module_info/2']
    exports = ['go/0']
    locals_ = [] #'for1/1'
    
    code = [
    [
        ('label', [1]),
        ('func_info', [('atom', 'prova'), ('atom', 'go'), 0]),
        ('label', [2]),
        ('allocate', [10, 0]),
        ('put_list', [('x', 32), ('nil', None), ('x', 1)]),
        ('move', [('literal', None), 0]),
        ('int_code_end', []),
        ('call_ext_last', [2, ('extfunc', 'io:format/2'), 10]),
        ('return', []),
    ],
    ]
    
    te = BeamEncoder(atoms, code, imports, exports, locals_, literals, attributes, compile_info)
    te.make_binary()
    te.write('prova.beam')
    
    

if __name__ == '__main__':
    #ciao_like()
    test()