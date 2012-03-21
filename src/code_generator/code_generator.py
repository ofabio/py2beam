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

# a = 7
# def call_go():
#     a = range(1, 6)
#     b = 3
#
#     def go(b):
#         print b
#         print a
#
#     go(b)
#
# call_go()


# ---
# erlang
# module() ->
#     A = lists:seq(1, 5),
#     go().
# go() ->
#     io:format("~w~n", [A]).


class CodeGenerator:
    def __init__(self):
        #self.code = []
        pass
        
    def generate(self):
        output = []
        
        #tree = Fun(output, 'go', [Range(1, 6), For(output, 'l', [Print('l')])])
        #tree = Fun(output, 'go', [Range(1, 6)])
        #tree = Fun(output, 'go', [Range(1, 6), Print()])
        #tree = Fun(output, 'go', [Print()])
        #tree = Module(Class('Person', [Fun(output, 'init', [Assign('self.name', 0)])]), Call('Person.init'), Print('Person.name'))
        #tree = Module(output, [Def('go', [Print('a')]), Range(1, 6), Assign('a'), Call('go')])
        #tree = Module(output, [Def(output, 'go', [Print('a')]), Range(1, 6), Assign('a'), Call('go')])
        #tree = Module(output, [Def(output, 'go', ['b'], [Print('a'), Print('b')]), Range(1, 6), Assign('a'), Assign('b', 3), Call('go', ['b'])])
        fun_call_go = Def(output, 'call_go', [], [Range(1, 6), 
                                                  Assign('a'), 
                                                  Assign('b', 3), 
                                                  Def(output, 'go', ['b'], [Print('b'), Print('a')]),
                                                  Call('go', ['b']),
                                                  ])
        tree = Module(output, [Assign('a', 7), fun_call_go, Call('call_go', [])])
        
        #tree = Module(output, [Def(output, 'call_go', [], []), Call('call_go', [])])
        
        output.append(tree.generate())
        
        output += (self.utility_code())
        
        output = self.assign_labels(output)
        
        return output
        
    def utility_code(self):
        code = [
        [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', 'module_info'), 0]),
            ('label', []),
            ('move', [('atom', 'prova'), ('x', 0)]),
            ('call_ext_only', [1, ('extfunc', 'erlang:get_module_info/1')]),
        ],
        [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', 'module_info'), 1]),
            ('label', []),
            ('move', [('x', 0), ('x', 1)]),
            ('move', [('atom', 'prova'), ('x', 0)]),
            ('call_ext_only', [2, ('extfunc', 'erlang:get_module_info/2')]),
        ],
        [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', 'dict_list_merge'), 1]),
            ('label', []),
            ('allocate_zero', [2, 1]),
            ('move', [('x', 0), ('y', 1)]),
            ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
            ('move', [('x', 0), ('y', 0)]),
            ('make_fun2', [('prova', '-dict_list_merge/1-fun-0-/2')]),
            ('move', [('y', 1), ('x', 2)]),
            ('move', [('y', 0), ('x', 1)]),
            ('call_ext_last', [3, ('extfunc', 'lists:foldl/3'), 2]),
        ],
        [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', 'dict_merge'), 2]),
            ('label', []),
            ('allocate', [2, 2]),
            ('move', [('x', 1), ('y', 0)]),
            ('move', [('x', 0), ('y', 1)]),
            ('make_fun2', [('prova', '-dict_merge/2-fun-0-/3')]),
            ('move', [('y', 0), ('x', 2)]),
            ('move', [('y', 1), ('x', 1)]),
            ('call_ext_last', [3, ('extfunc', 'orddict:merge/3'), 2]),
        ],
        [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', '-dict_merge/2-fun-0-'), 3]),
            ('label', []),
            ('move', [('x', 2), ('x', 0)]),
            ('return', []),
        ],
        [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', '-dict_list_merge/1-fun-0-'), 2]),
            ('label', []),
            ('call_only', [2, ('prova', 'dict_merge/2')]),
        ],
        ]
        
        return code

    def assign_labels(self, output):
        """assegna ai label dei valori incrementali e contemporaneamente salva i label riferiti
        alle destinazioni delle jump. In una seconda passata assegna quei label alle jump"""
        labels = dict()
        l = 0
        for fun in output:
            for instr in fun:
                if instr[0] == 'label':
                    l += 1
                    li = instr[1]
                    if instr[1]:
                        labels[li[0]] = l
                        li.pop()
                    li.append(l)
        for fun in output:
            for instr in fun:
                params = instr[1]
                for param in params:
                    if type(param) == tuple and param[0] == 'f':
                        new_param = (param[0], labels[param[1]])
                        params[params.index(param)] = new_param
        return output

class Module:
    def __init__(self, output, body):
        self.output = output
        self.body = body
        self.locals_ = []
        #self.inherited = 
        self.def_callable = []
        self.def_contained = []
        
    def generate(self):
        body = self.body
        locals_ = self.locals_
        def_callable = self.def_callable
        def_contained = self.def_contained
        
        code = [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', 'module'), 0]),
            ('label', []),
            ('allocate', [10, 0]),  # alloco 10 y!
        ]
        
        # y0 contiene una lista di dizionari, che nel complesso contengono tutte le variabili
        # ereditate a cui può accedere un'istruzione
        # y1 contiene il merge di tutti i dizionari della lista. Questo è quello che viene
        # effettivamente usato dalle istruzioni
        # da y2 in poi sono i registri usati per salvare le variabili.
        code += [
            ('move', [('nil', None), ('y', 0)]),
            ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
            ('move', [('x', 0), ('y', 1)]),
        ]
        #context.append('___')
        
        for b in body:
            if b.__class__ in (Def,):
                def_callable.append(b)
                b.def_callable = list(def_callable)
                def_contained.append(b.name)
                
                self.output.append(b.generate())
            else:
                b.locals_ = locals_
                if b.__class__ == Call:
                    b.def_contained = def_contained
                    b.def_callable = def_callable
                    
                code += b.generate()
        
        code += [
            ('deallocate', [10]),
            ('return', []),
        ]
        return code

class Def:
    def __init__(self, output, name, params, body):
        self.output = output
        self.name = name
        self.params = params
        self.body = body
        self.locals_ = []
        self.def_contained = []
        self.parents = []
        self.def_callable = []
        
    def generate(self):
        body = self.body
        locals_ = self.locals_
        params = self.params
        def_callable = self.def_callable
        def_contained = self.def_contained
        
        code = [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', self.name), len(params) + 1]),
            ('label', []),
            ('allocate', [10, 0]),  # alloco 10 y!
        ]
        
        # memorizzo in ('y', 0) il contesto
        code += [('move', [('x', 0), ('y', 0)])]
        
        # memorizzo i parametri della funzione in ('y', 2), ...
        for p in params:
            y = len(locals_)
            locals_.append(p)
            code += [('move', [('x', params.index(p) + 1), ('y', y + 2)])]
            
        # memorizzo in ('y', 1) il dizionario unificato contenente le variabili ereditate
        code += [
            ('call', [1, ('prova', 'dict_list_merge/1')]),
            ('move', [('x', 0), ('y', 1)]),
        ]
        
        for b in body:
            if b.__class__ in (Def,):
                b.parents = self.parents + [self.name]
                def_callable.append(b)
                b.def_callable = list(def_callable)
                def_contained.append(b.name)
                
                self.output.append(b.generate())    
            else:
                b.locals_ = self.locals_
                if b.__class__ == Call:
                    b.def_contained = def_contained
                    b.def_callable = def_callable
                code += b.generate()
        
        code += [
            ('deallocate', [10]),
            ('return', []),
        ]
        return code
        
class Call:
    def __init__(self, called, params):
        self.called = called
        self.params = params
        self.locals_ = []
        self.def_contained = []
        self.def_callable = []
        
    def generate(self):
        locals_ = self.locals_
        called = self.called
        params = self.params
        def_callable = self.def_callable
        def_contained = self.def_contained
        
        #print 'def_callable'
        #print [d.name for d in self.def_callable]
        #print 'def_contained'
        #print self.def_contained 
        
        # la funziona che stai cercando di chiamare è stata definita ?
        assert called in [d.name for d in def_callable]
        
        # caso 1: se la funzione target è contenuta nel def a cui appartiene la call, 
        # aggiunge il dizionario con le variabili locali al context
        
        if self.called in def_contained:
            code = self.pack_locals()
            code += [('put_list', [('x', 0), ('y', 0), ('x', 0)])]
        
        # caso 2: se la funzione è esterna al def, chiama la funzione passandogli un
        # sottinsieme del contesto, ovvero, un numero di elementi della lista del contesto 
        # in base al level della funzione chiamata
        
        else:
            level = len([d for d in def_callable if d.name == called][0].parents)
            #print 'level %s' % level
            #print 
            code += [
                ('move', [('y', 0), ('x', 0)])
                ('move', [('integer', level + 1), ('x', 1)]),
                ('call_ext', [2, ('extfunc', 'lists:sublist/2')]),
            ]
        
        # passa i parametri
        for p in params:
            code += [
                ('move', [('y', locals_.index(p) + 2), ('x', params.index(p) + 1)])
            ]
        n_params = len(params) + 1
        code += [('call', [n_params, ('prova', '%s/%d' % (self.called, n_params))])]
        return code
            
    def pack_locals(self):
        # riempie (x, 0) con un dizionario delle variabili locali
        locals_ = self.locals_
        
        code = [('call_ext', [0, ('extfunc', 'orddict:new/0')])]
        for var in locals_:
            y = locals_.index(var) + 2
            code += [
                #orddict:store('A', A, D0)
                ('move', [('x', 0), ('x', 2)]),
                ('move', [('atom', var), ('x', 0)]),
                ('move', [('y', y), ('x', 1)]),
                ('call_ext', [3, ('extfunc', 'orddict:store/3')]),
            ]
        return code
        
class Assign:
    def __init__(self, var, value = None):
        self.var = var
        self.value = value
        self.locals_ = []
        
    def generate(self):
        locals_ = self.locals_
        var = self.var
        value = self.value
        
        try:
            y = locals_.index(var)
        except ValueError:
            y = len(locals_) + 2
            locals_.append(var)
        
        if type(value) == int:
            code = [('move', [('integer', value), ('y', y)])]
        else:
            code = [('move', [('x', 0), ('y', y)])]
        return code
    
    
class Range:
    def __init__(self, start, stop):
        self.start = start
        self.stop = stop - 1
        self.locals_ = []
    
    def generate(self):
        code = [
            ('move', [('integer', self.start), ('x', 0)]),
            ('move', [('integer', self.stop), ('x', 1)]),
        ]

        code += [('call_ext', [2, ('extfunc', 'lists:seq/2')])]
        return code

class For:
    label = 1
    name_num = 1
    
    def __init__(self, output, item, body):
        self.output = output
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
                self.output.append(b.generate())
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
        self.locals_ = []
        
    def generate(self):
        value = self.value
        locals_ = self.locals_
        
        code = []
        if value == None:
            code += [('put_list', [('x', 0), ('nil', None), ('x', 1)])]
        else:
            try:
                y = locals_.index(value) + 2
                code = [('put_list', [('y', y), ('nil', None), ('x', 1)])]
            except ValueError:
                # orddict:fetch('A', Context)
                code += [
                    ('move', [('atom', value), ('x', 0)]),
                    ('move', [('y', 1), ('x', 1)]),
                    ('call_ext', [2, ('extfunc', 'orddict:fetch/2')]),
                    ('put_list', [('x', 0), ('nil', None), ('x', 1)])
                ]
        
        code += [
            ('move', [('literal', None), 0]),
            ('int_code_end', []),
            ('call_ext', [2, ('extfunc', 'io:format/2')]),
        ]
        return code
        
    def from_context(self):
        pass

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

    atoms = ['prova', 'go', 'lists', 'seq', 'for1', 'io', 'format', 'ok', 'erlang',
             'module', 'module_info', 'get_module_info', 'a', 'b','orddict', 'fetch', 'new', 'store', 'call_go',
             'dict_list_merge', 'lists', 'foldl', 'dict_merge', 'merge', '-dict_merge/2-fun-0-',
             '-dict_list_merge/1-fun-0-']
    imports = ['lists:seq/2', 'io:format/2', 'orddict:fetch/2', 'orddict:new/0', 'orddict:store/3',
               'lists:foldl/3', 'orddict:merge/3', 'erlang:get_module_info/1', 'erlang:get_module_info/2']
    exports = ['module_info/1', 'module_info/0', 'module/0']
    locals_ = ['-dict_list_merge/1-fun-0-/2', '-dict_merge/2-fun-0-/3', 'dict_merge/2',
               'dict_list_merge/1', 'call_go/1'] #'go/2',
    
    #locals_ = ['-dict_list_merge/1-fun-0-/2', '-dict_merge/2-fun-0-/3', 'dict_merge/2', 
    #           'dict_list_merge/1']
    
    cg = CodeGenerator()
    code = cg.generate()
    for i in code:
        for i2 in i:
            print '%s,' % (i2,)
        print
        
    te = BeamEncoder(atoms, code, imports, exports, literals, locals_, attributes, compile_info)
    te.make_binary()
    te.write('prova.beam')
    
def test():
    attributes = ExternalTerm(List(Tuple(Atom('vsn'), List(Big(10355834843103504983582285655618377565L)))))
    compile_info = ExternalTerm(List(Tuple(Atom('options'),Nil()),Tuple(Atom('version'),String('4.8')),Tuple(Atom('time'),Tuple(Integer(2012),Integer(3),Integer(8),Integer(21),Integer(1),Integer(28))),Tuple(Atom('source'),String('/Users/fabio/Programmazione/ErlangWorld/beam exploration/code/8/prova.erl'))))
    #literals = [ExternalTerm(String('~w~n')), ExternalTerm(Integer(0)), ExternalTerm(Integer(10))]
    literals = []

    atoms = ['prova2', 'module', 'orddict', 'new', 'A', 'store', 'B', 'dict_list_merge', 
             'lists', 'foldl', 'dict_merge', 'merge', 'module_info', 'erlang',
             'get_module_info', '-dict_merge/2-fun-0-', '-dict_list_merge/1-fun-0-']
    imports = ['orddict:new/0', 'orddict:store/3', 'lists:foldl/3', 
                         'orddict:merge/3', 'erlang:get_module_info/1', 
                         'erlang:get_module_info/2']
    exports = ['module_info/1', 'module_info/0', 'module/0']
    # {labeled_locals,[{'-dict_list_merge/1-fun-0-',2,14},
    #                 {'-dict_merge/2-fun-0-',3,12},
    #                 {dict_list_merge,1,4},
    #                 {dict_merge,2,6}]},
    
    locals_ = ['-dict_list_merge/1-fun-0-/2', '-dict_merge/2-fun-0-/3', 'dict_merge/2', 
               'dict_list_merge/1']
    
    code = [
    [
        ('label', [1]),
        ('line', [1]),
        ('func_info', [('atom', 'prova2'), ('atom', 'module'), 0]),
        ('label', [2]),
        ('allocate_zero', [1, 0]),
        ('line', [2]),
        ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
        ('move', [('integer', 3), ('x', 1)]),
        ('move', [('x', 0), ('x', 2)]),
        ('move', [('atom', 'A'), ('x', 0)]),
        ('line', [3]),
        ('call_ext', [3, ('extfunc', 'orddict:store/3')]),
        ('move', [('integer', 5), ('x', 1)]),
        ('move', [('x', 0), ('x', 2)]),
        ('move', [('atom', 'B'), ('x', 0)]),
        ('line', [4]),
        ('call_ext', [3, ('extfunc', 'orddict:store/3')]),
        ('move', [('x', 0), ('y', 0)]),
        ('line', [5]),
        ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
        ('move', [('integer', 10), ('x', 1)]),
        ('move', [('x', 0), ('x', 2)]),
        ('move', [('atom', 'B'), ('x', 0)]),
        ('line', [6]),
        ('call_ext', [3, ('extfunc', 'orddict:store/3')]),
        ('test_heap', [4, 1]),
        ('put_list', [('x', 0), ('nil', None), ('x', 1)]),
        ('put_list', [('y', 0), ('x', 1), ('x', 0)]),
        ('call_last', [1, ('prova2', 'dict_list_merge/1'), 1]),
    ],
    [
        ('label', [3]),
        ('line', [7]),
        ('func_info', [('atom', 'prova2'), ('atom', 'dict_list_merge'), 1]),
        ('label', [4]),
        ('allocate_zero', [2, 1]),
        ('move', [('x', 0), ('y', 1)]),
        ('line', [8]),
        ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
        ('move', [('x', 0), ('y', 0)]),
        ('make_fun2', [('prova2', '-dict_list_merge/1-fun-0-/2')]),
        ('move', [('y', 1), ('x', 2)]),
        ('move', [('y', 0), ('x', 1)]),
        ('line', [8]),
        ('call_ext_last', [3, ('extfunc', 'lists:foldl/3'), 2]),
    ],
    [
        ('label', [5]),
        ('line', [9]),
        ('func_info', [('atom', 'prova2'), ('atom', 'dict_merge'), 2]),
        ('label', [6]),
        ('allocate', [2, 2]),
        ('move', [('x', 1), ('y', 0)]),
        ('move', [('x', 0), ('y', 1)]),
        ('make_fun2', [('prova2', '-dict_merge/2-fun-0-/3')]),
        ('move', [('y', 0), ('x', 2)]),
        ('move', [('y', 1), ('x', 1)]),
        ('line', [10]),
        ('call_ext_last', [3, ('extfunc', 'orddict:merge/3'), 2]),
    ],
    [
        ('label', [7]),
        ('line', [0]),
        ('func_info', [('atom', 'prova2'), ('atom', 'module_info'), 0]),
        ('label', [8]),
        ('move', [('atom', 'prova2'), ('x', 0)]),
        ('line', [0]),
        ('call_ext_only', [1, ('extfunc', 'erlang:get_module_info/1')]),
    ],
    [
        ('label', [9]),
        ('line', [0]),
        ('func_info', [('atom', 'prova2'), ('atom', 'module_info'), 1]),
        ('label', [10]),
        ('move', [('x', 0), ('x', 1)]),
        ('move', [('atom', 'prova2'), ('x', 0)]),
        ('line', [0]),
        ('call_ext_only', [2, ('extfunc', 'erlang:get_module_info/2')]),
    ],
    [
        ('label', [11]),
        ('line', [10]),
        ('func_info', [('atom', 'prova2'), ('atom', '-dict_merge/2-fun-0-'), 3]),
        ('label', [12]),
        ('move', [('x', 2), ('x', 0)]),
        ('return', []),
    ],
    [
        ('label', [13]),
        ('line', [8]),
        ('func_info', [('atom', 'prova2'), ('atom', '-dict_list_merge/1-fun-0-'), 2]),
        ('label', [14]),
        ('call_only', [2, ('prova2', 'dict_merge/2')]),
    ],
    ]
    
    te = BeamEncoder(atoms, code, imports, exports, literals, locals_, attributes, compile_info)
    te.make_binary()
    te.write('prova2.beam')
    
    

if __name__ == '__main__':
    ciao_like()
    #test()