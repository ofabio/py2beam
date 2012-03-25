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
heap = 7

# python
print 'output would be...'

def a():
    print 1
for i in range(0, 2):
    a()
    def a():
        print 2
    
print '---'

class CodeGenerator:
    def __init__(self):
        #self.code = []
        pass

    def generate(self):
        output = []
        
        # tree = Module(output, [
        #                        Def(output, 'a', [], [Print('t'),]), 
        #                        Assign('t', 5),
        #                        Call('a', []),
        #                        Assign('t', 2),
        #                        Call('a', []),
        #                        ])

        tree = Module(output, [
                                Def(output, 'a', [], [Print(1),]), 
                                Range(0, 2),
                                For(output, 'i', [Call('a', []), Def(output, 'a', [], [Print(2),]),]),
                                ])

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
        #self.print_all(output)
        
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
        
    def print_all(self, code):
        for i in code:
            for i2 in i:
                print '%s,' % (i2,)
            print

class Module:
    def __init__(self, output, body):
        self.output = output
        self.body = body

    def generate(self):
        body = self.body

        code = [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', 'module'), 0]),
            ('label', []),
            ('allocate', [heap, 0]),  # alloco n y!
        ]

        # y0 contiene una lista di dizionari, ciascuno dei quali rappresenta l'insieme delle
        # variabili e delle funzioni definite ad un certo livello di profondità. Nel complesso
        # y0 contiene tutti i simboli a disposizione della funzione
        code += [
            ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
            ('put_list', [('x', 0), ('nil', None), ('y', 0)]),
        ]

        for b in body:
            if b.__class__ in (Def,):
                b.ancestors = ['module']
                # prepara il riferimento alla nuova funzione: [name, deep=1].
                # deep è la profondità che darà luogo la nuova funzione, nonchè il numero
                # di insiemi di variabili da trasportare, a partire da quello di livello
                # modulo in su!
                code += [
                    ('move', [('integer', 1), ('x', 0)]),
                    ('put_list', [('x', 0), ('nil', None), ('x', 0)]),
                    ('put_list', [('atom', b.name), ('x', 0), ('x', 0)]),
                ]
                code += assign_local(b.assign_to)
                
                self.output.append(b.generate())
            elif b.__class__ in (For,):
                b.ancestors = ['module']
                # passa al for (context, variabile su cui iterare il for)
                code += [
                    ('move', [('x', 0), ('x', 1)]),
                    ('move', [('y', 0), ('x', 0)]),
                    ('call', [2, ('prova', '%s/%d' % (b.name, 2))]),
                    ('move', [('x', 0), ('y', 0)]),
                ]
            
                self.output.append(b.generate())
            else:
                code += b.generate()

        code += [
            ('move', [('atom', 'ok'), ('x', 0)]),
            ('deallocate', [heap]),
            ('return', []),
        ]
        return code

class Def:
    n_fun = 0
    
    def __init__(self, output, assign_to, params, body):
        self.output = output
        self.assign_to = assign_to
        self.params = params
        self.body = body
        self.name = 'fun__%d' % Def.n_fun
        Def.n_fun += 1
        self.ancestors = []

    def generate(self):
        body = self.body
        params = self.params

        code = [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', self.name), len(params) + 1]),
            ('label', []),
            ('allocate', [heap, 0]),
        ]

        # memorizzo in ('y', 0) il contesto a cui aggiungo un nuovo dizionario 
        # per le variabili locali
        code += [
            ('move', [('x', 0), ('y', 0)]),
            ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
            ('put_list', [('x', 0), ('y', 0), ('y', 0)]),
        ]
        

        # memorizzo i parametri della funzione a partire da ('y', START_VAR_INDEX)
        #for p in params:
        #    y = len(locals_) + START_VAR_INDEX
        #    locals_.append(p)
        #    code += [('move', [('x', params.index(p) + 1), ('y', y)])]

        for b in body:
            if b.__class__ in (Def,):
                b.ancestors = self.ancestors + [self.name]
                code += [
                    ('move', [('integer', len(b.ancestors)), ('x', 0)]),
                    ('put_list', [('x', 0), ('nil', None), ('x', 0)]),
                    ('put_list', [('atom', b.name), ('x', 0), ('x', 0)]),
                ]
                code += assign_local(b.assign_to)
                
                self.output.append(b.generate())
            elif b.__class__ in (For,):
                b.ancestors = self.ancestors + [self.name]
            
                # passa al for (context; variabile su cui iterare il for)
                code += [
                    ('move', [('x', 0), ('x', 1)]),
                    ('move', [('y', 0), ('x', 0)])
                    ('call', [2, ('prova', '%s/%d' % (b.name, 2))]),
                    ('move', [('x', 0), ('y', 0)]),
                ]
            
                self.output.append(b.generate())
            else:
                code += b.generate()

        code += [
            ('deallocate', [heap]),
            ('return', []),
        ]
        return code

class For:
    label = 1
    name_num = 1

    def __init__(self, output, item, body):
        self.output = output
        self.name = "for%d" % For.name_num
        For.name_num += 1
        self.item = item
        self.body = body
        
    def generate(self):
        # for (context, list)
        item = self.item
        body = self.body
        
        code = [
            ('label', [For.label]),
            ('func_info', [('atom', 'prova'), ('atom', self.name), 2]),
            ('label', []),
            ('is_nonempty_list', [('f', For.label + 1), ('x', 1)]),
            ('allocate', [heap, 0]),
        ]
        
        code += [
            ('move', [('x', 0), ('y', 0)]),
            ('get_list', [('x', 1), ('x', 0), ('y', 1)]),
        ]
        code += assign_local(self.item)

        for b in body:
            if b.__class__ in (Def,):
                b.ancestors = self.ancestors
                code += [
                    ('move', [('integer', len(b.ancestors)), ('x', 0)]),
                    ('put_list', [('x', 0), ('nil', None), ('x', 0)]),
                    ('put_list', [('atom', b.name), ('x', 0), ('x', 0)]),
                ]
                code += assign_local(b.assign_to)
                
                self.output.append(b.generate())
            elif b.__class__ in (For,):
                b.ancestors = self.ancestors
            
                # passa al for (context; variabile su cui iterare il for)
                code += [
                    ('move', [('x', 0), ('x', 1)]),
                    ('move', [('y', 0), ('x', 0)])
                    ('call', [2, ('prova', '%s/%d' % (b.name, 2))]),
                    ('move', [('x', 0), ('y', 0)]),
                ]
            
                self.output.append(b.generate())
            else:
                code += b.generate()
                
        
        code += [
            ('move', [('y', 0), ('x', 0)]),
            ('move', [('y', 1), ('x', 1)]),
            ('call_last', [3, ('prova', self.name + '/2'), heap]),
            ('label', [For.label + 1]),
            ('is_nil', [('f', For.label), ('x', 1)]),
            #('move', [('y', 0), ('x', 0)]),
            ('return', []),
        ]

        For.label += 2

        return code

class Call:
    def __init__(self, called, params):
        self.called = called
        self.params = params

    def generate(self):
        called = self.called
        params = self.params
        
        code = merge_context()
        code += get_from_context(called)
        
        # ho ottenuto il riferimento alla funzione ovvero una lista [name, deep].
        code += [
            ('get_list', [('x', 0), ('y', 3), ('x', 0)]),
            ('get_list', [('x', 0), ('y', 4), ('x', 0)]),
        ]
        
        # prepara il sottinsieme del contesto da passare alla funzione
        code += [
            ('move', [('y', 0), ('x', 0)]),
            ('call_ext', [1, ('extfunc', 'lists:reverse/1')]),
            ('move', [('y', 4), ('x', 1)]),
            ('call_ext', [2, ('extfunc', 'lists:sublist/2')]),
            ('call_ext', [1, ('extfunc', 'lists:reverse/1')]),
        ]
        
        # comincio a costruire la lista dei parametri da passare alla funzione,
        # il primo elemento è il contesto creato appena sopra
        code += [('put_list', [('x', 0), ('nil', None), ('y', 5)]),]
        
        # cerco il valore di ciascuna variabile nel contesto e la aggiungo alla lista
        # di parametri da passare alla funzione
        code += [
            ('move', [('y', 0), ('x', 0)]),
            ('call', [1, ('prova', 'dict_list_merge/1')]),
            ('move', [('x', 0), ('y', 6)]),
        ]
        for p in params:
            code += [
                ('move', [('y', 6), ('x', 1)]),
                ('move', [('atom', p), ('x', 0)]),
                ('call_ext', [2, ('extfunc', 'orddict:fetch/2')]),
                ('put_list', [('x', 0), ('y', 5), ('y', 5)]),
            ]
        
        code += [
            ('move', [('y', 5), ('x', 2)]),
            ('move', [('atom', 'prova'), ('x', 0)]),
            ('move', [('y', 3), ('x', 1)]),
            ('call_ext', [3, ('extfunc', 'erlang:apply/3')]),
        ]
        #code += [('call', [1, ('prova', '%s/%d' % (self.called, 1))])]
        return code

class Assign:
    def __init__(self, var, value = None):
        self.var = var
        self.value = value

    def generate(self):
        var = self.var
        value = self.value

        if value:
            code = [('move', [('integer', value), ('x', 0)])]

        code += assign_local(var)
        
        return code

def assign_local(var):
    return [
        # estraggo la parte locale dal contesto
        ('get_list', [('y', 0), ('x', 2), ('y', 0)]),
        
        # inserisco la nuova variabile, tramite -> orddict:store('A', A, D0)
        ('move', [('x', 0), ('x', 1)]),
        ('move', [('atom', var), ('x', 0)]),
        ('call_ext', [3, ('extfunc', 'orddict:store/3')]),
        
        # rimetto la parte locale nel contesto
        ('put_list', [('x', 0), ('y', 0), ('y', 0)]),
    ]

class Range:
    def __init__(self, start, stop):
        self.start = start
        self.stop = stop - 1

    def generate(self):
        code = [
            ('move', [('integer', self.start), ('x', 0)]),
            ('move', [('integer', self.stop), ('x', 1)]),
        ]

        code += [('call_ext', [2, ('extfunc', 'lists:seq/2')])]
        return code

class Print:
    def __init__(self, value=None):
        self.value = value

    def generate(self):
        value = self.value

        code = []
        if value == None:
            code += [('put_list', [('x', 0), ('nil', None), ('x', 1)])]
        else:
            if type(value):
                code += [('move', [('integer', value), ('x', 0)])]
            else:
                code += merge_context()
                code += get_from_context(value)

        code += [
            ('put_list', [('x', 0), ('nil', None), ('x', 1)]),
            ('move', [('literal', None), 0]),
            ('int_code_end', []),
            ('call_ext', [2, ('extfunc', 'io:format/2')]),
        ]
        return code

class Debug:
    def __init__(self, what):
        self.what = what

    def generate(self):
        what = self.what
        
        if what == "context":
            code = [
                ('put_list', [('y', 0), ('nil', None), ('x', 1)]),
                ('move', [('literal', None), 0]),
                ('int_code_end', []),
                ('call_ext', [2, ('extfunc', 'io:format/2')]),
            ]
        else:
            raise Exception('debug what??')
        return code

def merge_context():
    return [
        ('move', [('y', 0), ('x', 0)]),
        ('call', [1, ('prova', 'dict_list_merge/1')]),
    ]

def get_from_context(var):
    return [
        # orddict:fetch('A', Dict)
        ('move', [('x', 0), ('x', 1)]),
        ('move', [('atom', var), ('x', 0)]),
        ('call_ext', [2, ('extfunc', 'orddict:fetch/2')]),
    ]
    

def ciao_like():
    attributes = ExternalTerm(List(Tuple(Atom('vsn'), List(Big(10355834843103504983582285655618377565L)))))
    compile_info = ExternalTerm(List(
            Tuple(Atom('options'), Nil()),
            Tuple(Atom('version'), String('4.8')), Tuple(
                Atom('time'),
                Tuple(Integer(2012), Integer(3), Integer(8), Integer(21), Integer(1), Integer(28))
            ), Tuple(Atom('source'),
            String('/Users/fabio/Programmazione/ErlangWorld/beam exploration/code/8/prova.erl'))))
    literals = [ExternalTerm(String('~w~n')), ExternalTerm(Integer(0)), ExternalTerm(Integer(10))]

    #atoms = ['prova', 'go', 'lists', 'seq', 'for1', 'io', 'format', 'ok', 'erlang',
    #         'module', 'module_info', 'get_module_info', 'a', 'orddict', 'fetch', 'new', 'store', 'call_go',
    #         'dict_list_merge', 'lists', 'sublist', 'foldl', 'dict_merge', 'merge', '-dict_merge/2-fun-0-',
    #         '-dict_list_merge/1-fun-0-', 'go2', 'i', 'fun__0', 't', 'apply']
    atoms = ['prova', 'go', 'lists', 'seq', 'for1', 'io', 'format', 'ok', 'erlang',
             'module', 'module_info', 'get_module_info', 'orddict', 'fetch', 'new', 'store', 'call_go',
             'dict_list_merge', 'lists', 'sublist', 'foldl', 'dict_merge', 'merge', '-dict_merge/2-fun-0-',
             '-dict_list_merge/1-fun-0-', 'fun__0', 't', 'apply', 'a', 'b', 'c', 'i',
             'reverse', 'fun__1'] # 'fun__2',


    imports = ['lists:seq/2', 'io:format/2', 'orddict:fetch/2', 'orddict:new/0', 'orddict:store/3',
               'lists:foldl/3', 'lists:sublist/2', 'orddict:merge/3', 'erlang:get_module_info/1',
               'erlang:get_module_info/2', 'erlang:apply/3', 'lists:reverse/1']
    exports = ['module_info/1', 'module_info/0', 'module/0', 'fun__0/1', 'fun__1/1'] #'fun__2/1'
    locals_ = ['-dict_list_merge/1-fun-0-/2', '-dict_merge/2-fun-0-/3', 'dict_merge/2',
               'dict_list_merge/1'] #'go/2', 'call_go/1'

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



if __name__ == '__main__':
    ciao_like()
    #test()