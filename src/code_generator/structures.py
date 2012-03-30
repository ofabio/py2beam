#!/usr/bin/env python
# encoding: utf-8
"""
structures.py

Created by Fabio Pricoco on 2012-03-27.
Copyright (c) 2012 ofabio. All rights reserved.
"""

import sys
import os
dirname = os.path.dirname
ROOT_FOLDER = dirname(dirname(__file__))
sys.path.append(ROOT_FOLDER)
from beam.assembler.beam_encoder import *

heap = 7

class Module:
    def __init__(self, body):
        self.output = []
        self.body = body

    def generate(self):
        output = self.output
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
            if b.__class__ in (Def, For):
                b.output = output
                b.ancestors = ['module']
            code += b.generate()

        code += [
            ('move', [('atom', 'ok'), ('x', 0)]),
            ('deallocate', [heap]),
            ('return', []),
        ]
        
        output.append(code)
        return output

class Def:
    n_fun = 0
    
    def __init__(self, assign_to, params, body):
        self.output = None
        self.assign_to = assign_to
        self.params = params
        self.body = body
        self.name = 'fun__%d' % Def.n_fun
        Def.n_fun += 1
        self.ancestors = []

    def generate(self):
        self.to_base()
        return self.inline()
        
    def to_base(self):
        output = self.output
        body = self.body
        params = self.params

        code = [
            ('label', []),
            ('func_info', [('atom', 'prova'), ('atom', self.name), 2]),
            ('label', []),
            ('allocate', [heap, 0]),
        ]

        # memorizzo in ('y', 0) il contesto a cui aggiungo un nuovo dizionario 
        # per le variabili locali, memorizzo anche la lista dei parametri
        code += [
            ('move', [('x', 0), ('y', 0)]),
            ('move', [('x', 1), ('y', 3)]),
            ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
            ('put_list', [('x', 0), ('y', 0), ('y', 0)]),
        ]
        
        # memorizzo i parametri attuali della funzione tra le variabili locali
        code += [
            # estraggo la parte locale dal contesto
            ('get_list', [('y', 0), ('y', 4), ('y', 0)]),
            ('move', [('y', 4), ('x', 2)]),
        ]
        for p in params:
            code += [
                # inserisco la nuova variabile, tramite -> orddict:store('A', A, D0)
                ('get_list', [('y', 3), ('x', 1), ('y', 3)]),
                ('move', [('atom', p), ('x', 0)]),
                ('call_ext', [3, ('extfunc', 'orddict:store/3')]),
                ('move', [('x', 0), ('x', 2)]),
            ]
        
        # rimetto la parte locale nel contesto
        code += [('put_list', [('x', 0), ('y', 0), ('y', 0)]),]
        
        for b in body:
            if b.__class__ in (Def, For):
                b.output = output
                b.ancestors = self.ancestors + [self.name]
            code += b.generate()
                
        code += [
            ('move', [('atom', 'None'), ('x', 0)]),
            ('deallocate', [heap]),
            ('return', []),
        ]
        output.append(code)
        
    def inline(self):
        # salva il riferimento alla nuova funzione: [name=fun1, deep=1].
        # deep è la profondità che darà luogo la nuova funzione, nonchè il numero
        # di insiemi di variabili da trasportare, a partire da quello di livello
        # modulo in su!
        code = [
            ('move', [('integer', len(self.ancestors)), ('x', 0)]),
            ('put_list', [('x', 0), ('nil', None), ('x', 0)]),
            ('put_list', [('atom', self.name), ('x', 0), ('x', 0)]),
        ]
        code += assign_local(self.assign_to)
        return code

class For:
    label = 1
    name_num = 1

    def __init__(self, item, items_list, body):
        self.output = None
        self.name = "for%d" % For.name_num
        For.name_num += 1
        self.item = item
        self.items_list = items_list
        self.body = body
    
    def generate(self):
        self.to_base()
        return self.inline()

    def to_base(self):
        # for (context, list)
        output = self.output
        item = self.item
        items_list = self.items_list
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
            if b.__class__ in (Def, For):
                b.output = output
                b.ancestors = self.ancestors + [self.name]
            code += b.generate()
                
        
        code += [
            ('move', [('y', 0), ('x', 0)]),
            ('move', [('y', 1), ('x', 1)]),
            ('call_last', [3, ('prova', self.name + '/2'), heap]),
            ('label', [For.label + 1]),
            ('is_nil', [('f', For.label), ('x', 1)]),
            #('move', [('y', 0), ('x', 0)]), dovrebbe non essere necessario!
            ('return', []),
        ]

        For.label += 2
        output.append(code)
        
    def inline(self):
        # passa al for (context, variabile su cui iterare il for)
        code = self.items_list.generate()
        code += [
            ('move', [('x', 0), ('x', 1)]),
            ('move', [('y', 0), ('x', 0)]),
            ('call', [2, ('prova', '%s/%d' % (self.name, 2))]),
            ('move', [('x', 0), ('y', 0)]),
        ]
        return code

class Call:
    def __init__(self, called, params):
        self.called = called
        self.params = params

    def generate(self):
        called = self.called
        params = self.params
        
        code = []
        
        code += merge_context()
        code += get_from_context(called)
        
        # ho ottenuto il riferimento alla funzione ovvero una lista [name, deep].
        code += [
            ('get_list', [('x', 0), ('y', 3), ('x', 0)]),
            ('get_list', [('x', 0), ('y', 4), ('x', 0)]),
        ]
        # y3->name y4->deep
        
        # prepara il sottinsieme del contesto da passare alla funzione
        code += [
            ('move', [('y', 0), ('x', 0)]),
            ('call_ext', [1, ('extfunc', 'lists:reverse/1')]),
            ('move', [('y', 4), ('x', 1)]),
            ('call_ext', [2, ('extfunc', 'lists:sublist/2')]),
            ('call_ext', [1, ('extfunc', 'lists:reverse/1')]),
            ('move', [('x', 0), ('y', 5)]),
        ]
        # y5->new_context
        
        # recupero dal contesto il valore di ciascuna variabile da passare e li inserisco
        # in una nuova lista. verrà infine costituita una lista contenente il contesto e
        # la suddetta lista di parametri
        code += [
            ('move', [('y', 0), ('x', 0)]),
            ('call', [1, ('prova', 'dict_list_merge/1')]),
            ('move', [('x', 0), ('y', 6)]),
        ]
        # y6->merged_context
        
        code += [('move', [('nil', None), ('y', 4)]),]
        # y4 -> empty_list
        
        for p in params:
            code += [
                ('move', [('y', 6), ('x', 1)]),
                ('move', [('atom', p), ('x', 0)]),
                ('call_ext', [2, ('extfunc', 'orddict:fetch/2')]),
                ('put_list', [('x', 0), ('y', 4), ('y', 4)]),
            ]
            
        # costruisce la lista finale
        code += [
            ('move', [('y', 4), ('x', 0)]),
            ('call_ext', [1, ('extfunc', 'lists:reverse/1')]),
            ('put_list', [('x', 0), ('nil', None), ('y', 6)]),
            ('put_list', [('y', 5), ('y', 6), ('x', 0)]),
            
        ]
        
        code += [
            ('move', [('x', 0), ('x', 2)]),
            ('move', [('atom', 'prova'), ('x', 0)]),
            ('move', [('y', 3), ('x', 1)]),
            ('call_ext', [3, ('extfunc', 'erlang:apply/3')]),
        ]
        return code

class Return:
    def __init__(self, obj=None):
        self.obj = obj
        
    def generate(self):
        obj = self.obj
        code = []
        if obj:
            code += evaluate(obj)
        else:
            code += [('move', [('atom', 'None'), ('x', 0)])]
        code += [
            ('deallocate', [heap]),
            ('return', []),
        ]
        return code
    
class Assign:
    def __init__(self, var, obj):
        self.var = var
        self.obj = obj

    def generate(self):
        code = evaluate(self.obj)
        code += assign_local(self.var)
        
        return code

def evaluate(obj):
    code = []
    if type(obj) == int:
        code += [('move', [('integer', obj), ('x', 0)])]
    elif type(obj) == float:
        code += [('move', [('float', obj), ('x', 0)])]
    else:
        code += obj.generate()
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
    def __init__(self, obj=None):
        self.obj = obj

    def generate(self):
        code = evaluate(self.obj)

        code += [
            ('put_list', [('x', 0), ('nil', None), ('x', 1)]),
            ('move', [('literal', None), 0]),
            ('int_code_end', []),
            ('call_ext', [2, ('extfunc', 'io:format/2')]),
        ]
        return code
        
class Var:
    def __init__(self, name):
        self.name = name
        
    def generate(self):
        name = self.name
        code = merge_context()
        code += get_from_context(name)
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
    

if __name__ == '__main__':
    main()

