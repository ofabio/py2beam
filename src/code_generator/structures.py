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
        self.module_name = None
        self.body = body

    def generate(self):
        module_name = self.module_name
        body = self.body
        output = []

        code = [
            ('label', []),
            ('func_info', [('atom', module_name), ('atom', 'module'), 0]),
            ('label', []),
            ('allocate', [heap, 0]),  # alloco n y!
            ('move', [('nil', None), ('y', 2)]),
        ]

        # y0 contiene una lista di dizionari, ciascuno dei quali rappresenta l'insieme delle
        # variabili e delle funzioni definite ad un certo livello di profondità. Nel complesso
        # y0 contiene tutti i simboli a disposizione della funzione
        # y1 viene usato per contenere la lista su cui iterare nel for
        # y2 viene usato come stack per le chiamate a funzione
        code += [
            ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
            ('put_list', [('x', 0), ('nil', None), ('y', 0)]),
        ]

        for b in body:
            b.module_name = module_name
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
        self.module_name = None
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
        module_name = self.module_name
        output = self.output
        body = self.body
        params = self.params

        code = [
            ('label', []),
            ('func_info', [('atom', module_name), ('atom', self.name), 2]),
            ('label', []),
            ('allocate', [heap, 0]),
            ('move', [('nil', None), ('y', 2)]),
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
            b.module_name = self.module_name
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
    name_num = 1

    def __init__(self, item, items_list, body):
        self.module_name = None
        self.output = None
        self.name = "for__%d" % For.name_num
        For.name_num += 1
        self.item = item
        self.items_list = items_list
        self.body = body
    
    def generate(self):
        self.to_base()
        return self.inline()

    def to_base(self):
        # for (context, list)
        module_name = self.module_name
        output = self.output
        item = self.item
        items_list = self.items_list
        body = self.body
        
        code = [
            ('label', ['%s:%d' % (self.name, 0)]),
            ('func_info', [('atom', module_name), ('atom', self.name), 2]),
            ('label', []),
            ('is_nonempty_list', [('f', '%s:%d' % (self.name, 1)), ('x', 1)]),
            ('allocate', [heap, 0]),
            ('move', [('nil', None), ('y', 2)]),
        ]
        
        code += [
            ('move', [('x', 0), ('y', 0)]),
            ('get_list', [('x', 1), ('x', 0), ('y', 1)]),
        ]
        code += assign_local(self.item)

        for b in body:
            b.module_name = self.module_name
            if b.__class__ in (Def, For):
                b.output = output
                b.ancestors = self.ancestors + [self.name]
            code += b.generate()
                
        
        code += [
            ('move', [('y', 0), ('x', 0)]),
            ('move', [('y', 1), ('x', 1)]),
            ('call_last', [3, (module_name, self.name + '/2'), heap]),
            ('label', ['%s:%d' % (self.name, 1)]),
            ('is_nil', [('f', '%s:%d' % (self.name, 0)), ('x', 1)]),
            #('move', [('y', 0), ('x', 0)]), dovrebbe non essere necessario!
            ('return', []),
        ]

        output.append(code)
        
    def inline(self):
        module_name = self.module_name
        # passa al for (context, variabile su cui iterare il for)
        code = self.items_list.generate()
        code += [
            ('move', [('x', 0), ('x', 1)]),
            ('move', [('y', 0), ('x', 0)]),
            ('call', [2, (module_name, '%s/%d' % (self.name, 2))]),
            ('move', [('x', 0), ('y', 0)]),
        ]
        return code

class Call:
    def __init__(self, target, params):
        self.module_name = None
        self.target = target
        self.params = params

    def generate(self):
        module_name = self.module_name
        target = self.target
        params = self.params
        
        code = list()
        
        for p in params:
            code += evaluate(self.module_name, p)
            code += to_stack()
        
        code += from_stack(len(params))
        # x0->params_list
        code += [('move', [('x', 2), ('y', 3)])]
        ###code += print_register(('y', 3))
        
        code += merge_context(module_name)
        code += get_from_context(target)
        
        # ho ottenuto il riferimento alla funzione ovvero una lista [name, deep].
        code += [
            ('get_list', [('x', 0), ('y', 4), ('x', 0)]),
            ('get_list', [('x', 0), ('y', 5), ('x', 0)]),
        ]
        # y3->params_list, y4->name, y5->deep
        
        # prepara il sottinsieme (anche improprio) del contesto da passare alla funzione
        code += [
            ('move', [('y', 0), ('x', 0)]),
            ('call_ext', [1, ('extfunc', 'lists:reverse/1')]),
            ('move', [('y', 5), ('x', 1)]),
            ('call_ext', [2, ('extfunc', 'lists:sublist/2')]),
            ('call_ext', [1, ('extfunc', 'lists:reverse/1')]),
            ('move', [('x', 0), ('y', 5)]),
        ]
        # y3->params_list, y4->name, y5->new_context
        
        # costruisco la lista finale
        code += [
            ('put_list', [('y', 3), ('nil', None), ('y', 3)]),
            ('put_list', [('y', 5), ('y', 3), ('x', 2)]),
        ]
        
        code += [
            ('move', [('atom', module_name), ('x', 0)]),
            ('move', [('y', 4), ('x', 1)]),
            ('call_ext', [3, ('extfunc', 'erlang:apply/3')]),
        ]
        return code

def print_register((r, n)):
    return [
        ('put_list', [(r, n), ('nil', None), ('x', 1)]),
        ('move', [('literal', '~w~n')]),
        ('int_code_end', []),
        ('call_ext', [2, ('extfunc', 'io:format/2')]),
    ]

class Return:
    def __init__(self, obj=None):
        self.module_name = None
        self.obj = obj
        
    def generate(self):
        obj = self.obj
        code = []
        if obj:
            code += evaluate(self.module_name, obj)
        else:
            code += [('move', [('atom', 'None'), ('x', 0)])]
        code += [
            ('deallocate', [heap]),
            ('return', []),
        ]
        return code
    
class Assign:
    def __init__(self, var, obj):
        self.module_name = None
        self.var = var
        self.obj = obj

    def generate(self):
        code = evaluate(self.module_name, self.obj)
        code += assign_local(self.var)
        
        return code

def evaluate(module_name, obj):
    code = []
    if type(obj) == int:
        code += [('move', [('integer', obj), ('x', 0)])]
    elif type(obj) == float:
        code += [('move', [('float', obj), ('x', 0)])]
    elif type(obj) == str:
        code += [('move', [('literal', obj), ('x', 0)])]
    else:
        obj.module_name = module_name
        code += obj.generate()
    return code
    
def to_stack():
    return [('put_list', [('x', 0), ('y', 2), ('y', 2)])]
    
def from_stack(n):
    """poppa dallo stack (y, 2) n parametri e li mette in una nuova lista su (x, 2)"""
    code = [('move', [('nil', None), ('x', 2)])]
    for i in range(n):
        code += [        
            ('get_list', [('y', 2), ('x', 0), ('y', 2)]),
            ('put_list', [('x', 0), ('x', 2), ('x', 2)]),
        ]
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
    def __init__(self, obj1, obj2):
        self.module_name = None
        self.obj1 = obj1
        self.obj2 = obj2

    def generate(self):
        code = list()
        code += evaluate(self.module_name, self.obj1)
        code += to_stack()
        code += evaluate(self.module_name, self.obj2)
        code += to_stack()
        
        code += from_stack(2)
        
        # sottrae 1 all'estremo destro
        code += [
            ('get_list', [('x', 2), ('y', 3), ('x', 2)]),
            ('get_list', [('x', 2), ('y', 4), ('x', 2)]),
            ('gc_bif2', [('f', 0), 1, ('extfunc', 'erlang:-/2'), 
                ('y', 4), ('integer', 1), ('y', 4)]),
            ('move', [('nil', None), ('x', 2)]),
            ('put_list', [('y', 4), ('x', 2), ('x', 2)]),
            ('put_list', [('y', 3), ('x', 2), ('x', 2)]),
        ]
        
        code += [
            ('move', [('atom', 'lists'), ('x', 0)]),
            ('move', [('atom', 'seq'), ('x', 1)]),
            ('call_ext', [3, ('extfunc', 'erlang:apply/3')]),
        ]
        return code
        
class Sum:
    def __init__(self, obj1, obj2):
        self.module_name = None
        self.obj1 = obj1
        self.obj2 = obj2
        
    def generate(self):
        module_name = self.module_name
        code = list()
        #code += print_register(('y', 2)) ###
        code += evaluate(self.module_name, self.obj1)
        code += to_stack()
        code += evaluate(self.module_name, self.obj2)
        code += to_stack()
        
        code += from_stack(2)
        
        code += [
            ('move', [('atom', module_name), ('x', 0)]),
            ('move', [('atom', 'utility__sum__'), ('x', 1)]),
            ('call_ext', [3, ('extfunc', 'erlang:apply/3')]),
        ]
        return code

class Print:
    def __init__(self, obj=None):
        self.module_name = None
        self.obj = obj

    def generate(self):
        code = list()
        code += evaluate(self.module_name, self.obj)

        code += [
            ('put_list', [('x', 0), ('nil', None), ('x', 1)]),
            ('move', [('literal', '~p~n')]),
            ('int_code_end', []),
            ('call_ext', [2, ('extfunc', 'io:format/2')]),
        ]
        return code
        
class Var:
    def __init__(self, name):
        self.module_name = None
        self.name = name
        
    def generate(self):
        module_name = self.module_name
        name = self.name
        code = merge_context(self.module_name)
        code += get_from_context(name)
        return code

class Debug:
    def __init__(self, what):
        self.module_name = None
        self.what = what

    def generate(self):
        what = self.what
        code = list()
        if what == "context":
            code += [
                ('put_list', [('y', 0), ('nil', None), ('x', 1)]),
            ]
        elif what == 'stack':
            code += [
                ('put_list', [('y', 2), ('nil', None), ('x', 1)]),
            ]
        else:
            raise Exception('debug what??')
        code += [
            ('move', [('literal', None), 0]),
            ('int_code_end', []),
            ('call_ext', [2, ('extfunc', 'io:format/2')]),
        ]
        return code

def merge_context(module_name):
    return [
        ('move', [('y', 0), ('x', 0)]),
        ('call', [1, (module_name, 'dict_list_merge/1')]),
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

