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
from helpers import *
from beam.assembler.beam_encoder import *

heap_n = 7
heap_memory = 0
heap_context = 1
heap_class_context = 2
heap_aux = 3
heap_stack = 4
heap_temp_base = 5

class Module:
    def __init__(self, body):
        self.module_name = None
        self.body = body

    def __repr__(self):
        return "%s(%s)" % (str(self.__class__).split(".")[-1] , self.body)

    def generate(self):
        module_name = self.module_name
        body = self.body
        output = []

        code = [
            ('label', []),
            ('func_info', [('atom', module_name), ('atom', 'module'), 0]),
            ('label', []),
            ('allocate', [heap_n, 0]),
            # prepara memory
            ('call_ext', [0, ('extfunc', 'common:init_memory/0')]),
            ('move', [('x', 0), ('y', heap_memory)]),
            # preapara stack
            ('move', [('nil', None), ('y', heap_stack)]),
            # prepara context
            ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
            ('put_list', [('x', 0), ('nil', None), ('y', heap_context)]),
        ]

        for b in body:
            b.module_name = module_name
            if b.__class__ in (Def, For, If, Class):
                b.output = output
                b.ancestors = ['module']
            b.is_in_class = False
            code += b.generate()

        code += [
            ('move', [('atom', 'ok'), ('x', 0)]),
            ('deallocate', [heap_n]),
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
        self.is_in_class = False

    def __repr__(self):
        return "%s(%s, %s, %s)" % (str(self.__class__).split(".")[-1] , self.assign_to, self.params, self.body)

    def generate(self):
        self.to_base()
        return self.inline()
        
    def to_base(self):
        module_name = self.module_name
        output = self.output
        body = self.body
        params = self.params
        htb = lambda x: heap_temp_base + x

        code = [
            ('label', []),
            ('func_info', [('atom', module_name), ('atom', self.name), 3]),
            ('label', []),
            ('allocate', [heap_n, 0]),
            ('move', [('nil', None), ('y', heap_stack)]),
        ]

        # memorizzo la memoria, il contesto (a cui aggiungo un nuovo dizionario 
        # per le variabili locali), la lista dei parametri
        code += [
            ('move', [('x', 0), ('y', heap_memory)]),
            ('move', [('x', 1), ('y', heap_context)]),
            ('move', [('x', 2), ('y', htb(0))]),
            ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
            ('put_list', [('x', 0), ('y', heap_context), ('y', heap_context)]),
        ]
        
        # memorizzo i parametri attuali della funzione tra le variabili locali
        for p in params:
            code += [
                ('get_list', [('y', htb(0)), ('x', 3), ('y', htb(0))]),
                ('move', [('y', heap_memory), ('x', 0)]),
                ('move', [('y', heap_context), ('x', 1)]),
                ('move', [('literal', p), ('x', 2)]),
                ('call_ext', [4, ('extfunc', 'common:assign/4')]),
                ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
                ('get_tuple_element', [('x', 0), 1, ('y', heap_context)]),
            ]
        
        for b in body:
            b.module_name = self.module_name
            if b.__class__ in (Def, For, If, Class):
                b.output = output
                b.ancestors = self.ancestors + [self.name]
            b.is_in_class = False
            code += b.generate()
        
        code += [
            #destroy_locals
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', heap_context), ('x', 1)]),
            ('call_ext', [2, ('extfunc', 'common:destroy_locals/2')]),
            ('move', [('x', 0), ('y', heap_memory)]),
        ]
        code += [
            ('put_tuple', [2, ('x', 0)]),
            ('put', [('y', heap_memory)]),
            ('put', [('atom', 'None')]),
            ('deallocate', [heap_n]),
            ('return', []),
        ]
        output.append(code)
        
    def inline(self):
        # creo un oggetto funzione passandogli il puntatore al codice, ovvero il nome
        # della funzione vera, e la profondità
        # "function___new__(Memory, FuncName, Deep)"
        # dopodichè assegno l'oggetto alla variabile self.name
        if self.is_in_class:
            code = [
                ('move', [('y', heap_memory), ('x', 0)]),
                ('move', [('atom', self.name), ('x', 1)]),
                ('move', [('integer', len(self.ancestors)), ('x', 2)]),
                ('move', [('integer', len(self.params)), ('x', 3)]),
                ('move', [('literal', self.assign_to), ('x', 4)]),
                ('call_ext', [5, ('extfunc', 'base:instancemethod___new__/5')]),
                ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
                ('get_tuple_element', [('x', 0), 1, ('x', 0)]),
            ]
        else:
            code = [
                ('move', [('y', heap_memory), ('x', 0)]),
                ('move', [('atom', self.name), ('x', 1)]),
                ('move', [('integer', len(self.ancestors)), ('x', 2)]),
                ('move', [('integer', len(self.params)), ('x', 3)]),
                ('move', [('literal', self.assign_to), ('x', 4)]),
                ('call_ext', [5, ('extfunc', 'base:function___new__/5')]),
                ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
                ('get_tuple_element', [('x', 0), 1, ('x', 0)]),
            ]
        
        if self.is_in_class:
            context = heap_class_context
        else:
            context = heap_context
        code += [
            ('move', [('x', 0), ('x', 3)]),
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', context), ('x', 1)]),
            ('move', [('literal', self.assign_to), ('x', 2)]),
            ('call_ext', [4, ('extfunc', 'common:assign/4')]),
            # valore di ritorno {Memory, Context}
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ('get_tuple_element', [('x', 0), 1, ('y', context)]),
        ]
        return code

class For:
    name_num = 1

    def __init__(self, item, iteration_list, body):
        self.module_name = None
        self.output = None
        self.name = "for__%d" % For.name_num
        For.name_num += 1
        self.item = item
        self.iteration_list = iteration_list
        self.body = body
        self.is_in_class = False
    
    def __repr__(self):
        return "%s(%s, %s, %s)" % (str(self.__class__).split(".")[-1] , self.item, self.iteration_list, self.body)

    def generate(self):
        self.to_base()
        return self.inline()

    def to_base(self):
        # for (context, list)
        module_name = self.module_name
        output = self.output
        item = self.item
        body = self.body
        
        code = [
            ('label', ['%s:%d' % (self.name, 0)]),
            ('func_info', [('atom', module_name), ('atom', self.name), 4]),
            ('label', []),
            
            ('allocate', [heap_n, 0]),
            ('move', [('nil', None), ('y', heap_stack)]),
        ]
        
        code += [
            ('move', [('x', 0), ('y', heap_memory)]),
            ('move', [('x', 1), ('y', heap_context)]),
            ('move', [('x', 2), ('y', heap_class_context)]),
            ('move', [('x', 3), ('x', 0)]),
            # ('x', 0) -> lista su cui fare il for
        ]
        code += [
            ('is_nonempty_list', [('f', '%s:%d' % (self.name, 1)), ('x', 0)]),
            ('get_list', [('x', 0), ('x', 0), ('y', heap_aux)]),
        ]
        #code += print_register(('x', 0))
        code += assign(item, self.is_in_class)

        for b in body:
            b.module_name = self.module_name
            if b.__class__ in (Def, For, If, Class):
                b.output = output
                b.ancestors = self.ancestors + [self.name]
            b.is_in_class = self.is_in_class
            code += b.generate()
        
        code += [
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', heap_context), ('x', 1)]),
            ('move', [('y', heap_class_context), ('x', 2)]),
            ('move', [('y', heap_aux), ('x', 3)]),
            ('call_last', [4, (module_name, self.name + '/4'), heap_n]),
            ('label', ['%s:%d' % (self.name, 1)]),
            ##exdeall
            ('is_nil', [('f', '%s:%d' % (self.name, 0)), ('x', 0)]),
            # ritorna una tupla: {Memory, Context}
        ]
        code += [
            ('put_tuple', [3, ('x', 3)]),
            ('put', [('y', heap_memory)]),
            ('put', [('y', heap_context)]),
            ('put', [('y', heap_class_context)]),
            ('move', [('x', 3), ('x', 0)]),
            ('deallocate', [heap_n]),
            ('return', []),
        ]

        output.append(code)
        
    def inline(self):
        module_name = self.module_name
        is_in_class = self.is_in_class
        self.iteration_list.module_name = module_name
        # anche gli oggetti dell'iteration list devono eventualmente
        # poter accadere alle cose definite nella classe
        self.iteration_list.is_in_class = is_in_class
        code = self.iteration_list.generate()
        #code += print_register(('x', 0))
        
        # recupero la vera lista dall'oggetto lista
        code += [
            ('move', [('x', 0), ('x', 1)]),
            ('move', [('y', heap_memory), ('x', 0)]),
            ('call_ext', [2, ('extfunc', 'base:list_value/2')]),
        ]
        # code += print_register(('y', heap_memory))
        # chiama il for passando (memory, context, class_context(eventualmente vuoto) e
        # lista di oggetti su cui iterare il for)
        if is_in_class:
            code += [('move', [('y', heap_class_context), ('x', 2)])]
        else:
            code += [('move', [('atom', 'empty'), ('x', 2)])]
        code += [
            ('move', [('x', 0), ('x', 3)]),
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', heap_context), ('x', 1)]),
            ('call', [4, (module_name, '%s/%d' % (self.name, 4))]),
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
        ]
        if is_in_class:
            code += [('get_tuple_element', [('x', 0), 2, ('y', heap_class_context)])]
        else:
            code += [('get_tuple_element', [('x', 0), 1, ('y', heap_context)])]
        return code


class If:
    name_num = 1

    def __init__(self, conds, bodies):
        self.module_name = None
        self.output = None
        self.name = "if__%d" % If.name_num
        If.name_num += 1
        self.conds = conds
        self.bodies = bodies
        self.is_in_class = False
    
    def __repr__(self):
        return "%s(%s, %s)" % (str(self.__class__).split(".")[-1] , self.conds, self.bodies)

    def generate(self):
        self.to_base()
        return self.inline()

    def to_base(self):
        # for (context, list)
        module_name = self.module_name
        output = self.output
        bodies = self.bodies
        conds = self.conds
        f = lambda x: '%s:%d' % (self.name, x)
        
        code = [
            ('label', []),
            ('func_info', [('atom', module_name), ('atom', self.name), 4]),
            ('label', []),
            ('allocate', [heap_n, 0]),
            # memoria
            ('move', [('x', 0), ('y', heap_memory)]),
            # contesto
            ('move', [('x', 1), ('y', heap_context)]),
            # class_context (se c'è)
            ('move', [('x', 2), ('y', heap_class_context)]),
            # lista condizioni
            ('move', [('x', 3), ('y', heap_aux)]),
            # stack
            ('move', [('nil', None), ('y', heap_stack)]),
        ]
        
        for i in range(len(conds)):
            code += [
                ('get_list', [('y', heap_aux), ('x', 0), ('y', heap_aux)]),
                ('is_eq_exact', [('f', f(i)), ('x', 0), ('atom', 'true')]),
            ]
            for b in bodies[i]:
                b.module_name = self.module_name
                b.is_in_class = self.is_in_class
                code += b.generate()
            code += [
                ('put_tuple', [3, ('x', 0)]),
                ('put', [('y', heap_memory)]),
                ('put', [('y', heap_context)]),
                ('put', [('y', heap_class_context)]),
                ('deallocate', [heap_n]),
                ('return', []),
                ('label', [f(i)]),
            ]
            
        if len(bodies) == len(conds) + 1:
            for b in bodies[-1]:
                b.module_name = self.module_name
                b.is_in_class = self.is_in_class
                code += b.generate()
        
        code += [
            ('put_tuple', [3, ('x', 0)]),
            ('put', [('y', heap_memory)]),
            ('put', [('y', heap_context)]),
            ('put', [('y', heap_class_context)]),
            ('deallocate', [heap_n]),
            ('return', []),
        ]
        
        output.append(code)

    def inline(self):
        module_name = self.module_name
        conds = self.conds
        # passa all'if (memory, context, class_context, lista dei risultati delle condizioni)
        code = list()
        for c in conds:
            c.module_name = self.module_name
            c.is_in_class = self.is_in_class
            code += c.generate()
            code += to_stack()
        code += from_stack(len(conds))
        
        code += [
            ('move', [('x', 2), ('x', 1)]),
            ('move', [('y', heap_memory), ('x', 0)]),
            ('call_ext', [2, ('extfunc', 'base:bool_obj_list_to_value_list/2')]),
            ('move', [('x', 0), ('x', 3)]),
        ]
        code += [
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', heap_context), ('x', 1)]),
            ('move', [('y', heap_class_context), ('x', 2)]),
            ('call', [4, (module_name, '%s/%d' % (self.name, 4))]),        
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
        ]
        if self.is_in_class:
            code += [('get_tuple_element', [('x', 0), 2, ('y', heap_class_context)])]
        else:
            code += [('get_tuple_element', [('x', 0), 1, ('y', heap_context)])]
            
        return code

class Class:
    name_num = 0
    
    def __init__(self, assign_to, body, super_=None):
        self.module_name = None
        self.output = None
        self.name = "class__%d" % Class.name_num
        Class.name_num += 1
        self.body = body
        self.assign_to = assign_to
        self.is_in_class = False
        self.super_ = super_

    def __repr__(self):
        return "%s(%s, %s, %s)" % (str(self.__class__).split(".")[-1] , self.assign_to, self.body, self.super_)

    def generate(self):
        self.to_base()
        return self.inline()

    def to_base(self):
        module_name = self.module_name
        output = self.output
        body = self.body
        htb = lambda x: heap_temp_base + x

        code = [
            ('label', []),
            ('func_info', [('atom', module_name), ('atom', self.name), 2]),
            ('label', []),
            ('allocate', [heap_n, 0]),
            ('move', [('nil', None), ('y', heap_stack)]),
        ]

        # memorizzo la memoria, il contesto (a cui aggiungo un nuovo dizionario 
        # per le variabili locali), la lista dei parametri
        code += [
            ('move', [('x', 0), ('y', heap_memory)]),
            ('move', [('x', 1), ('y', heap_context)]),
            ('call_ext', [0, ('extfunc', 'orddict:new/0')]),
            ('move', [('x', 0), ('y', heap_class_context)]),
        ]
        
        for b in body:
            b.module_name = self.module_name
            if b.__class__ in (Def, For, If, Class):
                b.output = output
                b.ancestors = self.ancestors
            b.is_in_class = True
            code += b.generate()
        
        # code += [
        #     #destroy_locals
        #     ('move', [('y', heap_memory), ('x', 0)]),
        #     ('move', [('y', heap_aux), ('x', 1)]),
        #     ('call_ext', [2, ('extfunc', 'common:destroy_locals/2')]),
        #     ('move', [('x', 0), ('y', heap_memory)]),
        # ]
        code += [
            ('put_tuple', [2, ('x', 2)]),
            ('put', [('y', heap_memory)]),
            ('put', [('y', heap_class_context)]),
            ('move', [('x', 2), ('x', 0)]),
            ('deallocate', [heap_n]),
            ('return', []),
        ]
        output.append(code)

    def inline(self):
        htb = lambda x: heap_temp_base + x
        module_name = self.module_name
        code = [
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', heap_context), ('x', 1)]),
            ('call', [2, (module_name, '%s/%d' % (self.name, 2))]),
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ('get_tuple_element', [('x', 0), 1, ('x', 2)]),
        ]
        
        # creo un oggetto classe passandogli il puntatore al codice, ovvero il nome
        # della classe vera, e il suo contesto
        # "class___new__(Memory, ClassName, Context, BeautyName)"
        # dopodichè assegno l'oggetto alla variabile self.assign_to
        code += [
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('atom', self.name), ('x', 1)]),
            ('move', [('literal', self.assign_to), ('x', 3)]),
            ('call_ext', [4, ('extfunc', 'base:class___new__/4')]),
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ('get_tuple_element', [('x', 0), 1, ('y', htb(0))]),
        ]
        
        # salva il riferimento all'oggetto classe nel contesto
        code += [
            ('move', [('y', htb(0)), ('x', 3)]),
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', heap_context), ('x', 1)]),
            ('move', [('literal', self.assign_to), ('x', 2)]),
            ('call_ext', [4, ('extfunc', 'common:assign/4')]),
            # valore di ritorno {Memory, Context}
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ('get_tuple_element', [('x', 0), 1, ('y', heap_context)]),
        ]
        
        # inserisci nei metodi della classe il riferimento alla classe
        code += [
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', htb(0)), ('x', 1)]),
            ('call_ext', [2, ('extfunc', 'common:links_methods_to_class/2')]),
            ('move', [('x', 0), ('y', heap_memory)]),
        ]
        
        
        return code

## just inline code methods ##
#(M, C, ModuleName, Obj, Attribute)
class Dot:
    def __init__(self, obj, attribute):
        self.module_name = None
        self.obj = obj
        self.attribute = attribute
        self.is_in_assign = None
        
    def __repr__(self):
        return "%s(%s, %s)" % (str(self.__class__).split(".")[-1] , self.obj, self.attribute)

    def generate(self):
        code = list()
        code += evaluate(self.module_name, self.is_in_class, self.obj)
        # se la dot si trova dentro assign, si ritorna a quest'ultimo obj + il nome
        # dell'attributo, in quanto l'assign dovrà fare un __setattribute__
        if self.is_in_assign:
            code += [
                ('move', [('x', 0), ('x', 1)]),
                ('put_tuple', [2, ('x', 0)]),
                ('put', [('x', 1)]),
                ('put', [('literal', self.attribute)]),
            ]
        else:
            code += [
                ('move', [('x', 0), ('x', 3)]),
                ('move', [('y', heap_memory), ('x', 0)]),
                ('move', [('y', heap_context), ('x', 1)]),
                ('move', [('atom', self.module_name), ('x', 2)]),
                ('move', [('literal', self.attribute), ('x', 4)]),
                ('call_ext', [5, ('extfunc', 'common:dot/5')]),
                ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
                ('get_tuple_element', [('x', 0), 1, ('x', 0)]),
            ]
        # code += print_register(('x', 0))
        return code
        
class Assign:
    def __init__(self, obj1, obj2):
        self.module_name = None
        self.obj1 = obj1
        self.obj2 = obj2
        self.is_in_class = False

    def __repr__(self):
        return "%s(%s, %s)" % (str(self.__class__).split(".")[-1] , self.obj1, self.obj2)

    def generate(self):
        obj1 = self.obj1
        obj2 = self.obj2
        code = list()
        if obj1.__class__ == Dot:
            obj1.is_in_assign = True
            code += evaluate(self.module_name, self.is_in_class, obj1)
            code += to_stack()
            code += evaluate(self.module_name, self.is_in_class, obj2)
            code += to_stack()
            code += from_stack(2)
            # settare sull'oggetto 1, l'attributo "a" a 2:
            # [{1,"a"},2]
            code += [
                ('get_list', [('x', 2), ('x', 0), ('x', 1)]),
                ('get_tuple_element', [('x', 0), 0, ('x', 3)]),
                ('get_tuple_element', [('x', 0), 1, ('x', 4)]),
                ('get_list', [('x', 1), ('x', 5), ('x', 1)]),
                ('move', [('y', heap_memory), ('x', 0)]),
                ('move', [('y', heap_context), ('x', 1)]),
                ('move', [('atom', self.module_name), ('x', 2)]),
                ('call_ext', [6, ('extfunc', 'common:dot2/6')]),
                ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ]
            
        else:
            code += evaluate(self.module_name, self.is_in_class, self.obj2)
            code += assign(self.obj1, self.is_in_class)
            # code += print_register(('x', 0))
        return code
        
class Int:
    def __init__(self, value):
        self.module_name = None
        self.value = value
        self.is_in_class = False
        
    def __repr__(self):
        return "%s(%s)" % (str(self.__class__).split(".")[-1] , self.value)

    def generate(self):
        code = list()
        # chiama int___new__(Memory, N)
        code += [
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('integer', self.value), ('x', 1)]),
            ('call_ext', [2, ('extfunc', 'base:int___new__/2')]),
            # valore di ritorno {Memory, N}
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ('get_tuple_element', [('x', 0), 1, ('x', 0)]),
        ]
        #code += print_register(('x', 0))
        return code
        
class Str:
    def __init__(self, value):
        self.module_name = None
        self.value = value
        self.is_in_class = False
        
    def __repr__(self):
        return "%s(%s)" % (str(self.__class__).split(".")[-1] , self.value)

    def generate(self):
        code = list()
        # chiama str___new__(Memory, N)
        code += [
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('literal', self.value), ('x', 1)]),
            ('call_ext', [2, ('extfunc', 'base:str___new__/2')]),
            # valore di ritorno {Memory, N}
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ('get_tuple_element', [('x', 0), 1, ('x', 0)]),
        ]
        return code

class Var:
    builtins = ["range"]
    
    def __init__(self, name):
        self.module_name = None
        self.name = name
        self.is_in_class = False
        
    def __repr__(self):
        return "%s(%s)" % (str(self.__class__).split(".")[-1] , self.name)

    def generate(self):
        code = list()
        # caso builtin
        if self.name in Var.builtins:
            code += [
                ('move', [('y', heap_memory), ('x', 0)]),
                ('move', [('literal', self.name), ('x', 1)]),
                ('call_ext', [2, ('extfunc', 'base:builtin_function_or_method___new__/2')]),
                # valore di ritorno {Memory, N}
                ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
                ('get_tuple_element', [('x', 0), 1, ('x', 0)]),
            ]
            return code
        
        if self.is_in_class:
            code += [
                ('move', [('y', heap_class_context), ('x', 0)]),
                ('move', [('y', heap_context), ('x', 1)]),
                ('move', [('literal', self.name), ('x', 2)]),
                ('call_ext', [3, ('extfunc', 'common:get_from_context/3')]),
            ]
        else:
            code += [
                ('move', [('y', heap_context), ('x', 0)]),
                ('move', [('literal', self.name), ('x', 1)]),
                ('call_ext', [2, ('extfunc', 'common:get_from_context/2')]),
            ]
        #code += print_register(('x', 0))
        return code
        
class Print:
    def __init__(self, obj):
        self.module_name = None
        self.obj = obj
        self.is_in_class = False

    def __repr__(self):
        return "%s(%s)" % (str(self.__class__).split(".")[-1] , self.obj)

    def generate(self):
        code = list()
        obj = self.obj
        obj.module_name = self.module_name
        obj.is_in_class = self.is_in_class
        code += obj.generate()
        code += [
            ('move', [('x', 0), ('x', 1)]),
            ('move', [('y', heap_memory), ('x', 0)]),
            ('call_ext', [2, ('extfunc', 'common:print_standard_or_overwrited/2')]),
        ]
        # se la print dovesse ritornare la memoria ricordarsi di salvarla
        return code
                
class Call:
    def __init__(self, target, params):
        self.module_name = None
        self.target = target
        self.params = params
        self.is_in_class = False

    def __repr__(self):
        return "%s(%s, %s)" % (str(self.__class__).split(".")[-1] , self.target, self.params)

    def generate(self):
        target = self.target
        params = self.params
        module_name = self.module_name
        
        code = list()
        
        code += evaluate(module_name, self.is_in_class, target)
        code += to_stack()
        for p in params:
            code += evaluate(module_name, self.is_in_class, p)
            code += to_stack()
        
        code += from_stack(len(params) + 1)
        
        code += [
            ('get_list', [('x', 2), ('x', 3), ('x', 4)]),
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', heap_context), ('x', 1)]),
            ('move', [('atom', module_name), ('x', 2)]),
            ('call_ext', [5, ('extfunc', 'common:call/5')]),
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ('get_tuple_element', [('x', 0), 1, ('x', 0)]),
        ]
        return code


class KeywordOperator(object):
    def __init__(self, obj1, obj2):
        self.module_name = None
        self.obj1 = obj1
        self.obj2 = obj2
        self.is_in_class = False

    def __repr__(self):
        return "%s(%s, %s)" % (str(self.__class__).split(".")[-1] , self.obj1, self.obj2)

    def generate(self):
        obj1 = self.obj1
        obj2 = self.obj2
        module_name = self.module_name
        code = list()
        code += evaluate(self.module_name, self.is_in_class, obj1)
        code += to_stack()
        code += evaluate(self.module_name, self.is_in_class, obj2)
        code += to_stack()
        code += from_stack(2)
        
        # call_keyword(M, C, ModuleName, Keyword, Obj, [Other])
        code += [
            ('get_list', [('x', 2), ('x', 4), ('x', 5)]),
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', heap_context), ('x', 1)]),
            ('move', [('atom', module_name), ('x', 2)]),
            ('move', [('literal', self.keyword), ('x', 3)]),
            ('call_ext', [6, ('extfunc', 'common:call_keyword/6')]),
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ('get_tuple_element', [('x', 0), 1, ('x', 0)]),
        ]
        return code
        
class Gt(KeywordOperator):
    def __init__(self, obj1, obj2):
        super(Gt, self).__init__(obj1, obj2)
        self.keyword = "__gt__"
        
class Add(KeywordOperator):
    def __init__(self, obj1, obj2):
        super(Add, self).__init__(obj1, obj2)
        self.keyword = "__add__"
        
class Sub(KeywordOperator):
    def __init__(self, obj1, obj2):
        super(Sub, self).__init__(obj1, obj2)
        self.keyword = "__sub__"
        
class Mul(KeywordOperator):
    def __init__(self, obj1, obj2):
        super(Mul, self).__init__(obj1, obj2)
        self.keyword = "__mul__"
                
class Div(KeywordOperator):
    def __init__(self, obj1, obj2):
        super(Div, self).__init__(obj1, obj2)
        self.keyword = "__div__"
                
class Return:
    def __init__(self, obj=None):
        self.module_name = None
        self.obj = obj
        self.is_in_class = False
        
    def __repr__(self):
        return "%s(%s)" % (str(self.__class__).split(".")[-1] , self.obj)

    def generate(self):
        htb = lambda x: heap_temp_base + x
        obj = self.obj
        code = []
        if obj:
            obj.module_name = self.module_name
            obj.is_in_class = self.is_in_class
            code += obj.generate()
            #code += evaluate(self.module_name, obj)
            code += [('move', [('x', 0), ('y', htb(0))])]
        else:
            code += [('move', [('atom', 'None'), ('y', htb(0))])]
        #code += print_register(('y', htb(0)))
            
        code += [
            #destroy_locals
            ('move', [('y', heap_memory), ('x', 0)]),
            ('move', [('y', heap_context), ('x', 1)]),
            ('call_ext', [2, ('extfunc', 'common:destroy_locals/2')]),
            ('move', [('x', 0), ('y', heap_memory)]),
        ]
        code += [
            ('put_tuple', [2, ('x', 0)]),
            ('put', [('y', heap_memory)]),
            ('put', [('y', htb(0))]),
            ('deallocate', [heap_n]),
            ('return', []),
        ]
        return code

class Debug:
    def __init__(self, what):
        self.module_name = None
        self.what = what
        self.is_in_class = False

    def __repr__(self):
        return "%s(%s)" % (str(self.__class__).split(".")[-1] , self.what)

    def generate(self):
        what = self.what
        code = list()
        if what == "memory":
            code += [
                ('put_list', [('y', heap_memory), ('nil', None), ('x', 1)]),
            ]
        elif what == "context":
            code += [
                ('put_list', [('y', heap_context), ('nil', None), ('x', 1)]),
            ]
        elif what == 'stack':
            code += [
                ('put_list', [('y', heap_stack), ('nil', None), ('x', 1)]),
            ]
        elif what == 'aux':
            code += [
                ('put_list', [('y', heap_aux), ('nil', None), ('x', 1)]),
            ]
        else:
            raise Exception('debug what??')
        code += [
            ('move', [('literal', '~140p~n')]),
            ('int_code_end', []),
            ('call_ext', [2, ('extfunc', 'io:format/2')]),
        ]
        return code
    

if __name__ == '__main__':
    main()

