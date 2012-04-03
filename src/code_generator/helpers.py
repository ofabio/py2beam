#!/usr/bin/env python
# encoding: utf-8
"""
helpers.py

Created by Fabio Pricoco on 2012-04-02.
Copyright (c) 2012 ofabio. All rights reserved.
"""

import sys
import os


def print_register((r, n)):
    return [
        ('put_list', [(r, n), ('nil', None), ('x', 1)]),
        ('move', [('literal', '~w~n')]),
        ('int_code_end', []),
        ('call_ext', [2, ('extfunc', 'io:format/2')]),
    ]
    
    
def evaluate(module_name, obj):
    code = []
    if type(obj) == int:
        code += [('move', [('integer', obj), ('x', 0)])]
    elif type(obj) == float:
        code += [('move', [('float', obj), ('x', 0)])]
    elif type(obj) == str:
        code += [
            ('move', [('literal', obj)]),
            ('int_code_end', []),
        ]
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