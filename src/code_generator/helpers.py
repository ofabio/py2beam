#!/usr/bin/env python
# encoding: utf-8
"""
helpers.py

Created by Fabio Pricoco on 2012-04-02.
Copyright (c) 2012 ofabio. All rights reserved.
"""

import sys
import os

heap_n = 7
heap_memory = 0
heap_context = 1
heap_class_context = 2
heap_aux = 3
heap_stack = 4
heap_temp_base = 5

def print_register((r, n)):
    return [
        ('put_list', [(r, n), ('nil', None), ('x', 1)]),
        ('move', [('literal', '~p~n')]),
        ('int_code_end', []),
        ('call_ext', [2, ('extfunc', 'io:format/2')]),
    ]
    
def evaluate(module_name, is_in_class, obj):
    obj.is_in_class = is_in_class
    obj.module_name = module_name
    return obj.generate()

def to_stack():
    return [('put_list', [('x', 0), ('y', heap_stack), ('y', heap_stack)])]

def from_stack(n):
    """pop dallo stack (y, 2) n parametri e li mette in una nuova lista su (x, 2)"""
    code = [('move', [('nil', None), ('x', 2)])]
    for i in range(n):
        code += [        
            ('get_list', [('y', heap_stack), ('x', 0), ('y', heap_stack)]),
            ('put_list', [('x', 0), ('x', 2), ('x', 2)]),
        ]
    return code
    
def call_method(module_name, method, params, memory_expected):
    code = list()
    code += evaluate(module_name, params[0])
    code += to_stack()
    code += evaluate(module_name, params[1])
    code += to_stack()

    code += from_stack(2)
    
    # chiama il metodo method dell'oggetto
    # call_method(Memory, Obj, Method, Params)
    code += [
        ('get_list', [('x', 2), ('x', 1), ('x', 3)]),
        ('move', [('y', heap_memory), ('x', 0)]),
        ('move', [('literal', method), ('x', 2)]),
        ('call_ext', [4, ('extfunc', 'common:call_method/4')]),
    ]
    if memory_expected:
        code += [
            ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
            ('get_tuple_element', [('x', 0), 1, ('x', 0)]),
        ]
    return code
    
def assign(var, is_in_class=False):
    # assegna un oggetto alla variabile "var" aumentando il riferimento in memoria e,
    # eventualmente, decrementando il riferimento all'elemento in memoria già associato 
    # alla variabile
    if is_in_class:
        context = heap_class_context
    else:
        context = heap_context
    return [
        ('move', [('x', 0), ('x', 3)]),
        ('move', [('y', heap_memory), ('x', 0)]),
        ('move', [('y', context), ('x', 1)]),
        ('move', [('literal', var), ('x', 2)]),
        ('call_ext', [4, ('extfunc', 'common:assign/4')]),
        # valore di ritorno {Memory, Context}
        ('get_tuple_element', [('x', 0), 0, ('y', heap_memory)]),
        ('get_tuple_element', [('x', 0), 1, ('y', context)]),
    ]