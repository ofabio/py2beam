#!/usr/bin/env python
# encoding: utf-8
"""
sparecchiatavola.py

Created by Fabio Pricoco on 2012-03-25.
Copyright (c) 2012 ofabio. All rights reserved.
"""


def test():
    attributes = ExternalTerm(List(Tuple(Atom('vsn'), List(Big(10355834843103504983582285655618377565L)))))
    compile_info = ExternalTerm(List(
            Tuple(Atom('options'), Nil()),
            Tuple(Atom('version'), String('4.8')), Tuple(
                Atom('time'),
                Tuple(Integer(2012), Integer(3), Integer(8), Integer(21), Integer(1), Integer(28))
            ), Tuple(Atom('source'),
            String('/Users/fabio/Programmazione/ErlangWorld/beam exploration/code/8/prova.erl'))))
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