#!/usr/bin/env python
# encoding: utf-8

import os
dirname = os.path.dirname
BEAM_FOLDER = dirname(dirname(__file__))
from beam_writer import BeamWriter
from external_term_parser import *

class BeamEncoder():
    def __init__(self, atoms, code, imports, exports, literals, locals_, attributes, compile_info):
        self.load_opcodes()
        self.atoms = atoms
        self.code = code
        self.imports = imports
        self.exports = exports
        self.literals = literals
        self.locals = locals_
        self.labels = dict()
        self.attributes = attributes
        self.compile_info = compile_info
        self.beam_writer = BeamWriter()

    def load_opcodes(self):
        self.opcodes = dict()
        for line in open(os.path.join(BEAM_FOLDER, 'common', 'opcodes.txt'),'r'):
            s = line.split('->')
            dx = s[1].strip(' ()\n').split(',')
            opname = dx[0].rstrip()
            opcode = int(s[0].strip())
            self.opcodes[opname] = opcode

    def make_binary(self):
        self.assign_labels()
        self.assign_lambda_indexes()
        atoms = self.atoms
        imports = self.parse_imports(self.imports)
        exports = self.parse_exports(self.exports)
        lambdas = self.parse_lambdas(self.locals)
        code = self.parse_code(self.code)
        literals = self.literals
        locals_ = self.parse_locals(self.locals)
        attributes = self.attributes
        compile_info = self.compile_info

        beam_writer = self.beam_writer
        beam_writer.atom = atoms
        beam_writer.impT = imports
        beam_writer.expT = exports
        beam_writer.code = code
        beam_writer.funT = lambdas
        beam_writer.litT = literals
        beam_writer.locT = locals_
        beam_writer.attr = attributes
        beam_writer.cInf = compile_info

        beam_writer.build_binary()

    def assign_labels(self):
        # ['recursive', 'tail_fac', 'erlang', '-', '*', 'module_info', 'get_module_info']
        # exports = ['module_info/0', 'module_info/1', 'tail_fac/1']
        # locals_ = ['tail_fac/2']
        exports = self.exports
        locals_ = self.locals

        for fun in self.code:
            for instr in fun:
                if instr[0] == 'func_info':
                    info = instr[1]
                    if info[1] == None:
                        # si tratta di una funzione di ordine superiore
                        continue
                    self.labels['%s/%d' % (info[1][1], info[2])] = fun[fun.index(instr) + 1][1][0]
        
    def assign_lambda_indexes(self):
        self.lambda_indexes = [l for l in self.locals if l.startswith('-')]

    def write(self, file_name):
        self.beam_writer.write(file_name)

    def parse_imports(self, imports):
        result = list()
        for imp in imports:
            s0 = imp.split(':')
            m = s0[0]
            s1 = s0[1].split('/')
            f = s1[0]
            a = s1[1]
            result.append((self.atoms.index(m) + 1, self.atoms.index(f) + 1, int(a)))
        return result

    def parse_exports(self, exports):
        # ['hello/0', 'module_info/0', 'module_info/1']
        # [(3, 1, 6), (3, 0, 4), (2, 0, 2)]
        result = list()
        #label = 0
        for fun in exports:
            #label += 2
            s0 = fun.rsplit('/', 1)
            result.append((self.atoms.index(s0[0]) + 1, int(s0[1]), self.labels[fun]))
            #self.labeled_exports[fun] = label
        return result

    def parse_lambdas(self, locals_):
        # ['-dict_list_merge/1-fun-0-/2']
        # [21, 2, 18, 0, 0, '\x01\x08<\xec']
        result = list()
        lambdas = [l for l in locals_ if l.startswith('-')]
        for lam in lambdas:
            s0 = lam.rsplit('/', 1)
            result.append((self.atoms.index(s0[0]) + 1, int(s0[1]), self.labels[lam],
                           self.lambda_indexes.index(lam), 0, 17317100))
        #print result
        return result

    def parse_locals(self, locals_):
        # ['tail_fac/2']
        # [(2, 2, 4)]
        result = list()
        for fun in locals_:
            s0 = fun.rsplit('/', 1)
            result.append((self.atoms.index(s0[0]) + 1, int(s0[1]), self.labels[fun]))
        return result

    def parse_code(self, code):
        setattr(self, self.atoms[0], self.module_name)
        fun_r = list()
        #print code
        for fun in code:
            instr_r = list()
            for instr in fun:
                param_r = list()
                for param in instr[1]:
                    if type(param) is int:
                        type_ = 'pure'
                        val = param
                    else:
                        type_ = param[0]
                        val = param[1]
                    p = getattr(self, type_)(val)
                    if type(p) == tuple:
                        param_r += p
                    else:
                        param_r.append(p)
                    #print param_r
                        
                instr_r.append((self.opcodes[instr[0]], param_r))
                #print instr_r
            fun_r.append(instr_r)
            
            # aggiungo un int_code_end all'ultima funzione
        fun_r[-1].append((3, []))
        #print fun_r
        return fun_r

    def pure(self, n):
        if n > 15:
            return (8, n)
        return n * 16

    def atom(self, atom):
        if atom.startswith('-'):
            return (10, self.atoms.index(atom) + 1)
        return (self.atoms.index(atom) + 1) * 16 + 2

    def x(self, n):
        return n * 16 + 3
        
    def y(self, n):
        return n * 16 + 4

    def f(self, n):
        return n * 16 + 5

    def extfunc(self, fun):
        return self.pure(self.imports.index(fun))

    def module_name(self, fun):
        if fun.startswith('-'):
            return self.lambda_indexes.index(fun) * 16
        else:
            return self.labels[fun] * 16 + 5

    def integer(self, n):
        return n * 16 + 1
        
    def literal(self, n):
        return 71
        
    def nil(self, n):
        return 2


if __name__ == '__main__':
    
    atoms = ['prova', 'for', 'io', 'format', 'ok', 'module_info', 'erlang', 'get_module_info']
    # deve essere mantenuto l'ordine degli import!
    imports = ['io:format/2', 'erlang:get_module_info/1', 'erlang:get_module_info/2']
    exports = ['for/1']
    locals_ = []
    lambdas = []
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
            ],
    ]

    te = BeamEncoder(atoms, code, imports, exports, literals, locals_,
                     attributes, compile_info)
    te.make_binary()
    te.write('prova.beam')