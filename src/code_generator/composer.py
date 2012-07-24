#!/usr/bin/env python
# encoding: utf-8
"""
composer.py

Created by Fabio Pricoco on 2012-03-11.
Copyright (c) 2012 ofabio. All rights reserved.
"""

import sys
import os
dirname = os.path.dirname
ROOT_FOLDER = dirname(dirname(__file__))
sys.path.append(ROOT_FOLDER)
from beam.assembler.beam_encoder import *


class Composer:
    def __init__(self, base_path, module_name, tree, verbose=False):
        self.base_path = base_path
        self.module_name = module_name
        self.tree = tree
        self.beam_encoder = None
        self.verbose = verbose

    def generate(self):
        self.tree.module_name = self.module_name
        code = self.tree.generate()
        
        code += (self.utility_code())
        
        code = self.assign_labels(code)
        
        #self.print_code(code)
        
        hb = HeaderBuilder(code)
        h = hb.get_header()
        if self.verbose:
            self.print_header(h)
            self.print_code(code)
        
        h['literals'] = [ExternalTerm(String(l)) for l in h['literals']]
        be = BeamEncoder(h['atoms'], code, h['imports'], h['exports'], h['literals'], h['locals'], 
                         h['attributes'], h['compile_info'])
        be.make_binary()
        self.beam_encoder = be
        
    def write(self):
        be = self.beam_encoder
        if not be:
            raise Exception('Do generate, before!')
        filename = os.path.join(self.base_path, self.module_name + ".beam")
        # be.write('code_generator/%s.beam' % self.module_name)
        be.write(filename)
        print
        print 'file "%s.beam" generated!' % self.module_name

    def utility_code(self):
        module_name = self.module_name
        code = [
        [
            ('label', []),
            ('func_info', [('atom', module_name), ('atom', 'module_info'), 0]),
            ('label', []),
            ('move', [('atom', module_name), ('x', 0)]),
            ('call_ext_only', [1, ('extfunc', 'erlang:get_module_info/1')]),
        ],
        [
            ('label', []),
            ('func_info', [('atom', module_name), ('atom', 'module_info'), 1]),
            ('label', []),
            ('move', [('x', 0), ('x', 1)]),
            ('move', [('atom', module_name), ('x', 0)]),
            ('call_ext_only', [2, ('extfunc', 'erlang:get_module_info/2')]),
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
                        if param[1] == 0:
                            new_param = (param[0], 0)
                        else:
                            new_param = (param[0], labels[param[1]])
                        params[params.index(param)] = new_param
        return output
        
    
    def print_code(self, code):
        print '-- CODE --'
        for i in code:
            for i2 in i:
                print '%s,' % (i2,)
            print
        print '----------'
            
    def print_header(self, header):
        print '-- HEADER --'
        for h in header.items():
            print '%s: %s' % (h[0], h[1])
        print '------------'

class HeaderBuilder:
    def __init__(self, code):
        self.code = code
        self.mod_name = self.get_module_name()
    
    def get_module_name(self):
        for instr in self.code[0]:
            if instr[0] == 'func_info':
                return instr[1][0][1]
                
    def get_header(self):
        h = dict()
        atoms = set()
        imports = set()
        exports = list()
        locals_ = list()
        literals = list()
        
        for fun in self.code:
            for instr in fun:
                atoms.update(set(self.add_atoms(instr)))
                imports.update(set(self.add_imports(instr)))
                (exp, loc) = self.add_exports_and_locals(instr)
                exports += exp
                locals_ += loc
                self.feed_literals(literals, instr)
                
        # sposta l'atomo del nome_modulo al primo posto
        atomsl = list(atoms)
        del(atomsl[atomsl.index(self.mod_name)])
        atomsl.insert(0, self.mod_name)
        
        h['atoms'] = atomsl
        h['imports'] = list(imports)
        h['exports'] = exports
        h['locals'] = locals_
        
        h['literals'] = literals
        
        h['attributes'] = ExternalTerm(List(Tuple(Atom('vsn'), 
                            List(Big(10355834843103504983582285655618377565L)))))
        
        h['compile_info'] = ExternalTerm(List(
                Tuple(Atom('options'), Nil()),
                Tuple(Atom('version'), String('4.8')), Tuple(
                    Atom('time'),
                    Tuple(Integer(2012), Integer(3), Integer(8), Integer(21), Integer(1), Integer(28))
                ), Tuple(Atom('source'),
                String('/Users/fabio/Programmazione/ErlangWorld/beam exploration/code/8/prova.erl'))))
        
        #print h
        return h
        
    def add_atoms(self, instr):
        res = list()
        for p in instr[1]:
            if type(p) == tuple:
                if p[0] == 'atom':
                    res.append(p[1])
                elif p[0] == 'extfunc':
                    s1 = p[1].split(':')
                    res.append(s1[0])
                    s2 = s1[1].split('/')
                    res.append(s2[0])
                elif p[0] == self.mod_name:
                    s = p[1].split('/')
                    res.append(s[0])
        return res

    def add_imports(self, instr):
        res = list()
        if 'ext' in instr[0] or 'bif2' in instr[0]:
            for p in instr[1]:
                if type(p) == tuple and p[0] == 'extfunc':
                    res.append(p[1])
        return res
        
    def add_exports_and_locals(self, instr):
        exp = list()
        loc = list()
        if instr[0] == 'func_info':
            func_name = '%s/%d' % (instr[1][1][1], instr[1][2])
            if (func_name.startswith('module') or func_name.startswith('fun__') or 
                func_name.startswith('utility__')):
                exp.append(func_name)
            else:
                loc.append(func_name)
        return (exp, loc)
        
    def feed_literals(self, literals, instr):
        if instr[0] in ('move', 'get_list', 'put_list', 'put'):
            params = instr[1]
            for p in params:
                if type(p) == tuple and p[0] == 'literal':
                    if p[1] in literals:
                        n_lit = literals.index(p[1])
                    else:
                        n_lit = len(literals)
                        literals.append(p[1])
                    pos = params.index(p)
                    del(params[pos])
                    params.insert(pos, ('literal', n_lit))

if __name__ == '__main__':
    import test
    test.main()