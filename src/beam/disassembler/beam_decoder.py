#!/usr/bin/env python
# encoding: utf-8
"""
beam_decoder.py

Created by Fabio Pricoco on 2012-03-10.
Copyright (c) 2012 ofabio. All rights reserved.
"""

import sys
import os
from beam_reader import BeamReader

class BeamDecoder:
    def __init__(self, file_name):
        self.beam_reader = BeamReader(file_name)
    
    def decode(self):
        beam_reader = self.beam_reader
        beam_reader.parse()
        atoms = beam_reader.get_atoms()
        code = beam_reader.get_code()
        imports = beam_reader.get_imports()
        exports = beam_reader.get_exports()
        lambdas = beam_reader.get_lambdas()
        locals_ = beam_reader.get_locals()
        literals = beam_reader.get_literals()
        
        p_atoms = atoms
        p_imports = self.decode_imports(imports, atoms)
        (p_exports, exports_labels) = self.decode_exports(exports, atoms)
        (p_locals, locals_labels) = self.decode_locals(locals_, atoms)
        lambda_labels = self.decode_lambdas(lambdas, atoms)
        p_literals = self.decode_literals(literals)
        labels = dict(exports_labels.items() + locals_labels.items())
        #print labels
        p_code = self.decode_code(code, atoms, labels, lambda_labels, p_imports)
        
        self.atoms = p_atoms
        self.imports = p_imports
        self.exports = p_exports
        self.locals = p_locals
        self.literals = p_literals
        self.code = p_code
        
    def decode_code(self, code, atoms, labels, lambda_labels, imports):
        result = list()
        for instr in code:
            pos = 0
            params = list()
            hof = False
            en = False
            for param in instr['params']:
                dp = self.decode_param(instr['opname'], param, pos, atoms, labels,
                                       lambda_labels, imports, hof, en)
                hof = False
                en = False
                if dp == 'HigherOrderFun':
                    hof = True
                elif dp == 'extended_numbering':
                    en = True
                else:
                    params.append(dp)
                pos += 1
            result.append((instr['opname'], params))
        return self.split_functions(result)
        
    def split_functions(self, instrs):
        instrs.pop(-1)
        code = list()
        j1 = False
        start = 0
        for instr in instrs:
            opname = instr[0]
            if opname == 'func_info':
                if j1 == False:
                    j1 = True
                else:
                    pos = instrs.index(instr)
                    for i in range(2):
                        if instrs[pos - 1][0] in ['line', 'label']:
                            pos -= 1
                    code.append(instrs[start:pos])
                    start = pos
        code.append(instrs[start:])
        return code
        
    def decode_param(self, opname, p, pos, atoms, labels, lambda_labels, imports, hof, en):
        if hof:
            return ('atom', atoms[p - 1])
        if en:
            return p
        if p == 2:
            return ('nil', None)
        elif p == 71:
            return ('literal', None)
            
        elif p % 16 == 0:
            if opname.startswith('call') and pos == 1:
                return ('extfunc', imports[p / 16])
            elif opname == 'make_fun2':
                #return 'lambda n.%d' % (p / 16)
                return (atoms[0], lambda_labels[p / 16])
                ###
            else:
                return p / 16
        elif p % 16 == 1:
            return ('integer', (p - 1) / 16)
        elif p % 16 == 2:
            return ('atom', atoms[(p - 2) / 16 - 1])
        elif p % 16 == 3:
            return ('x', (p - 3) / 16)
        elif p % 16 == 4:
            return ('y', (p - 4) / 16)
        elif p % 16 == 5:
            if opname.startswith('call'):
                return (atoms[0], labels[(p - 5) / 16])
            else:
                return ('f', (p - 5) / 16)
        elif p == 10:
            return 'HigherOrderFun'
            
        elif p == 8:
            return 'extended_numbering'
    
    def decode_imports(self, imports, atoms):
        return ['%s:%s/%s' % (atoms[imp[0] - 1], atoms[imp[1] - 1], imp[2]) for imp in imports]

    def decode_exports(self, exports, atoms):
        exports_res = list()
        labels = dict()
        for exp in exports:
            val = '%s/%s' % (atoms[exp[0] - 1], exp[1])
            exports_res.append(val)
            labels[exp[2]] = val
        return (exports_res, labels)
        

    def decode_locals(self, locals_, atoms):
        locals_res = list()
        labels = dict()
        for loc in locals_:
            val = '%s/%s' % (atoms[loc[0] - 1], loc[1])
            locals_res.append(val)
            labels[loc[2]] = val
        return (locals_res, labels)

    def decode_lambdas(self, lambdas, atoms):
        lambda_labels = dict()
        for lam in lambdas:
            val = '%s/%s' % (atoms[lam[0] - 1], lam[1])
            lambda_labels[lam[3]] = val
        return lambda_labels

    def decode_literals(self, literals):
        #TODO
        #return literals
        return 'Not Implemented yet!'
    
    def print_all(self):
        print 'atoms = %s' % self.atoms
        print 
        print 'code = ['
        for func in self.code:
            print '    ['
            for instr in func:
                print '        %s,' % (instr,)
            print '    ],'
        print ']'
        print 
        print 'imports = %s' % self.imports
        print 
        print 'exports = %s' % self.exports
        print 
        print 'locals_ = %s' % self.locals
        print 
        print 'literals = %s' % self.literals


if __name__ == '__main__':
    beam_decoder = BeamDecoder('recursive.beam')
    beam_decoder.decode()
    beam_decoder.print_all()