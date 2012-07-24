# -*- coding: utf-8 -*-

import sys
import os

# root relativa al modulo corrente
dirname = os.path.dirname
BEAM_FOLDER = dirname(dirname(__file__))
sys.path.append(BEAM_FOLDER)

from common.hexByteConversion import ByteToHex
import struct
import zlib

class BeamReader:
    
    def __init__(self, beam_file, verbose=True):
        self.verbose = verbose
        fp = open(beam_file, "rb")
        self.beam = fp.read()
        fp.close
        
        self.load_opcodes()
        
#        self.external_formats = {
#            107: {'name': 'string', 'fields': [{'size': '2', 'name': 'length'}, 
#                                                {'size': 'length', 'name': 'characters'}]}
#        }
        
    def load_opcodes(self):
        self.commands = dict()
        for line in open(os.path.join(BEAM_FOLDER, 'common', 'opcodes.txt') ,'r'):
            s = line.split('->')
            dx = s[1].strip(' ()\n').split(',')
            opname = dx[0].rstrip()
            n_params = int(dx[1].lstrip())
            self.commands[int(s[0].strip())] = {'opname': opname,
                                                'n_params': n_params}
    
    
    def get_atoms(self):
        return self.beam_parsed['Atom']['atoms']
        
    def get_code(self):
        return self.beam_parsed['Code']['code']
        
    def get_imports(self):
        return self.beam_parsed['ImpT']['imports']

    def get_exports(self):
        return self.beam_parsed['ExpT']['exports']
    
    def get_lambdas(self):
        if self.beam_parsed['FunT'] == None:
            return None
        return self.beam_parsed['FunT']['lambdas']

    def get_literals(self):
        pass
        #return self.beam_parsed['LitT']['code']
        
    def get_locals(self):
        return self.beam_parsed['LocT']['functions']
    
    # parse methods
    
    def parse(self):
        beam_parsed = dict()
        # beam_parsed["header"] = self.parse_header()
        beam_parsed["Atom"] = self.parse_atoms()
        beam_parsed["StrT"] = self.parse_string()
        beam_parsed["ImpT"] = self.parse_iemport('import')
        beam_parsed["ExpT"] = self.parse_iemport('export')
        beam_parsed["FunT"] = self.parse_lambda()
        #beam_parsed["LitT"] = self.parse_literal()
        beam_parsed["LocT"] = self.parse_function()
        beam_parsed["Attr"] = self.parse_attribute()
        self.beam_parsed = beam_parsed
        beam_parsed["Code"] = self.parse_code()
        self.beam_parsed = beam_parsed

    def parse_header(self):
        pass

    def parse_atoms(self):
        # start from "Atom"
        atoms_chunk = dict()
        b = self.beam
        gen = self.gen_byte(b[b.find("\x41\x74\x6F\x6D"):])
        
        id_ = self.read_word(gen)
        size = self.read_int(gen)
        n_atoms = self.read_int(gen)

        atoms = list()
        for i in range(n_atoms):
            atom_length = self.read_byte_value(gen)
            atoms.append(self.read(gen, atom_length))
            
        if self.verbose:
            atoms_chunk['id'] = id_
            atoms_chunk['size'] = size
            atoms_chunk['n_atoms'] = n_atoms
        atoms_chunk['atoms'] = atoms
        
        return atoms_chunk
    
    def parse_code(self):
        # start from "Code"
        code_chunk = dict()
        b = self.beam
        gen = self.gen_byte(b[b.find("\x43\x6F\x64\x65"):])
        
        id_ = self.read_word(gen)
        size = self.read_int(gen)
        subsize = self.read_int(gen)
        instruction_set = self.read_int(gen)
        opcode_max = self.read_int(gen)
        n_labels = self.read_int(gen)
        n_functions = self.read_int(gen)

        # get instructions
        #n_label = 0
        len_code = size - subsize - 4
        n_letti = 0
        commands = list()
        while n_letti < len_code:
            opcode = self.read_byte_value(gen)
            n_letti += 1
            params = list()
            if opcode == 0:
                break
            #print opcode
            if not opcode in self.commands:
                print 'opcode unknown: %s' % opcode
                continue
            for i in range(self.commands[opcode]['n_params']):
                l = self.read_byte_value(gen)
                params.append(l)
                n_letti += 1
                if l in (8, 9, 13):
                    params.append(self.read_byte_value(gen))
                    n_letti += 1
            # se l'opcode è una def di funzione di ordine superiore:
            if opcode == 2 and params[-2] == 10:
                params.append(self.read_byte_value(gen))
                n_letti += 1
            #if opcode == 1:
                #n_label += 1
                #if n_label > 15:
            #        params.append(self.read_byte_value(gen))
            #        n_letti += 1
            #print params
            #params = self.resolve_pointer(opcode, params)
            commands.append({'opname': self.commands[opcode]['opname'],
                             'params': params})
        
        if self.verbose:
            code_chunk['id'] = id_
            code_chunk['size'] = size
            code_chunk['subsize'] = subsize
            code_chunk['instruction_set'] = instruction_set
            code_chunk['opcode_max'] = opcode_max
            code_chunk['n_labels'] = n_labels
            code_chunk['n_functions'] = n_functions
        code_chunk['code'] = commands
        
        return code_chunk
        
#    def resolve_pointer(self, opcode, i, val):
#        if opcode == 1 or opcode == 153:
#            return val / 16
#        elif opcode == 2:
#            if i in (0, 1):
#                return self.beam_parsed['Atom']['atoms'][(val - 2) / 16 - 1]
#            elif i == 2:
#                return val / 16
#        elif opcode == 64:
#                
#        return val
#        raise Exception('some handled case')

    def resolve_pointer(self, opcode, params):
        if not params:
            return []
        if params[0] == 71:
            # for literal in self.beam_parsed['LitT']['literals']:
 #                   if 'characters' in literal['params']: 
#                        print "%s: %s" % (literal['params']['characters'], literal['name'])
            literals = self.beam_parsed['LitT']['literals']
            params[0] = literals[params[1] / 16]['params']['characters']
            params[1] = '(x,0)'
        else:
            params = map(self.standard_resolution, params)
        return params
        
    def standard_resolution(self, param):
        if param % 16 == 0:
            return param / 16
        elif param % 16 == 2:
            return self.beam_parsed['Atom']['atoms'][(param - 2) / 16 - 1]
        elif param % 16 == 3:
            return "(x,%s)" % ((param - 3) / 16)
        raise Exception('some handled case')
            
        
    def parse_string(self):
        # start from "StrT"
        string_chunk = dict()
        b = self.beam
        gen = self.gen_byte(b[b.find("\x53\x74\x72\x54"):])
        
        id_ = self.read_word(gen)
        size = self.read_int(gen)
        # da completare una volta visto in funzione
        if self.verbose:
            string_chunk['id'] = id_
            string_chunk['size'] = size
        
        return string_chunk
        
    def parse_iemport(self, switch):
        # start from "Impt"
        iemport_chunk = dict()
        b = self.beam
        if switch == 'import':
            gen = self.gen_byte(b[b.find("\x49\x6D\x70\x54"):])
        elif switch == 'export':
            gen = self.gen_byte(b[b.find("\x45\x78\x70\x54"):])
            
        id_ = self.read_word(gen)
        size = self.read_int(gen)
        n_entry = self.read_int(gen)
        # da completare una volta visto in funzione

        iemports = list()
        for i in range(n_entry):
            iemports.append([self.read_int(gen),
                            self.read_int(gen),
                            self.read_int(gen)])

        if self.verbose:
            iemport_chunk['id'] = id_
            iemport_chunk['size'] = size
            iemport_chunk['n_entry'] = n_entry        
        if switch == 'import':
            iemport_chunk['imports'] = iemports
        elif switch == 'export':
            iemport_chunk['exports'] = iemports
            
        return iemport_chunk
        
    def parse_lambda(self):
        # start from "FunT"
        lambda_chunk = dict()
        b = self.beam
        gen = self.gen_byte(b[b.find("\x46\x75\x6E\x54"):])
        
        try:
            id_ = self.read_word(gen)
        except StopIteration:
            return None
        size = self.read_int(gen)
        n_entry = self.read_int(gen)
        
        lambdas = list()
        for i in range(n_entry):
            lambdas.append([self.read_int(gen),
                            self.read_int(gen),
                            self.read_int(gen),
                            self.read_int(gen),
                            self.read_int(gen),
                            self.read_int(gen)])
        if self.verbose:
            lambda_chunk['id'] = id_
            lambda_chunk['size'] = size
            lambda_chunk['n_entry'] = n_entry
        lambda_chunk['lambdas'] = lambdas
        
        return lambda_chunk
        
    def parse_literal(self):
        # LilT(4), size_chunk(4), size_uncompressed(4), 
        # compresso: (n_literals(4), seq of: [decl_size(4), ExternalTermFormat(1)=131, 
        #             ext_type(1), {other fields depending on the type}])
        # start from "LitT"
        literal_chunk = dict()
        b = self.beam
        gen = self.gen_byte(b[b.find("\x4C\x69\x74\x54"):])
        
        id_ = self.read_word(gen)
        size = self.read_int(gen)
        uncompressed_size = self.read_int(gen)
        
        data = zlib.decompress(self.read(gen, size - 4))
        gen_d = self.gen_byte(data)
        n_literals = self.read_int(gen_d)
        
        decls = list()
        for i in range(n_literals):
            decl_size = self.read_int(gen_d)
            # generator on literal
            gen_l = self.gen_byte(self.read(gen_d, decl_size))
            literal = self.read_literal(gen_l)
            literal['size'] = decl_size
            decls.append(literal)
                
        if self.verbose:
            literal_chunk['id'] = id_
            literal_chunk['size'] = size
            literal_chunk['uncompressed_size'] = uncompressed_size
            literal_chunk['n_literals'] = n_literals
        literal_chunk['literals'] = decls
        
        return literal_chunk
        
    def read_literal(self, gen):
        result = dict()
        #107: {'name': 'string', 'fields': [{'size': '2', 'name': 'length'}, 
        #                                    {'size': 'length', 'name': 'string'}]}
        etf = self.read_byte_value(gen)     # sempre 0x83
        ext_type = self.read_byte_value(gen)
        ef = self.external_formats[ext_type]
        result['etf'] = etf
        result['name'] = ef['name']
        
        result_field = dict()
        for field in ef['fields']:
            if field['name'] == 'length':
                result_field[field['name']] = self.read_short(gen, int(field['size']))
            else:
                # size potrebbe dipendere un altro campo
                if field['size'].isdigit():
                    result_field[field['name']] = \
                            self.read(gen, int(field['size']))
                else:
                    result_field[field['name']] = \
                            self.read(gen, result_field[field['size']])
                    
        result['params'] = result_field
        return result
                    
        
    def parse_function(self):
        # start from "LocT"
        function_chunk = dict()
        b = self.beam
        gen = self.gen_byte(b[b.find("\x4C\x6F\x63\x54"):])
        
        id_ = self.read_word(gen)
        size = self.read_int(gen)
        n_entry = self.read_int(gen)
        
        functions = list()
        for i in range(n_entry):
            functions.append([self.read_int(gen),
                              self.read_int(gen),
                              self.read_int(gen)])
                              
        if self.verbose:
            function_chunk['id'] = id_
            function_chunk['size'] = size
            function_chunk['n_entry'] = n_entry        
        function_chunk['functions'] = functions

        return function_chunk
        
    def parse_attribute(self):
        # start from "Attr"
        attribute_chunk = dict()
        b = self.beam
        gen = self.gen_byte(b[b.find("\x41\x74\x74\x72"):])
        
        id_ = self.read_word(gen)
        size = self.read_int(gen)
        
                              
    # utils method
        
    def read_byte_value(self, gen):
        return ord(self.read(gen, 1))
    
    def read_int(self, gen):
        return struct.unpack(">i", self.read(gen, 4))[0]
        
    def read_short(self, gen, nbytes):
        return struct.unpack(">h", self.read(gen, nbytes))[0]
        
    def read_word(self, gen):
        return self.read(gen, 4)
    
    def read(self, gen, nbytes):
        l = list()
        for i in range(nbytes):
            l.append(gen.next())
        return "".join(l)
    
    def gen_byte(self, bytes):
        for b in bytes:
            yield b
            

    # print methods
    
    def print_atom(self):
        print '----- ATOMS -----'
        print self.beam_parsed['Atom']['atoms']
        
            
    def print_code(self):
        print '----- CODE -----'
        i = 0
        for instruction in self.beam_parsed['Code']['code']:
            i += 1
            print '%2s.  %s: %s' % (i, instruction['opname'],
                                    instruction['params'])
                                    
    def print_import(self):
        print '----- IMPORTS -----'
        for imp in self.beam_parsed['ImpT']['imports']:
            print "module: %s  function: %s  arity: %s" % (tuple(imp))
            
    def print_export(self):
        print '----- EXPORTS -----'
        for exp in self.beam_parsed['ExpT']['exports']:
            print "function: %s  arity: %s  label: %s" % (tuple(exp))
    
    def print_lambda(self):
        print '----- LAMBDA -----'
        print self.beam_parsed['FunT']
        #for exp in self.beam_parsed['FunT']['lambdas']:
        #    print "function: %s  arity: %s  label: %s" % (tuple(exp))
    
    def print_literal(self):
        print '----- LITERALS -----'
        for literal in self.beam_parsed['LitT']['literals']:
            if 'characters' in literal['params']: 
                print "%s: %s" % (literal['params']['characters'], literal['name'])
            else:
                print "%s: %s" % (literal['params'], literal['name'])
        
        if self.verbose:
            print self.beam_parsed['LitT']
            
    def print_function(self):
        print '----- FUNCTIONS -----'
        for exp in self.beam_parsed['LocT']['functions']:
            print "function: %s  arity: %s  label: %s" % (tuple(exp))
        if self.verbose:
            print self.beam_parsed['LocT']
    
                                    
    def print_all(self):
        self.print_atom()
        print ''
        self.print_code()
        print ''
        self.print_import()
        print ''
        self.print_export()
        print ''
        self.print_lambda()
        print ''
        #self.print_literal()
        #print ''
        self.print_function()
        
    def toHex(self, s):
        return '"' + ' '.join([hex(ord(h)) for h in s]) + '"'

if __name__ == '__main__':
    beam_parser = BeamReader("recursive.beam", verbose=True)
    #beam_parser = BeamParser("beam_maker/recursive.beam", verbose=True)
    beam_parser.parse()
    beam_parser.print_all()
    