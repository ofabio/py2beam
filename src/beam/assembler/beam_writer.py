# -*- coding: utf-8 -*-

import sys
import os
dirname = os.path.dirname
BEAM_FOLDER = dirname(dirname(__file__))
sys.path.append(BEAM_FOLDER)

import struct as s
from common.hexByteConversion import ByteToHex
from common.hexByteConversion import HexToByte
import zlib
from external_term_parser import *

def pack_int(i):
    return s.pack(">i", i)
    
#def pack_short(short):
#    return s.pack(">h", short)
    
def pack_string(string):
    return s.pack(">%ss" % len(string), string)
    
class BeamWriter:
    def __init__(self):
        self.atom = []
        self.code = []
        self.strT = []
        self.impT = []
        self.expT = []
        self.funT = []
        self.litT = []
        self.locT = []
        self.attr = []
        self.cInf = []
        self.abst = []
        self.line = []
        
    def build_binary(self):
        atom = self.get_atom()
        code = self.get_code()
        strT = self.get_strT()
        impT = self.get_impT()
        expT = self.get_expT()
        funT = self.get_funT()
        litT = self.get_litT()
        locT = self.get_locT()
        attr = self.get_attr()
        cInf = self.get_cInf()
        abst = self.get_abst()
        line = self.get_line()
        header = self.get_header(atom, code, strT, impT, expT, funT, litT, locT, attr, cInf,
                                 abst, line)
        
        binary = header + atom + code + strT + impT + expT + funT + litT + locT + attr + \
                 cInf + abst + line
        
        '''print 'Header:'
        print ByteToHex(header)
        print 'Atom:'
        print ByteToHex(atom)
        print 'Code:'
        print ByteToHex(code)
        print 'String Table:'
        print ByteToHex(strT)
        print 'Import Table:'
        print ByteToHex(impT)
        print 'Export Table:'
        print ByteToHex(expT)
        print 'Lambda Table:'
        print ByteToHex(funT)
        print 'Literal Table:'
        print ByteToHex(litT)
        print 'Local Function Table:'
        print ByteToHex(locT)
        print 'Attribute:'
        print ByteToHex(attr)
        print 'Compilation Information:'
        print ByteToHex(cInf)
        print 'Abst:'
        print ByteToHex(abst)
        print 'Line:'
        print ByteToHex(line)'''
        
        self.binary = binary
        
    def write(self, file_name):
        fp = open(file_name, 'wb')
        fp.write(self.binary)
        fp.close()
        
    def get_header(self, *args):
        len_other = reduce(lambda x, y: x+len(y), args, 0)
        return pack_string('FOR1') + pack_int(len_other + 4) + pack_string('BEAM')
        
    def get_atom(self):
        # atom list
        al = str()
        for atom in self.atom:
            al += pack_int(len(atom))[-1]
            al += pack_string(atom)
        # attach num atoms
        r = pack_int(len(self.atom)) + al
        # attach id, chunk size
        r = pack_string('Atom') + pack_int(len(r)) + r
        # add padding
        r = self.add_padding(r)
        return r
        
    def get_code(self):
        # id, size, subsize, instruction_set, opcode_max, n_labels, n_functions
        # instruction list
        il = str()
        opcode_max = 0
        n_labels = 1
        for fun in self.code:
            for instr in fun:
                opcode = instr[0]
                # some annotation
                if opcode > opcode_max:
                    opcode_max = opcode
                if opcode == 1:
                    n_labels += 1
                il += pack_int(opcode)[-1]
                for param in instr[1]:
                    il += pack_int(param)[-1]
        # attach id, size, subsize, instruction_set, opcode_max, n_labels, n_functions
        r = pack_string('Code') + pack_int(len(il) + 20) + pack_int(16) + pack_int(0) + \
            pack_int(opcode_max) + pack_int(n_labels) + pack_int(len(self.code)) + il
        # add padding
        r = self.add_padding(r)
        return r
        
    def get_strT(self):
        return pack_string('StrT') + pack_int(0)
        
    def get_impT(self):
        il = str()
        for imp in self.impT:
            for p in imp:
                il += pack_int(p)
        r = pack_string('ImpT') + pack_int(len(il) + 4) + pack_int(len(self.impT)) + il
        return r
        
    def get_expT(self):
        el = str()
        for exp in self.expT:
            for p in exp:
                el += pack_int(p)
        r = pack_string('ExpT') + pack_int(len(el) + 4) + pack_int(len(self.expT)) + el
        return r
    
    def get_funT(self):
        fl = str()
        for fun in self.funT:
            for p in fun:
                fl += pack_int(p)
        r = pack_string('FunT') + pack_int(len(fl) + 4) + pack_int(len(self.funT)) + fl
        return r
    
    def get_litT(self):
        # id, size, uncompressed_size, compresso(n_literals, foreach(size, external_format)), padding
        
        #zlib.decompress('\x78\x9C\x63\x60\x60\x60\x64\x60\x60\x60\x6F\xCE\x66\x60\xAE\x4C
        #\x2D\x06\x00\x09\x1E\x02\x4B\x00\x4C\x6F\x63\x54\x00\x00\x00\x04\x00\x00\x00\x00')
        #->   '\x00\x00\x00\x01\x00\x00\x00\x07\x83k\x00\x03yes'
        
        # [String('yes'), List(List(Integer(5000), Tuple(Integer(1), Integer(2), Integer(3)))), String('~w')]
        #'\x00\x00\x00\x03\x00\x00\x00\x07\x83k\x00\x03yes\x00\x00\x00\x1a\x83l\x00\x00\x00\x01l\x00\x00\x00\x02b\x00\x00\x13\x88h\x03a\x01a\x02a\x03jj\x00\x00\x00\x06\x83k\x00\x02~w'
        
        ll = str()
        for literal in self.litT:
            res = literal.parse()
            ll += pack_int(len(res)) + res
        zipping = pack_int(len(self.litT)) + ll
        uncompressed_size = len(zipping)
        zipped = zlib.compress(zipping)
        r = pack_string('LitT') + pack_int(len(zipped) + 4) + pack_int(uncompressed_size) + zipped
        
        # add padding
        r = self.add_padding(r)
        return r
    
    def get_locT(self):
        fl = str()
        for fun in self.locT:
            for p in fun:
                fl += pack_int(p)
        r = pack_string('LocT') + pack_int(len(fl) + 4) + pack_int(len(self.locT)) + fl
        return r
        
    def get_attr(self):
        #data = HexToByte('83 6C 00 00 00 01 68 02 64 00 03 76 73 6E 6C 00 00 ' + 
        #'00 01 6E 10 00 FA 7E A2 20 85 A4 C7 F1 AD A0 BE 26 A6 A6 E2 39 6A 6A')
        data = self.attr.parse()
        r = pack_string('Attr') + pack_int(len(data)) + data
        return r
        
    def get_cInf(self):
        #data = HexToByte('83 6C 00 00 00 04 68 02 64 00 07 6F 70 74 69 6F 6E ' +
        #'73 6A 68 02 64 00 07 76 65 72 73 69 6F 6E 6B 00 03 34 2E 38 68 02 64 ' + 
        #'00 04 74 69 6D 65 68 06 62 00 00 07 DC 61 03 61 04 61 0C 61 1F 61 29 ' + 
        #'68 02 64 00 06 73 6F 75 72 63 65 6B 00 46 2F 55 73 65 72 73 2F 66 61 ' + 
        #'62 69 6F 2F 44 65 73 6B 74 6F 70 2F 45 72 6C 61 6E 67 57 6F 72 6C 64 ' + 
        #'2F 62 65 61 6D 20 65 78 70 6C 6F 72 61 74 69 6F 6E 2F 63 6F 64 65 2F ' + 
        #'36 2F 72 65 63 75 72 73 69 76 65 2E 65 72 6C 6A')
        data = self.cInf.parse()
        r = pack_string('CInf') + pack_int(len(data)) + data
        
        # add padding
        r = self.add_padding(r)
        return r
        
    def get_abst(self):
        return pack_string('Abst') + pack_int(0)
        
    def get_line(self):
        #00 00 00 00 00 00 00 00 00 00 00 08 00 00 00 03 00 00 00 00 41 61 71
        #data = HexToByte('00 00 00 00 00 00 00 00 00 00 00 09 00 00 00 05 00 00 00 00 41 51 81 A1 B1')
        data = HexToByte('4C 69 6E 65 00 00 00 22 00 00 00 00 00 00 00 00 00 00 00 11 00 00 00 0A 00 00 00 00 41 81 91 A1 C1 D1 09 11 09 12 09 14 09 15 00 00')
        r = pack_string('Line') + pack_int(len(data)) + data
        # add padding
        r = self.add_padding(r)
        return r
        
    def add_padding(self, r):
        pad = len(r) % 4
        if pad > 0:
            r += pack_int(0)[:(4 - pad)]
        return r
