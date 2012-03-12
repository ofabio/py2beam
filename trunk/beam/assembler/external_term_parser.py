#!/usr/bin/env python
# encoding: utf-8
"""
external_term_parser.py

Created by Fabio Pricoco on 2012-03-06.
Copyright (c) 2012 ofabio. All rights reserved.
"""

import sys
import os
dirname = os.path.dirname
BEAM_FOLDER = dirname(dirname(__file__))
sys.path.append(BEAM_FOLDER)
from common.hexByteConversion import HexToByte, ByteToHex
import struct as s

# 6C -> list
# 68,69 -> tuple
# 64 -> atom
# 6B -> String
# 61,62 -> integer

def pack_int(i):
    return s.pack(">i", i)
    
def pack_short(short):
    return s.pack(">h", short)
    
def pack_string(string):
    return s.pack(">%ss" % len(string), string)
    
class ExternalTerm:
    def __init__(self, obj):
        self.obj = obj
    
    def parse(self):
        return '\x83' + self.obj.parse()

class List:
    # LIST (1,4,N,N2)(108{0x6C},Length,Elements,Tail)
    
    def __init__(self, *args):
        self.content = list(args)
        
    def parse(self):
        content = self.content
        result = str()
        for element in content:
            result += element.parse()
        result += Nil().parse()
        return '\x6C' + pack_int(len(content)) + result

class Tuple:
    # SMALL_TUPLE (1,1,N)(104{0x68},Arity,Elements)
    # LARGE_TUPLE (1,4,N)(105{0x69},Arity,Elements)
    
    def __init__(self, *args):
        self.content = list(args)
        
    def parse(self):
        content = self.content
        result = str()
        for element in content:
            result += element.parse()
        l = len(content)
        if l <= 255:
            return '\x68' + pack_int(l)[-1] + result
        else:
            return '\x69' + pack_int(l) + result
    
class String:
    # STRING (1,2,Len)(107{0x6B},Length,Characters)
    
    def __init__(self, content):
        self.content = content
        
    def parse(self):
        content = self.content
        return '\x6B' + pack_short(len(content)) + pack_string(content)
    
    
class Integer:
    # SMALL_INTEGER (1,1)(97{0x61},Int)
    # INTEGER (1,4)(98{0x62},Int)
    
    def __init__(self, content):
        self.content = content
        
    def parse(self):
        content = self.content
        if content <= 255:
            return '\x61' + pack_int(content)[-1]
        else:
            return '\x62' + pack_int(content)

class Atom:
    # SMALL_ATOM (1,1,Len)(115,Len,AtomName)
    # ATOM (1,2,Len)(100,Len,AtomName)
    
    def __init__(self, content):
        self.content = content
        
    def parse(self):
        content = self.content
        l = len(content)
        #if l <= 255:
        #    return '\x73' + pack_short(l)[-1] + pack_string(content)
        #else:
        #    return '\x64' + pack_short(l) + pack_string(content)
        # ATOM is favourite for some reason
        return '\x64' + pack_short(l) + pack_string(content)

class Float:
    # FLOAT (1,31)(99,FloatString)
    
    def __init__(self, content):
        self.content = content
        
    def parse(self):
        raise Exception('Not implemented yet!')
        content = self.content
        return '\x63'
        
class Binary:
    # BINARY (1,4,Len)(109,Len,Data)
    
    def __init__(self, content):
        self.content = content
    
    def parse(self):
        content = self.content
        return '\x6D' + pack_int(len(content)) + pack_string(content)
        
class Big:
    # SMALL_BIG (1,1,1,n)(110{6E},n,sign,d(0),...,d(n-1))
    # LARGE_BIG (1,4,1,n)(110{6F},n,sign,d(0),...,d(n-1))
    
    def __init__(self, content):
        self.content = content
        
    def parse(self):
        c = self.content
        sign = 0
        if c < 0:
            sign = 1
        c = abs(c)
        result = str()
        while(c != 0):
            r = c % 256
            result += pack_int((r))[-1]
            c = c / 256
        
        return '\x6E' + pack_int(len(result))[-1] + pack_int(sign)[-1] + result

class Nil:
    # NIL (1)(106)
    
    def parse(self):
        return '\x6A'

def test1():
    #[[5000,{1,2,3}]]
    expected = "83 6C 00 00 00 01 6C 00 00 00 02 62 00 00 13 88 68 03 61 01 61 02 61 03 6A 6A"
    obj = ExternalTerm(List(List(Integer(5000), Tuple(Integer(1), Integer(2), Integer(3)))))
    test(expected, obj, 1)
            
def test_attr():
    # Attr
    # FIXME: potrebbe essere necessario un padding se il binario ha lunghezza dispari
    expected = "83 6C 00 00 00 01 68 02 64 00 03 76 73 6E 6C 00 00 00 01 6E 10 00 5D 57 EC 47 EC A1 CF E8 30 58 78 FA 13 76 CA 07 6A 6A"
    obj = ExternalTerm(List(Tuple(Atom('vsn'), List(Big(10355834843103504983582285655618377565L)))))
    test(expected, obj, 'attr')

def test_big():
    input_ = 10355834843103504983582285655618377565L
    big = Big(input_)
    parsed = big.parse()
    expected = HexToByte('6E 10 00 5D 57 EC 47 EC A1 CF E8 30 58 78 FA 13 76 CA 07')
    try:
        assert parsed == expected
    except:
        print '------'
        print 'ERROR on "big" test\n'
        print 'parsed:'
        print ByteToHex(parsed)
        print '\nwhile expected:'
        print ByteToHex(expected)
        print '------'
        
def test_cInf():
    # CInf
    expected = '83 6C 00 00 00 04 68 02 64 00 07 6F 70 74 69 6F 6E 73 6A 68 02 64 00 07 76 65 72 73 69 6F 6E 6B 00 03 34 2E 38 68 02 64 00 04 74 69 6D 65 68 06 62 00 00 07 DC 61 03 61 08 61 15 61 01 61 1C 68 02 64 00 06 73 6F 75 72 63 65 6B 00 49 2F 55 73 65 72 73 2F 66 61 62 69 6F 2F 50 72 6F 67 72 61 6D 6D 61 7A 69 6F 6E 65 2F 45 72 6C 61 6E 67 57 6F 72 6C 64 2F 62 65 61 6D 20 65 78 70 6C 6F 72 61 74 69 6F 6E 2F 63 6F 64 65 2F 38 2F 70 72 6F 76 61 2E 65 72 6C 6A'
    obj = ExternalTerm(List(Tuple(Atom('options'),Nil()),Tuple(Atom('version'),String('4.8')),Tuple(Atom('time'),Tuple(Integer(2012),Integer(3),Integer(8),Integer(21),Integer(1),Integer(28))),Tuple(Atom('source'),String('/Users/fabio/Programmazione/ErlangWorld/beam exploration/code/8/prova.erl'))))
    test(expected, obj, "CInf")

def test(expected, etp, label_test):
    expected = HexToByte(expected)
    parsed = etp.parse()
    try:
        assert parsed == expected
    except:
        print '------'
        print 'ERROR on test %s\n' % label_test
        print 'parsed:'
        print ByteToHex(parsed)
        print '\nwhile expected:'
        print ByteToHex(expected)
        print '------'


if __name__ == '__main__':
    test1()
    test_attr()
    test_big()
    test_cInf()