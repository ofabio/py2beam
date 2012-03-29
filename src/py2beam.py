#!/usr/bin/env python
# -*- coding:utf-8 -*-

from frontend.scanner import PyScanner
from frontend.parser import PyParser
from code_generator.composer import Composer

class Py2Beam(object):
    def __init__(self, lexer = None):
        pass

if __name__ == '__main__':
    scanner = PyScanner()
    code = r'''
def factorial(n):
    if n == 0.0:
        return 1
    elif n > 0.0:
        f = 1.0
        for i in xrange(1, int(n)+1):
            f *= i
        return f
    else:
        return None'''
    code = r"""
a = 5
def call_go():
    b = 7
    print b
call_go()
print a
"""
    print code
    parser = PyParser(lexer=scanner)
    raw_code = parser.parse(code)
    composer = Composer(raw_code)
    composer.generate()
