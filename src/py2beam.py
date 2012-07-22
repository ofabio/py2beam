#!/usr/bin/env python
# -*- coding:utf-8 -*-

from frontend.new_scanner import PyScanner
from frontend.new_parser import PyParser
from code_generator.composer import Composer

class Py2Beam(object):
    def __init__(self, lexer = None):
        pass

if __name__ == '__main__':
    beam_name = "prova"
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
a = 230
def call_go():
    b = 7
    #print b
call_go()
print a
#print 3
"""
    code = r"""
class Pippo:
    print 4
def fun1():
    print 12
fun1()
"""
    print code
    parser = PyParser(lexer=scanner)
    tree = parser.parse(code)
    composer = Composer(beam_name, tree)
    composer.generate()
    composer.write()
    from subprocess import call
    # erl -pa ./ -run prova module -run init stop -noshell
    # call(["erl", "-pa", "./code_generator", "-run", "prova", "module", "-run", "init", "stop", "-noshell"])






