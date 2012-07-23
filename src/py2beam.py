#!/usr/bin/env python
# -*- coding:utf-8 -*-

import sys
import getopt
from frontend.new_scanner import PyScanner
from frontend.new_parser import PyParser
from code_generator.composer import Composer

help_message = '''
The help message goes here.
'''

class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg


def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        try:
            opts, args = getopt.getopt(argv[1:], "ho:v", ["help", "output="])
        except getopt.error, msg:
            raise Usage(msg)

        # option processing
        for option, value in opts:
            if option == "-v":
                verbose = True
            if option in ("-h", "--help"):
                raise Usage(help_message)
            if option in ("-o", "--output"):
                output = value

    except Usage, err:
        print >> sys.stderr, sys.argv[0].split("/")[-1] + ": " + str(err.msg)
        print >> sys.stderr, "\t for help use --help"
        return 2


if __name__ == "__main__":
    # sys.exit(main())
    main()
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
a = 3
print a
class Pippo:
    a = 5
    def fun1(self):
        print 12
p = Pippo()
p.fun1()
print Pippo.a
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




