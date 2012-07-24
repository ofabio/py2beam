#!/usr/bin/env python
# -*- coding:utf-8 -*-

import sys, os
import getopt
from subprocess import call
from frontend.new_scanner import PyScanner
from frontend.new_parser import PyParser
from code_generator.composer import Composer

help_message = '''\
Usage: py2beam.py [options] <source file>
Options:
  -o, --output= <file>         Place the output into <file>
  -e, --execute                Executes the compiled beam
  -v                           Output messages about what the compiler is doing
  -h, --help                   Display this information
'''

class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg


def main(argv=None):
    if argv is None:
        argv = sys.argv
    execute = False
    verbose = False
    output = None
    try:
        try:
            opts, args = getopt.getopt(argv[1:], "eho:v", ["help", "output=", "execute"])
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
            if option in ("-e", "--execute"):
                execute = True
        
        if args == []:
            raise Usage(help_message)
        
        for arg in args:
            out_path, file_ = os.path.split(arg)
            filename, ext = os.path.splitext(file_)
            if output:
                out_path, out_file = os.path.split(output)
                filename, out_ext = os.path.splitext(out_file)
            code = ""
            with open(arg, 'r') as source:
                for l in source:
                    code += l
            if len(code) > 0:
                beam_name = filename
                scanner = PyScanner()
                print code
                parser = PyParser(lexer=scanner)
                tree = parser.parse(code)
                composer = Composer(out_path, beam_name, tree, verbose)
                composer.generate()
                composer.write()
                if execute:
                    print 78 * "#"
                    print "NOW EXECUTING"
                    print 78 * "#"
                    call(["erl", "-pa", out_path, "-run", beam_name, "module", "-run", "init", "stop", "-noshell"])
                    print 78 * "#"
    except Usage, err:
        # print >> sys.stderr, sys.argv[0].split("/")[-1] + ": " + str(err.msg)
        print >> sys.stderr, str(err.msg)
        # print >> sys.stderr, "\t for help use --help"
        return 2


if __name__ == "__main__":
    sys.exit(main())
    # main()
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
    composer = Composer('code_generator', beam_name, tree)
    composer.generate()
    composer.write()
    from subprocess import call
    # erl -pa ./ -run prova module -run init stop -noshell
    # call(["erl", "-pa", "./code_generator", "-run", "prova", "module", "-run", "init", "stop", "-noshell"])




