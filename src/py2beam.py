#!/usr/bin/env python
# -*- coding:utf-8 -*-

import sys, os, shutil
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
                # print code
                parser = PyParser(lexer=scanner)
                print "compiling %s..." % file_
                tree = parser.parse(code)
                build_dir = os.path.join(out_path, filename)
                if not os.path.exists(build_dir) or not os.path.isdir(build_dir):
                    os.makedirs(build_dir)
                for lib in ("base", "common", "builtins"):
                    # if not os.path.exists(os.path.join(build_dir, "%s.beam" % lib)):
                    # print "inserting %s lib..." % lib
                    shutil.copyfile(os.path.join("src", "code_generator", "%s.beam" % lib), os.path.join(build_dir, "%s.beam" % lib))
                composer = Composer(build_dir, beam_name, tree, verbose)
                composer.generate()
                composer.write()
                if execute:
                    print 79 * "#"
                    print 33*" " + "NOW EXECUTING"
                    print 79 * "#"
                    # erl -pa ./ -run prova module -run init stop -noshell
                    call(["erl", "-pa", build_dir, "-run", beam_name, "module", "-run", "init", "stop", "-noshell"])
                    print 79 * "#"
    except Usage, err:
        # print >> sys.stderr, sys.argv[0].split("/")[-1] + ": " + str(err.msg)
        print >> sys.stderr, str(err.msg)
        # print >> sys.stderr, "\t for help use --help"
        return 2


if __name__ == "__main__":
    sys.exit(main())




