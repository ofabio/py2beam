#!/usr/bin/env python
# encoding: utf-8
"""
examples_creator.py

Created by maxdrift on 2012-07-26.
Copyright (c) 2012 Maxdrift's. All rights reserved.
"""

import sys
import os

TESTFILE = "src/code_generator/test.py"
DESTFOLDER = "examples"

def main():
    rec = False
    test_num = 1
    indent_col = 0
    test_body = ""
    with open(TESTFILE, 'r') as testfile:
        for l in testfile:
            start_mark =  l.find("print 'output would be...'")
            if start_mark != -1:
                indent_col = start_mark
                rec = True
                continue
            if l.find("print '------------'") != -1:
                rec = False
                print "writing test", test_num
                with open(os.path.join(DESTFOLDER, "test%s.py" % test_num), 'w') as newtest:
                    newtest.write(test_body)
                test_body = ""
                test_num += 1
            if rec:
                # print "line:", l
                if l != "":
                    test_body += l[indent_col:]



if __name__ == '__main__':
    main()
    print "done!"

