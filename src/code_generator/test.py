#!/usr/bin/env python
# encoding: utf-8
"""
test.py

Created by Fabio Pricoco on 2012-03-27.
Copyright (c) 2012 ofabio. All rights reserved.
"""

from structures import *
from composer import Composer


def test1():
    print '-- TEST 1 --'
    print 'output would be...'
    
    def a():
        print 1
        c = 5
        d = 8
        b(c, d)
    def b(c, d):
        print c
        print d
    a()
    print '------------'
    
    tree = Module([
        Def('a', [], [
            Print(1),
            Assign('c', 5),
            Assign('d', 8),
            Call('b', ['c', 'd'])]),
        Def('b', ['c', 'd'], [
            Print('c'),
            Print('d'),
        ]), Call('a', []),
    ])
    
    composer = Composer(tree)
    composer.generate()

def test2():
    print '-- TEST 2 --'
    print 'output would be...'
    
    for i in range(1, 6):
        print i
    print '------------'
    
    tree = Module([
        Range(1, 6),
        For('i', [
            Print('i'),
        ])
    ])
    
    composer = Composer(tree)
    composer.generate()

def test3():
    print '-- TEST 3 --'
    print 'output would be...'

    for l in range(0, 2):
        print l
    
    print '------------'
    
    tree = Module([
        Range(0, 2),
        For('l', [
            Print('l')
        ]),
    ])
    
    composer = Composer(tree)
    composer.generate()

def test4():
    print '-- TEST 4 --'
    print 'output would be...'
    
    def a():
        print 5
    for l in range(0, 2):
        a()
        def a():
            print 10
    
    print '------------'
    
    tree = Module([
        Def('a', [], [
            Print(5)
        ]),
        Range(0, 2),
        For('l', [
            Call('a', []),
            Def('a', [], [
                Print(10)
            ])
        ]),
    ])
    
    composer = Composer(tree)
    composer.generate()

def main():
    test4()

if __name__ == '__main__':
    main()

