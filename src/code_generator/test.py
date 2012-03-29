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
            Print(Var('c')),
            Print(Var('d')),
        ]), Call('a', []),
    ])
    
    composer = Composer(tree)
    composer.generate()
    composer.write('prova.beam')

def test2():
    print '-- TEST 2 --'
    print 'output would be...'
    
    for i in range(0, 2):
        print i
    print '------------'
    
    tree = Module([
        For('i', Range(0, 2), [
            Print(Var('i')),
        ])
    ])
    
    composer = Composer(tree)
    composer.generate()
    composer.write('prova.beam')

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
        For('l', Range(0, 2), [
            Call('a', []),
            Def('a', [], [
                Print(10)
            ])
        ]),
    ])
    
    composer = Composer(tree)
    composer.generate()
    composer.write('prova.beam')
    
def test5():
    print '-- TEST 5 --'
    print 'output would be...'

    def hello():
        return 5
    print hello()

    print '------------'

    tree = Module([
        Def('hello', [], [
            Return(5)
        ]),
        Print(Call('hello', []))
    ])

    composer = Composer(tree)
    composer.generate()
    composer.write('prova.beam')

def test6():
    print '-- TEST 6 --'
    print 'output would be...'

    def add(p):
        return p
    def hello():
        a = 5
        return add(a)
    print hello()

    print '------------'

    tree = Module([
        Def('add', ['p'], [
            Return(Var('p'))
        ]),
        Def('hello', [], [
            Assign('a', 5),
            Return(Call('add', ['a']))
        ]),
        Print(Call('hello', []))
    ])

    composer = Composer(tree)
    composer.generate()
    composer.write('prova.beam')

def main():
    test6()

if __name__ == '__main__':
    main()

