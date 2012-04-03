#!/usr/bin/env python
# encoding: utf-8
"""
test.py

Created by Fabio Pricoco on 2012-03-27.
Copyright (c) 2012 ofabio. All rights reserved.
"""

from structures import *
from composer import Composer

def compose():
    composer = Composer('prova', tree)
    composer.generate()
    composer.write()

def test1():
    global tree
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
            Call('b', [Var('c'), Var('d')])]),
        Def('b', ['c', 'd'], [
            Print(Var('c')),
            Print(Var('d')),
        ]), Call('a', []),
    ])

def test2():
    global tree
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
    
def test3():
    global tree
    print '-- TEST 3 --'
    print 'output would be...'

    a = 5
    print a
    for i in range(0, 2):
        print i
        print a
        a = 7
    print a
    print '------------'

    tree = Module([
        Assign('a', 5),
        Print(Var('a')),
        For('i', Range(0, 2), [
            Print(Var('i')),
            Print(Var('a')),
            Assign('a', 7),
        ]),
        Print(Var('a'))
    ])

def test4():
    global tree
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
    
def test5():
    global tree
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

    composer = Composer('fabio', tree)
    composer.generate()
    composer.write()

def test6():
    global tree
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
            Return(Call('add', [Var('a')]))
        ]),
        Print(Call('hello', []))
    ])
    
def test7():
    global tree
    print '-- TEST 7 --'
    print 'output would be...'
    
    a = 5
    def ciao(a):
        print a
    ciao(a)

    print '------------'

    tree = Module([
        Assign('a', 5),
        Def('ciao', ['a'], [
            Print(Var('a'))
        ]),
        Call('ciao', [Var('a')]),
    ])
    
def test8():
    global tree
    print '-- TEST 8 --'
    print 'output would be...'

    print 5 + 7

    print '------------'

    tree = Module([
        Print(Sum(5, 7))
    ])

def test9():
    global tree
    print '-- TEST 9 --'
    print 'output would be...'

    print "ciao " + "fabio"

    print '------------'

    tree = Module([
        Print(Sum("ciao ", "fabio"))
    ])
    
def test10():
    global tree
    print '-- TEST 10 --'
    print 'output would be...'

    a = 5
    if a > 7:
        print ">7"
    else:
        print "else!"

    print '------------'

    tree = Module([
        Assign('a', 5),
        If([Grt(Var('a'), 7)], [Print(">7"), Print("else!")]),
    ])
    
def main():
    test10()
    compose()

if __name__ == '__main__':
    main()

