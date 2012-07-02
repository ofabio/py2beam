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
            Print(Int(1)),
            Assign('c', Int(5)),
            Assign('d', Int(8)),
            Call(Var('b'), [Var('c'), Var('d')])]),
        Def('b', ['c', 'd'], [
            Print(Var('c')),
            Print(Var('d')),
        ]), Call(Var('a'), []),
    ])

def test2():
    global tree
    print '-- TEST 2 --'
    print 'output would be...'
    
    for i in range(0, 2):
        print i
    print '------------'
    
    tree = Module([
        For('i', Range(Int(0), Int(2)), [
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
        Assign('a', Int(5)),
        Print(Var('a')),
        For('i', Range(Int(0), Int(2)), [
            Print(Var('i')),
            Print(Var('a')),
            Assign('a', Int(7)),
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
            Print(Int(5)),
        ]),
        For('l', Range(Int(0), Int(2)), [
            Call(Var('a'), []),
            Def('a', [], [
                Print(Int(10)),
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
            Return(Int(5))
        ]),
        Print(Call(Var('hello'), []))
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
            Assign('a', Int(5)),
            Return(Call(Var('add'), [Var('a')]))
        ]),
        Print(Call(Var('hello'), []))
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
        Assign('a', Int(5)),
        Def('ciao', ['a'], [
            Print(Var('a'))
        ]),
        Call(Var('ciao'), [Var('a')]),
    ])
     
def test8():
    global tree
    print '-- TEST 8 --'
    print 'output would be...'

    print 5 + 7

    print '------------'

    tree = Module([
        Print(Add(Int(5), Int(7)))
    ])

def test9():
    global tree
    print '-- TEST 9 --'
    print 'output would be...'

    print "ciao " + "fabio"

    print '------------'

    tree = Module([
        Print(Add(Str("ciao "), Str("fabio")))
    ])
    
def test10():
    global tree
    print '-- TEST 10 --'
    print 'output would be...'

    a = 9
    if a > 7:
        print ">7"
    else:
        print "->"
        print "else!"

    print '------------'

    tree = Module([
        Assign('a', Int(9)),
        If([Gt(Var('a'), Str("7"))], [
            [Print(Str(">7"))], 
            [Print(Str("->")), Print(Str("else!"))]
        ]),
    ])
    
def test11():
    global tree
    print '-- TEST 11 --'
    print 'output would be...'

    a = 5
    a = 7
    print a

    print '------------'

    tree = Module([
        Assign('pippo', Int(5)),
        #Debug('memory'),
        Assign('pippo', Int(7)),
        #Debug('memory'),
        Print(Var('pippo')),
        #Sum(Var('pippo'), Var('pluto'))
    ])
    
def test12():
    global tree
    print '-- TEST 12 --'
    print 'output would be...'

    def pippofun(a):
        print a
        b = 7
        return b
    print pippofun(5)

    print '------------'

    tree = Module([
        Def('pippofun', ['a'], [
            Print(Var('a')),
            Assign('b', Int(7)),
            Return(Var('b')),
        ]),
        Print(Call(Var('pippofun'), [Int(5)])),
        #Debug('context'),
        #Debug('memory'),
    ])
    
def test13():
    global tree
    print '-- TEST 13 --'
    print 'output would be...'

    a = 8
    b = a
    print b

    print '------------'

    tree = Module([
        Assign('a', Int(8)),
        Assign('b', Var('a')),
        Print(Var('b')),
        Debug('context'),
        Debug('memory'),
    ])

def test14():
    global tree
    print '-- TEST 14 --'
    print 'output would be...'

    def fun():
        class Pippo:
            a = 5
            print a

    print '------------'

    tree = Module([
        Assign('c', Int(5)),
        Def('pippo_fun', [], [
            Class('Pippo', [
                Assign('a', Var('c')),
                Print(Var('a')),
            ]),
            Debug('memory'),
        ]),
        Debug('memory'),
        Call(Var('pippo_fun'), []),
        Debug('memory'),
    ])
    
def test15():
    global tree
    print '-- TEST 15 --'
    print 'output would be...'

    class Pippo:
        a = 2
        print a
    
    Pippo = 5

    print '------------'

    tree = Module([
        Class('Pippo', [
            Assign('a', Int(2)),
            Print(Var('a')),
        ]),
        Debug('memory'),
        Assign('Pippo', Int(5)),
        Debug('memory'),
    ])

def test16():
    global tree
    print '-- TEST 16 --'
    print 'output would be...'

    class Pippo(object):
        a = 2
        def __getattribute__(self, name):
            return 69

    print Pippo().a

    print '------------'

    tree = Module([
        Class('Pippo', [
            Assign('a', Int(2)),
            Def('__getattribute__', ['self', 'name'], [
                Return(Int(69))
            ]),
        ]),
        Debug('memory'),
        Dot(Var('Pippo'), 'a'),
        Debug('memory'),
    ])



def main():
    test16()
    compose()

if __name__ == '__main__':
    main()

