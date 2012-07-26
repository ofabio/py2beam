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
    composer = Composer('.', 'prova', tree)
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
            Debug('context'),
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
    
    i = 1
    for i in range(i, 3):
        print i
    print i
    print '------------'

    tree = Module([
        Assign('i', Int(1)),
        For('i', Range(Var('i'), Int(3)), [
            Print(Var('i')),
        ]),
        Print(Var('i')),
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

    composer = Composer('.', 'fabio', tree)
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

    a = 6
    print a > 7
    if a > 7:
        print ">7"
    else:
        print "<7"
        print "else!"

    print '------------'

    tree = Module([
        Assign('a', Int(6)),
        Print(Gt(Var('a'), Int(7))),
        If([Gt(Var('a'), Int(7))], [
            [Print(Str(">7"))],
            [Print(Str("<7")), Print(Str("else!"))],
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
            # Debug('memory'),
            Return(Var('b')),
        ]),
        Print(Call(Var('pippofun'), [Int(5)])),
        # Call(Var('pippofun'), [Int(5)]),
        # Debug('context'),
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
            return name

    print Pippo().a

    print '------------'

    tree = Module([
        Class('Pippo', [
            Assign('a', Int(2)),
            Def('__getattribute__', ['self', 'name'], [
                Return(Var('name')),
            ]),
        ]),
        # Debug('memory'),
        #Print(Dot(Var('Pippo'), 'a')),
        Print(Dot(Var('Pippo'), 'a')),
        # Print(Int(12)),
        # Debug('memory'),
    ])

def test17():
    global tree
    print '-- TEST 17 --'
    print 'output would be...'

    class Pippo(object):
        a = 2
        def fun1(self, name):
            return name

    print Pippo.fun1(Pippo(), 5)

    print '------------'

    tree = Module([
        Class('Pippo', [
            Assign('a', Int(31)),
            Def('fun1', ['self', 'name'], [
                Return(Var('name')),
            ]),
        ]),
        # Debug('memory'),
        Print(Call(Dot(Var('Pippo'), 'fun1'), [Var('Pippo'), Int(5)] )),
        # Debug('memory'),
    ])

def test18():
    global tree
    print '-- TEST 18 --'
    print 'output would be...'

    class Pippo(object):
        a = 2
        def fun1(self, name):
            return name

    p = Pippo()
    print p.fun1(5)

    print '------------'

    tree = Module([
        Class('Pippo', [
            Assign('a', Int(31)),
            Def('fun1', ['self', 'name'], [
                Return(Var('name')),
            ]),
        ]),
        Assign('p', Call(Var('Pippo'), [] )),
        # Debug('memory'),
        Print(Call(Dot(Var('p'), 'fun1'), [Int(5)] )),
        # Print(Dot(Var('p'), 'fun1')),
        # Debug('memory'),
    ])

def test19():
    global tree
    print '-- TEST 19 --'
    print 'output would be...'

    class Pippo(object):
        a = 2
        def __call__(self):
            return 15
    
    p = Pippo()
    print p()

    print '------------'

    tree = Module([
        Class('Pippo', [
            Assign('a', Int(2)),
            Def('__call__', ['self'], [
                Return(Int(15)),
            ]),
        ]),
        Assign('p', Call(Var('Pippo'), [])),
        # Debug('memory'),
        Print(Call(Var('p'), [])),
    ])

def test20():
    global tree
    print '-- TEST 20 --'
    print 'output would be...'

    print (5).__repr__()

    print '------------'

    tree = Module([
        # Debug('memory'),
        # Call(Dot(Int(5), '__repr__'), []),
        Print(Call(Dot(Int(5), '__repr__'), [])),
        # Call(Dot(Int(5), '__repr__'), []),
        # Debug('memory'),
    ])


def test21():
    global tree
    print '-- TEST 21 --'
    print 'output would be...'

    def pippofun(a):
        print a
    
    a = 8
    pippofun(a)

    print '------------'

    tree = Module([
        Def('pippofun', ['a'], [
            Print(Var('a')),
        ]),
        # Debug('memory'),
        Assign('a', Int(8)),
        Call(Var('pippofun'), [Var('a')]),
    ])
    
def test22():
    global tree
    print '-- TEST 22 --'
    print 'output would be...'

    class Pippo(object):
        def hello(self):
            return 6

    p = Pippo()
    print Pippo
    print p
    print Pippo.hello
    print p.hello

    print '------------'

    tree = Module([
        Class('Pippo', [
            Def('hello', ['self'], [
                Return(Int(6)),
            ]),
        ]),
        Assign('p', Call(Var('Pippo'), [])),
        Print(Var('Pippo')),
        Print(Var('p')),
        Print(Dot(Var('Pippo'), 'hello')),
        # Debug('memory'),
        Print(Dot(Var('p'), 'hello')),
    ])
    
def test23():
    global tree
    print '-- TEST 23 --'
    print 'output would be...'

    class Pippo(object):
        def hello(self):
            return 6
        def __call__(self):
            return self

    p = Pippo()
    print p.hello()
    print p()

    print '------------'

    tree = Module([
        Class('Pippo', [
            Def('hello', ['self'], [
                Return(Int(6)),
            ]),
            Def('__call__', ['self'], [
                Return(Var('self')),
            ]),
        ]),
        Assign('p', Call(Var('Pippo'), [])),
        # Debug('memory'),
        Print(Call(Dot(Var('p'), 'hello'), [])),
        Print(Call(Var('p'), [])),
    ])

def test24():
    global tree
    print '-- TEST 24 --'
    print 'output would be...'

    class Pippo(object):
        def hello(self):
            return 6

    h = Pippo.hello
    print h
    # h(Pippo)

    print '------------'

    tree = Module([
        Class('Pippo', [
            Def('hello', ['self'], [
                Return(Int(6)),
            ]),
        ]),
        Dot(Var('Pippo'), 'hello'),
        Assign('h', Dot(Var('Pippo'), 'hello')),
        Print(Var('h')),
        #TypeError: unbound method hello() must be called with Pippo instance as first argument (got nothing instead)
        # Debug('memory'),
        Call(Var('h'), [Var('Pippo')]),
    ])

def test25():
    global tree
    print '-- TEST 25 --'
    print 'output would be...'

    # def hello(a,b):
    #     return 5
    # hello()
    
    print '------------'

    tree = Module([
        Def('hello', [], [
            Print(Int(7)),
            Return(Int(5)),
        ]),
        Call(Var('hello'), [Int(1)]),
    ])

def test26():
    global tree
    print '-- TEST 26 --'
    print 'output would be...'

    class Pippo(object):
        def __setattr__a(self):
            return 6
    p = Pippo()
    p.a = 5
    print p.a

    print '------------'

    tree = Module([
        Class('Pippo', [
            Def('__setattr__a', ['self', 'a', 'b', 'c'], [
                Return(Int(6)),
            ]),
        ]),
        Assign('p', Call(Var('Pippo'), [])),
        Assign(Dot(Var('p'), 'a'), Int(5)),
        Print(Dot(Var('p'), 'a')),
        # Debug('memory'),
    ])
    
def test27():
    global tree
    print '-- TEST 27 --'
    print 'output would be...'

    for i in range(0, 5):
        print i
    print '------------'

    tree = Module([
        Range(Int(0), Int(5)),
        Debug('memory'),
    ])
    
def test28():
    global tree
    print '-- TEST 28 --'
    print 'output would be...'
    
    # i = 1
    # class Pippo(object):
    #     for i in range(i, 3):
    #         print i
    #     print i
    print '------------'

    tree = Module([
        Assign('i', Int(1)),
        Class('Pippo', [
            For('i', Range(Var('i'), Int(3)), [
                Print(Var('i')),
            ]),
            Print(Var('i')),
        ])
    ])

def test29():
    global tree
    print '-- TEST 29 --'
    print 'output would be...'
    
    a = 5
    print a > 7

    print '------------'

    tree = Module([
        Assign('a', Int(5)),
        Print(Gt(Var('a'), Int(7))),
    ])
    
def test30():
    global tree
    print '-- TEST 30 --'
    print 'output would be...'
    
    a = 10
    class Pippo:
        print a > 7
        if a > 7:
            print ">7"
            b = 8
        else:
            print "<7"
            print "else!"
        print b

    print '------------'

    tree = Module([
        Assign('a', Int(10)),
        Class('Pippo', [
            Print(Gt(Var('a'), Int(7))),
            If([Gt(Var('a'), Int(7))], [
                [Print(Str(">7")), Assign('b', Int(8))],
                [Print(Str("<7")), Print(Str("else!"))],
            ]),
            Print(Var('b')),
        ]),
        # Debug('memory'),
    ])

def test31():
    global tree
    print '-- TEST 31 --'
    print 'output would be...'

    for i in range(1, 3):
        print i

    print '------------'

    tree = Module([
        For('i', Call(Var('range'), [Int(1), Int(3)]), [
            Print(Var('i')),
        ]),
    ])
    
def test32():
    global tree
    print '-- TEST 32 --'
    print 'output would be...'

    print (((3 + 7) / 2) * 5)

    print '------------'

    tree = Module([
        Print(Mul(Div(Add(Int(3),Int(7)), Int(2)), Int(5))),
    ])
        
def test33():
    global tree
    print '-- TEST 33 --'
    print 'output would be...'

    print 10 < 3
    print 6 < 11

    print '------------'

    tree = Module([
        Print(Lt(Int(10), Int(3))),
        Print(Lt(Int(6), Int(11))),
    ])

def test34():
    global tree
    print '-- TEST 34 --'
    print 'output would be...'

    print 10 == 10
    print 6 == 11

    print '------------'

    tree = Module([
        Print(Eq(Int(10), Int(10))),
        Print(Eq(Int(6), Int(11))),
    ])

def main():
    test34()
    compose()

if __name__ == '__main__':
    main()

