class Pippo(object):
    a = 2
    def fun1(self, name):
        return name
print Pippo.fun1(Pippo(), 5)
