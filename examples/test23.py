class Pippo(object):
    def hello(self):
        return 6
    def __call__(self):
        return self
p = Pippo()
print p.hello()
print p()
