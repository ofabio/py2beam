class Pippo(object):
    a = 2
    def __call__(self):
        return 15

p = Pippo()
print p()
