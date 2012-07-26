class Pippo(object):
    def __setattr__a(self):
        return 6
p = Pippo()
p.a = 5
print p.a
