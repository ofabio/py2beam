class Pippo(object):
    a = 2
    def __getattribute__(self, name):
        return name
print Pippo().a
