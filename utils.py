
def getLines(filename):
    f = open(filename)
    return [line.strip() for line in f.readlines()]
