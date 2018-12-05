# --- Part Two ---
# Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.
#
# The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:
#
# abcde
# fghij
# klmno
# pqrst
# fguij
# axcye
# wvxyz
# The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.
#
# What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)
#
from functools import reduce

def getLines(filename):
    f = open(filename)
    return [line.strip() for line in f.readlines()]

def removeLetterAtX(x):
    def cloture(word):
        return word[:x] + word[x+1:]
    return cloture

def findDuplicates(enumerable):
    def compare(acc, element):
        duplicates, set = acc
        if element in set:
            duplicates.add(element)
        set.add(element)
        return (duplicates, set)
    return reduce(compare, enumerable, (set(), set()))

def lettersRemaining(ids):
    for x in range(0, len(ids[0])):
        modifiedIds = map(removeLetterAtX(x) , ids)
        duplicates, _ = findDuplicates(modifiedIds)
        if len(duplicates) > 0:
            return duplicates.pop()
    return None

#same but pure function
def lettersRemaining(ids):
    def tryWithPosition(x):
        modifiedIds = map(removeLetterAtX(x) , ids)
        duplicates, _ = findDuplicates(modifiedIds)
        if len(duplicates) > 0:
            return duplicates.pop()
        return None
    results = map(tryWithPosition, range(0, len(ids[0])))
    return reduce(lambda x, y : x or y, results, None)

ids = getLines('day2/input')
print(lettersRemaining(ids)) #rteotyxzbodglnpkudawhijsc
