import re
import collections
from functools import reduce

# example line
# #1166 @ 449,278: 21x10

def extractNumbers(string):
    return [int(nb) for nb in re.compile('[0-9]+').findall(string)]

# Claim type
Claim = collections.namedtuple(
    'Claim',
    ['id', 'fromLeft', 'fromTop', 'width', 'height']
)

# Square inch
Coords = collections.namedtuple(
    'Coords',
    ['x', 'y']
)

# returns tuple as (id, fromLeft, fromTop, width, height)
def parseAsClaim(line):
    return Claim(*extractNumbers(line))

def findDuplicates(enumerable):
    def aux(acc, element):
        duplicates, seen = acc
        if element in seen:
            duplicates.add(element)
        seen.add(element)
        return (duplicates, seen)
    return reduce(aux, enumerable, (set(), set()))

# returns list of coords for claim
def coordList(claim):
    coords = []
    for i in range(claim.fromLeft, claim.fromLeft + claim.width):
        for j in range(claim.fromTop, claim.fromTop + claim.height):
            coords.append(Coords(i, j))
    return coords

if __name__ == '__main__':
    f = open('input')
    lines = [line.strip() for line in f.readlines()]
    claims = [parseAsClaim(line) for line in lines]
    # turn claims to collections of coords
    claims = [coordList(claim) for claim in claims]
    # flatten list of Coords
    coords = [coord for claimCoords in claims for coord in claimCoords]
    # find duplicates
    duplicates, _ = findDuplicates(coords)
    # part one: return number of duplicates
    print(len(duplicates))
    # part two: return the claim that does not overlap
    overlap = True
    claimId = 0
    for claim in [parseAsClaim(line) for line in lines]:
        if not overlap:
            break
        overlap = False
        claimId = claim.id
        for coord in coordList(claim):
            if coord in duplicates:
                overlap = True
                break
    print(claimId)
