# The device on your wrist beeps several times, and once again you feel like you're falling.
#
# "Situation critical," the device announces. "Destination indeterminate. Chronal interference detected. Please specify new target coordinates."
#
# The device then produces a list of coordinates (your puzzle input). Are they places it thinks are safe or dangerous? It recommends you check manual page 729. The Elves did not give you a manual.
#
# If they're dangerous, maybe you can minimize the danger by finding the coordinate that gives the largest distance from the other points.
#
# Using only the Manhattan distance, determine the area around each coordinate by counting the number of integer X,Y locations that are closest to that coordinate (and aren't tied in distance to any other coordinate).
#
# Your goal is to find the size of the largest area that isn't infinite. For example, consider the following list of coordinates:
#
# 1, 1
# 1, 6
# 8, 3
# 3, 4
# 5, 5
# 8, 9
# If we name these coordinates A through F, we can draw them on a grid, putting 0,0 at the top left:
#
# ..........
# .A........
# ..........
# ........C.
# ...D......
# .....E....
# .B........
# ..........
# ..........
# ........F.
# This view is partial - the actual grid extends infinitely in all directions. Using the Manhattan distance, each location's closest coordinate can be determined, shown here in lowercase:
#
# aaaaa.cccc
# aAaaa.cccc
# aaaddecccc
# aadddeccCc
# ..dDdeeccc
# bb.deEeecc
# bBb.eeee..
# bbb.eeefff
# bbb.eeffff
# bbb.ffffFf
# Locations shown as . are equally far from two or more coordinates, and so they don't count as being closest to any.
#
# In this example, the areas of coordinates A, B, C, and F are infinite - while not shown here, their areas extend forever outside the visible grid. However, the areas of coordinates D and E are finite: D is closest to 9 locations, and E is closest to 17 (both including the coordinate's location itself). Therefore, in this example, the size of the largest area is 17.
#
# What is the size of the largest area that isn't infinite?

from functools import reduce
import collections
import math
import re

# a and b are tuples of the same length
def manhattanDistance(a, b):
    return reduce(lambda acc, el : acc + abs(el[0] - el[1]), zip(a, b), 0)

Coords = collections.namedtuple(
    'Coords',
    ['x', 'y']
)

Range = collections.namedtuple(
    'Range',
    ['min_x', 'min_y', 'max_x', 'max_y']
)

def extractCoords(line):
    numbers = [int(number) for number in re.compile('[0-9]+').findall(line)]
    return Coords(*numbers)

def getCoords(filename):
    f = open(filename)
    return [extractCoords(line) for line in f.readlines()]

def rangeOfCoordinates(coordinates):
    min_x = reduce(lambda acc, el: min(acc, el.x), coordinates, math.inf)
    min_y = reduce(lambda acc, el: min(acc, el.y), coordinates, math.inf)
    max_x = reduce(lambda acc, el: max(acc, el.x), coordinates, 0)
    max_y = reduce(lambda acc, el: max(acc, el.y), coordinates, 0)
    return Range(min_x, min_y, max_x, max_y)

def allCoordinatesInRange(range_of_c):
    return [
        Coords(x, y)
        for x in range(range_of_c.min_x, range_of_c.max_x + 1)
        for y in range(range_of_c.min_y, range_of_c.max_y + 1)
    ]

def getClosestFromCoords(coords):
    def closure(reference):
        distances = {point: manhattanDistance(reference, point) for point in coords}
        minimum_distance = min(distances.values())
        closestPoints = [point[0] for point in distances.items() if point[1] == minimum_distance]
        return closestPoints[0] if len(closestPoints) == 1 else None
    return closure

# create a dictionary of areas indexed by ccord from the input
def getAreasForCoordsInRange(coordinates, all_coords, range_of_c):
    getClosestPoint = getClosestFromCoords(coordinates)
    def aux(acc, el):
        dictionary, infinite_areas = acc
        closest = getClosestPoint(el)
        if closest is not None:
            dictionary.setdefault(closest, set())
            dictionary[closest].add(el)
            if (el.x in {range_of_c.min_x, range_of_c.max_x} or
               el.y in {range_of_c.min_y, range_of_c.max_y}):
               infinite_areas.add(closest)
        return dictionary, infinite_areas
    return reduce(aux, all_coords, ({}, set()))

def largestNonInfiniteArea(areas, infinite_areas):
    def aux(max_size, area):
        key, value = area
        if key not in infinite_areas:
            max_size = max(max_size, len(value))
        return max_size
    return reduce(aux, areas.items(), 0)

def part1(filename):
    coordinates = getCoords(filename)
    range_of_c = rangeOfCoordinates(coordinates)
    # range_of_c = Range(min_x=0, min_y=0, max_x=500, max_y=500)
    all_coords = allCoordinatesInRange(range_of_c)
    areas, infinite_areas = getAreasForCoordsInRange(coordinates, all_coords, range_of_c)
    max_size = largestNonInfiniteArea(areas, infinite_areas)
    return max_size


print(part1('day6/input'))
