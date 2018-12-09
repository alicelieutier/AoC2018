from functools import reduce
import collections
import math
import re

# a and b are tuples of the same length
def manhattan_distance(point_a, point_b):
    return reduce(lambda acc, el: acc + abs(el[0] - el[1]), zip(point_a, point_b), 0)

Coords = collections.namedtuple(
    'Coords',
    ['x', 'y']
)

Range = collections.namedtuple(
    'Range',
    ['min_x', 'min_y', 'max_x', 'max_y']
)

def extract_coords(line):
    numbers = [int(number) for number in re.compile('[0-9]+').findall(line)]
    return Coords(*numbers)

def get_coords(filename):
    file = open(filename)
    return [extract_coords(line) for line in file.readlines()]

def range_of_coordinates(coordinates):
    min_x = reduce(lambda acc, el: min(acc, el.x), coordinates, math.inf)
    min_y = reduce(lambda acc, el: min(acc, el.y), coordinates, math.inf)
    max_x = reduce(lambda acc, el: max(acc, el.x), coordinates, 0)
    max_y = reduce(lambda acc, el: max(acc, el.y), coordinates, 0)
    return Range(min_x, min_y, max_x, max_y)

def all_coordinates_in_range(range_of_c):
    return [
        Coords(x, y)
        for x in range(range_of_c.min_x, range_of_c.max_x + 1)
        for y in range(range_of_c.min_y, range_of_c.max_y + 1)
    ]

def get_closest_from_coords(coords):
    def closure(reference):
        distances = {point: manhattan_distance(reference, point) for point in coords}
        minimum_distance = min(distances.values())
        closest_points = [point[0] for point in distances.items() if point[1] == minimum_distance]
        return closest_points[0] if len(closest_points) == 1 else None
    return closure

def get_areas_for_coords_in_range(coordinates, all_coords, range_of_c):
    """creates a dictionary of areas indexed by ccord from the input"""
    get_closest_point = get_closest_from_coords(coordinates)
    def aux(acc, coord):
        dictionary, infinite_areas = acc
        closest = get_closest_point(coord)
        if closest is not None:
            dictionary.setdefault(closest, set())
            dictionary[closest].add(coord)
            if (coord.x in {range_of_c.min_x, range_of_c.max_x} or
                    coord.y in {range_of_c.min_y, range_of_c.max_y}):
                infinite_areas.add(closest)
        return dictionary, infinite_areas
    return reduce(aux, all_coords, ({}, set()))

def largest_non_infinite_area(areas, infinite_areas):
    def aux(max_size, area):
        key, value = area
        if key not in infinite_areas:
            max_size = max(max_size, len(value))
        return max_size
    return reduce(aux, areas.items(), 0)

def get_sums_for_coords_in_range(coordinates, all_coords):
    def sum_map(coord):
        return reduce(lambda acc, x: acc + manhattan_distance(x, coord), coordinates, 0)
    return map(sum_map, all_coords)

def part1(filename):
    coordinates = get_coords(filename)
    range_of_c = range_of_coordinates(coordinates)
    # range_of_c = Range(min_x=0, min_y=0, max_x=500, max_y=500)
    all_coords = all_coordinates_in_range(range_of_c)
    areas, infinite_areas = get_areas_for_coords_in_range(coordinates, all_coords, range_of_c)
    max_size = largest_non_infinite_area(areas, infinite_areas)
    return max_size

def part2(filename, size):
    coordinates = get_coords(filename)
    range_of_c = range_of_coordinates(coordinates)
    all_coords = all_coordinates_in_range(range_of_c)
    sums_of_distance_to_coords = get_sums_for_coords_in_range(coordinates, all_coords)
    size_of_area = reduce(
        lambda acc, sum: acc + 1 if sum < size else acc,
        sums_of_distance_to_coords,
        0
    )
    return size_of_area

print(part1('day6/input_test') == 17)
print(part1('day6/input') == 4186)

print(part2('day6/input_test', 32) == 16)
print(part2('day6/input', 10000) == 45509)
