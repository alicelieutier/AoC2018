import re

def generateNumber(filename):
    f = open(filename)
    numbers = re.compile('[0-9]+').findall(f.read())
    return (int(n) for n in numbers)

def consumeNode(gen):
    child_count = next(gen)
    metadata_count = next(gen)
    children = [consumeNode(gen) for i in range(0, child_count)]
    metadata = [next(gen) for i in range(0, metadata_count)]
    return (children, metadata)

def value(node):
    children, metadata = node
    if len(children) == 0:
        return sum(metadata)
    else:
        return sum((value(children[index-1]) for index in metadata if index - 1 < len(children)))

def sumAllMetadata(node):
    children, metadata = node
    return sum(metadata) + sum((sumAllMetadata(child) for child in children))

def part1(filename):
    gen = generateNumber(filename)
    root = consumeNode(gen)
    metadata_sum = sumAllMetadata(root)
    print(metadata_sum)
    return metadata_sum

def part2(filename):
    gen = generateNumber(filename)
    root = consumeNode(gen)
    result = value(root)
    print(result)
    return result

print(part1('day8/input_test') == 138)
print(part1('day8/input') == 42196)

print(part2('day8/input_test') == 66)
print(part2('day8/input') == 33649)
