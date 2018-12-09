import re

def generate_number(filename):
    file = open(filename)
    numbers = re.compile('[0-9]+').findall(file.read())
    return (int(n) for n in numbers)

def consume_node(gen):
    child_count = next(gen)
    metadata_count = next(gen)
    children = [consume_node(gen) for i in range(0, child_count)]
    metadata = [next(gen) for i in range(0, metadata_count)]
    return (children, metadata)

def value(node):
    children, metadata = node
    if not children:
        return sum(metadata)
    return sum((value(children[index-1]) for index in metadata if index - 1 < len(children)))

def sum_all_metadata(node):
    children, metadata = node
    return sum(metadata) + sum((sum_all_metadata(child) for child in children))

def part1(filename):
    gen = generate_number(filename)
    root = consume_node(gen)
    metadata_sum = sum_all_metadata(root)
    return metadata_sum

def part2(filename):
    gen = generate_number(filename)
    root = consume_node(gen)
    result = value(root)
    return result

print(part1('day8/input_test') == 138)
print(part1('day8/input') == 42196)

print(part2('day8/input_test') == 66)
print(part2('day8/input') == 33649)
