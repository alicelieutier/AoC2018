from functools import reduce

def quicksort(sequence, cmp=lambda x, y: x - y):
    """sorts the list, using cmp as a comparison function if provided"""
    if len(sequence) <= 1:
        return sequence
    pivot = sequence[0]
    list_1, list_2 = _partition(sequence[1:], pivot, cmp)
    return quicksort(list_1, cmp) + [pivot] + quicksort(list_2, cmp)

def _partition(sequence, pivot, cmp):
    def aux(acc, element):
        list_1, list_2 = acc
        if cmp(pivot, element) > 0:
            list_1.append(element)
        else:
            list_2.append(element)
        return list_1, list_2
    return reduce(aux, sequence, ([], []))

def frequency(enumerable):
    """returns frequency_counter, max_frequency, most_frequent"""
    def aux(acc, element):
        frequency_counter, max_frequency, most_frequent = acc
        frequency_counter.setdefault(element, 0)
        frequency_counter[element] += 1
        if frequency_counter[element] > max_frequency:
            max_frequency = frequency_counter[element]
            most_frequent = element
        return (frequency_counter, max_frequency, most_frequent)
    return reduce(aux, enumerable, ({}, 0, None))

def find_duplicates(enumerable):
    """returns a set of all duplicates"""
    def aux(acc, element):
        duplicates, seen = acc
        if element in seen:
            duplicates.add(element)
        seen.add(element)
        return (duplicates, seen)
    duplicates, _ = reduce(aux, enumerable, (set(), set()))
    return duplicates

if __name__ == "__main__":
    print(frequency([1, 5, 5]) == ({1: 1, 5: 2}, 2, 5))

    print(find_duplicates([1, 2, 3, 4, 5, 4, 2]) == {2, 4})
    print(find_duplicates([1, 2, 3]) == set())

    print(quicksort([2, 3, 1, 5, 4]) == [1, 2, 3, 4, 5])
