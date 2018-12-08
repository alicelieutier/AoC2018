def quicksort(list, cmp=lambda x, y: x - y):
    if len(list) <= 1:
        return list
    else:
        pivot = list[0]
        l1, l2 = _partition(list[1:], pivot, cmp)
        return quicksort(l1, cmp) + [pivot] + quicksort(l2, cmp)

def _partition(list, pivot, cmp):
    def aux(acc, el):
        l1, l2 = acc
        if (cmp(pivot, el) > 0):
            l1.append(el)
        else:
            l2.append(el)
        return l1, l2
    return reduce(aux, list, ([],[]))


# returns frequency_counter, max_frequency, most_frequent
def frequency(enumerable):
    def aux(acc, element):
        frequency_counter, max_frequency, most_frequent = acc
        frequency_counter.setdefault(element, 0)
        frequency_counter[element] += 1
        if frequency_counter[element] > max_frequency:
            max_frequency = frequency_counter[element]
            most_frequent = element
        return (frequency_counter, max_frequency, most_frequent)
    return reduce(frequency, enumerable, ({}, 0, None))

# returns a set of all duplicates
def findDuplicates(enumerable):
    def aux(acc, element):
        duplicates, seen = acc
        if element in seen:
            duplicates.add(element)
        seen.add(element)
        return (duplicates, seen)
    duplicates, _ = reduce(aux, enumerable, (set(), set()))
    return duplicates
