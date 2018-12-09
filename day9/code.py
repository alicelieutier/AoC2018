from functools import reduce
import collections

def place_marble(marble, current_marble, marbles):
    new_place = (current_marble + 2) % len(marbles)
    marbles.insert(new_place, marble)
    return new_place, marbles

def remove_marble(current_marble, marbles):
    index_to_remove = (current_marble - 7) % len(marbles)
    marble = marbles.pop(index_to_remove)
    return index_to_remove, marble, marbles

def part1(nb_of_players, highest_marble):
    def play(acc, marble):
        marbles, scores, current_marble, current_player, extra = acc
        if marble % 23 == 0:
            current_marble, extra_marble, marbles = remove_marble(current_marble, marbles)
            scores.setdefault(current_player, 0)
            scores[current_player] += (marble + extra_marble)
            current_player = (current_player + 1) % nb_of_players
        else: # standard case
            current_marble, marbles = place_marble(marble, current_marble, marbles)
            current_player = (current_player + 1) % nb_of_players
        return marbles, scores, current_marble, current_player, extra

    _, scores, _, _, _ = reduce(
        play,
        range(1, highest_marble + 1),
        ([0], {}, 0, 0, False)
    )

    highest_score = max(scores.values())
    return highest_score

# using a linked list for part 2 to make the algo more linear-ish
Marble = collections.namedtuple(
    'Marble',
    ['prevm', 'nextm']
)

def find_place(current_marble, marbles, delta):
    prevm, nextm = None, None
    if delta == 0:
        prevm = current_marble
        nextm = marbles[current_marble].nextm
        return prevm, nextm
    if delta > 0:
        return find_place(marbles[current_marble].nextm, marbles, delta - 1)
    # delta < 0
    return find_place(marbles[current_marble].prevm, marbles, delta + 1)

def insert_in_list(new_marble, prevm, nextm, marbles):
    marbles[new_marble] = Marble(prevm, nextm)
    marbles[prevm] = Marble(marbles[prevm].prevm, new_marble)
    marbles[nextm] = Marble(new_marble, marbles[nextm].nextm)
    return marbles

def remove_from_list(marble_to_remove, marbles):
    prevm, nextm = marbles.pop(marble_to_remove)
    marbles[prevm] = Marble(marbles[prevm].prevm, nextm)
    marbles[nextm] = Marble(prevm, marbles[nextm].nextm)
    return marbles, nextm

def gen_part2():
    marbles = {0: Marble(0, 0)}
    current_marble = 0
    new_marble = 1
    while True:
        # print('{}, {}'.format(new_marble, list_to_string(marbles, current_marble)))
        if new_marble % 23 == 0:
            marble_to_remove, _ = find_place(current_marble, marbles, -7)
            marbles, current_marble = remove_from_list(marble_to_remove, marbles)
            yield new_marble + marble_to_remove
        else:
            prevm, nextm = find_place(current_marble, marbles, 1)
            marbles = insert_in_list(new_marble, prevm, nextm, marbles)
            current_marble = new_marble
        new_marble += 1

# debug
def list_to_string(marbles, start):
    def read_list(marbles, start):
        yield start
        current = marbles[start].next
        while current != start:
            yield current
            current = marbles[current].next
    return '->'.join([str(m) for m in read_list(marbles, start)])

def part2(nb_of_players, highest_marble):
    gen_score = gen_part2()
    players = [0 for _ in range(nb_of_players)]
    current_player = 0
    for _ in range(23, highest_marble + 1, 23):
        score = next(gen_score)
        players[current_player] += score
        current_player = (current_player + 23) % nb_of_players
    highest_score = max(players)
    return highest_score


print(part1(9, 25) == 32)
print(part1(10, 1618) == 8317)
print(part1(13, 7999) == 146373)
print(part1(17, 1104) == 2764)
print(part1(21, 6111) == 54718)
print(part1(30, 5807) == 37305)
# # personal input
# print(part1(468, 71010) == 374287)

print(part2(9, 25) == 32)
print(part2(10, 1618) == 8317)
print(part2(13, 7999) == 146373)
print(part2(17, 1104) == 2764)
print(part2(21, 6111) == 54718)
print(part2(30, 5807) == 37305)
# # # # personal input
# print(part2(468, 71010) == 374287)
# print(part2(468, 7101000) == 3083412635)
