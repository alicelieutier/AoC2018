from functools import reduce
import collections

def placeMarble(marble, current_marble, marbles):
    new_place = (current_marble + 2) % len(marbles)
    marbles.insert(new_place, marble)
    return new_place, marbles

def removeMarble(current_marble, marbles):
    index_to_remove = (current_marble - 7) % len(marbles)
    marble = marbles.pop(index_to_remove)
    return index_to_remove, marble, marbles

def part1(nb_of_players, highest_marble):
    marbles = [0]
    current_marble = 0 #index

    def play(acc, marble):
        marbles, scores, current_marble, current_player, extra = acc
        if marble % 23 == 0:
            current_marble, extra_marble, marbles = removeMarble(current_marble, marbles)
            scores.setdefault(current_player, 0)
            scores[current_player] += (marble + extra_marble)
            current_player = (current_player + 1) % nb_of_players
        else: # standard case
            current_marble, marbles = placeMarble(marble, current_marble, marbles)
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
    ['prev', 'next']
)

def findPlace(current_marble, marbles, delta):
    prev, next = None, None
    if delta == 0:
        prev = current_marble
        next = marbles[current_marble].next
        return prev, next
    elif delta > 0:
        return findPlace(marbles[current_marble].next, marbles, delta - 1)
    else: # delta < 0
        return findPlace(marbles[current_marble].prev, marbles, delta + 1)

def insertInList(new_marble, prev, next, marbles):
    marbles[new_marble] = Marble(prev, next)
    marbles[prev] = Marble(marbles[prev].prev, new_marble)
    marbles[next] = Marble(new_marble, marbles[next].next)
    return marbles

def removeFromList(marble_to_remove, marbles):
    prev, next = marbles.pop(marble_to_remove)
    marbles[prev] = Marble(marbles[prev].prev, next)
    marbles[next] = Marble(prev, marbles[next].next)
    return marbles, next

def genPart2():
    marbles = {0: Marble(0, 0)}
    current_marble = 0
    new_marble = 1
    while True:
        # print('{}, {}'.format(new_marble, listToString(marbles, current_marble)))
        if new_marble % 23 == 0:
            marble_to_remove, _ = findPlace(current_marble, marbles, -7)
            marbles, current_marble = removeFromList(marble_to_remove, marbles)
            yield new_marble + marble_to_remove
        else:
            prev, next = findPlace(current_marble, marbles, 1)
            marbles = insertInList(new_marble, prev, next, marbles)
            current_marble = new_marble
        new_marble += 1

# debug
def listToString(marbles, start):
    def readList(marbles, start):
        yield start
        current = marbles[start].next
        while current != start:
            yield current
            current = marbles[current].next
    return '->'.join([str(m) for m in readList(marbles, start)])

def part2(nb_of_players, highest_marble):
    g = genPart2()
    players = [0 for i in range(nb_of_players)]
    current_player = 0
    for i in range(23, highest_marble + 1, 23):
        score = next(g)
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
