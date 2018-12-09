from functools import reduce

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
            scores.setdefault(current_player, 0)
            scores[current_player] += marble
            extra = True
        if extra is True:
            extra = False
            current_marble, extra_marble, marbles = removeMarble(current_marble, marbles)
            scores[current_player] += extra_marble
            current_player = (current_player + 1) % nb_of_players
        else: # standard case
            current_marble, marbles = placeMarble(marble, current_marble, marbles)
            current_player = (current_player + 1) % nb_of_players
        return marbles, scores, current_marble, current_player, extra

    _, scores, _, _, _ = reduce(
        play,
        range(1, highest_marble),
        ([0], {}, 0, 0, False)
    )

    highest_score = max(scores.values())
    print(highest_score)
    return highest_score



print(part1(9, 25) == 32)
# other test inputs
print(part1(10, 1618) == 8317)
print(part1(13, 7999) == 146373)
print(part1(17, 1104) == 2764)
print(part1(21, 6111) == 54718)
print(part1(30, 5807) == 37305)
# # personal input
print(part1(468, 71010) == 374287)
