# You've managed to sneak in to the prototype suit manufacturing lab. The Elves are making decent progress, but are still struggling with the suit's size reduction capabilities.
#
# While the very latest in 1518 alchemical technology might have solved their problem eventually, you can do better. You scan the chemical composition of the suit's material and discover that it is formed by extremely long polymers (one of which is available as your puzzle input).
#
# The polymer is formed by smaller units which, when triggered, react with each other such that two adjacent units of the same type and opposite polarity are destroyed. Units' types are represented by letters; units' polarity is represented by capitalization. For instance, r and R are units with the same type but opposite polarity, whereas r and s are entirely different types and do not react.
#
# For example:
#
# In aA, a and A react, leaving nothing behind.
# In abBA, bB destroys itself, leaving aA. As above, this then destroys itself, leaving nothing.
# In abAB, no two adjacent units are of the same type, and so nothing happens.
# In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.
# Now, consider a larger example, dabAcCaCBAcCcaDA:
#
# dabAcCaCBAcCcaDA  The first 'cC' is removed.
# dabAaCBAcCcaDA    This creates 'Aa', which is removed.
# dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
# dabCBAcaDA        No further actions can be taken.
# After all possible reactions, the resulting polymer contains 10 units.
#
# How many units remain after fully reacting the polymer you scanned?
from functools import reduce
import re

input_polymer = open('day5/input').read().strip()
letters = list('abcdefghijklmnopqrstuvwxyz')

def aux(pairs, letter):
    pairs.append('{}{}'.format(letter, letter.upper()))
    pairs.append('{}{}'.format(letter.upper(), letter))
    return pairs

reactingPairs = reduce(aux, letters, [])

def react(polymer):
    for pair in reactingPairs:
        polymer = re.sub(pair, '', polymer)
    return polymer

polymer = input_polymer
reactedPolymer = react(polymer)
while len(polymer) > len(reactedPolymer):
    polymer = reactedPolymer
    reactedPolymer = react(polymer)

reactedLength = len(polymer)
print(reactedLength)

# part 2
min_length = reactedLength
for letter in letters:
    polymer = input_polymer
    polymer = re.sub(letter, '', polymer)
    polymer = re.sub(letter.upper(), '', polymer)
    reactedPolymer = react(polymer)
    while len(polymer) > len(reactedPolymer):
        polymer = reactedPolymer
        reactedPolymer = react(polymer)
    min_length = min(min_length, len(polymer))

print(min_length)
