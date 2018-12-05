# --- Day 4: Repose Record ---
# You've sneaked into another supply closet - this time, it's across from the prototype suit manufacturing lab. You need to sneak inside and fix the issues with the suit, but there's a guard stationed outside the lab, so this is as close as you can safely get.
#
# As you search the closet for anything that might help, you discover that you're not the first person to want to sneak in. Covering the walls, someone has spent an hour starting every midnight for the past few months secretly observing this guard post! They've been writing down the ID of the one guard on duty that night - the Elves seem to have decided that one guard was enough for the overnight shift - as well as when they fall asleep or wake up while at their post (your puzzle input).
#
# For example, consider the following records, which have already been organized into chronological order:
#
# [1518-11-01 00:00] Guard #10 begins shift
# [1518-11-01 00:05] falls asleep
# [1518-11-01 00:25] wakes up
# [1518-11-01 00:30] falls asleep
# [1518-11-01 00:55] wakes up
# [1518-11-01 23:58] Guard #99 begins shift
# [1518-11-02 00:40] falls asleep
# [1518-11-02 00:50] wakes up
# [1518-11-03 00:05] Guard #10 begins shift
# [1518-11-03 00:24] falls asleep
# [1518-11-03 00:29] wakes up
# [1518-11-04 00:02] Guard #99 begins shift
# [1518-11-04 00:36] falls asleep
# [1518-11-04 00:46] wakes up
# [1518-11-05 00:03] Guard #99 begins shift
# [1518-11-05 00:45] falls asleep
# [1518-11-05 00:55] wakes up
# Timestamps are written using year-month-day hour:minute format. The guard falling asleep or waking up is always the one whose shift most recently started. Because all asleep/awake times are during the midnight hour (00:00 - 00:59), only the minute portion (00 - 59) is relevant for those events.
#
# Visually, these records show that the guards are asleep at these times:
#
# Date   ID   Minute
#             000000000011111111112222222222333333333344444444445555555555
#             012345678901234567890123456789012345678901234567890123456789
# 11-01  #10  .....####################.....#########################.....
# 11-02  #99  ........................................##########..........
# 11-03  #10  ........................#####...............................
# 11-04  #99  ....................................##########..............
# 11-05  #99  .............................................##########.....
# The columns are Date, which shows the month-day portion of the relevant day; ID, which shows the guard on duty that day; and Minute, which shows the minutes during which the guard was asleep within the midnight hour. (The Minute column's header shows the minute's ten's digit in the first row and the one's digit in the second row.) Awake is shown as ., and asleep is shown as #.
#
# Note that guards count as asleep on the minute they fall asleep, and they count as awake on the minute they wake up. For example, because Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.
#
# If you can figure out the guard most likely to be asleep at a specific time, you might be able to trick that guard into working tonight so you can have the best chance of sneaking in. You have two strategies for choosing the best guard/minute combination.
#
# Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?
#
# In the example above, Guard #10 spent the most minutes asleep, a total of 50 minutes (20+25+5), while Guard #99 only slept for a total of 30 minutes (10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas any other minute the guard was asleep was only seen on one day).
#
# While this example listed the entries in chronological order, your entries are in the order you found them. You'll need to organize them before they can be analyzed.
#
# What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 10 * 24 = 240.)

import re
import time

from functools import reduce

def extractData(file):
    return re.compile('\[(.*)\] (.*)\n').findall(file)

def extractGuards(file):
    return set(re.compile(' Guard #([0-9]+)').findall(file))

def stripTime(datestring):
    return time.strptime(datestring, '%Y-%m-%d %H:%M')

def isGuardTurn(event):
    return re.compile('Guard #([0-9]+)').match(event[1])

def isFallsAsleep(event):
    return re.compile('falls asleep').match(event[1])

def isWakesUp(event):
    return re.compile('wakes up').match(event[1])

def guard(event):
    return int(re.compile('Guard #([0-9]+)').findall(event[1])[0])

f = open('day4/input').read()
lines = extractData(f)
guards = extractGuards(f)
events = sorted([(stripTime(line[0]), line[1]) for line in lines])

def createSleepingMinutes(events):
    def aux(acc, event):
        sleeping_minutes, currentGuardId, fellAsleep = acc
        if isGuardTurn(event):
            currentGuardId = guard(event)
        elif isFallsAsleep(event):
            fellAsleep = event[0].tm_min
        else:
            def createMinute(min):
                return (min, currentGuardId)
            sleeping_minutes.extend(map(createMinute, range(fellAsleep, event[0].tm_min)))
        return (sleeping_minutes, currentGuardId, fellAsleep)

    sleeping_minutes, _, _ = reduce(aux, events, ([], 0, 0))
    return sleeping_minutes

def frequency(acc, element):
    frequency_counter, max_frequency, most_frequent = acc
    frequency_counter.setdefault(element, 0)
    frequency_counter[element] += 1
    if frequency_counter[element] > max_frequency:
        max_frequency = frequency_counter[element]
        most_frequent = element
    return (frequency_counter, max_frequency, most_frequent)

sleep_minutes = createSleepingMinutes(events)
guards = map(lambda x: x[1], sleep_minutes)
_, _, mfg = reduce(frequency, guards, ({}, 0, None))
mfg_minutes = [sm[0] for sm in sleep_minutes if sm[1] == mfg]
_, _, mfg_mfm = reduce(frequency, mfg_minutes, ({}, 0, None))
print(mfg * mfg_mfm)

_, _, mfm = reduce(frequency, sleep_minutes, ({}, 0, None))
print(mfm[0] * mfm[1])
