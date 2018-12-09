# --- Day 7: The Sum of Its Parts ---
# You find yourself standing on a snow-covered coastline; apparently, you landed a little off course. The region is too hilly to see the North Pole from here, but you do spot some Elves that seem to be trying to unpack something that washed ashore. It's quite cold out, so you decide to risk creating a paradox by asking them for directions.
#
# "Oh, are you the search party?" Somehow, you can understand whatever Elves from the year 1018 speak; you assume it's Ancient Nordic Elvish. Could the device on your wrist also be a translator? "Those clothes don't look very warm; take this." They hand you a heavy coat.
#
# "We do need to find our way back to the North Pole, but we have higher priorities at the moment. You see, believe it or not, this box contains something that will solve all of Santa's transportation problems - at least, that's what it looks like from the pictures in the instructions." It doesn't seem like they can read whatever language it's in, but you can: "Sleigh kit. Some assembly required."
#
# "'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh' at once!" They start excitedly pulling more parts out of the box.
#
# The instructions specify a series of steps and requirements about which steps must be finished before others can begin (your puzzle input). Each step is designated by a single letter. For example, suppose you have the following instructions:
#
# Step C must be finished before step A can begin.
# Step C must be finished before step F can begin.
# Step A must be finished before step B can begin.
# Step A must be finished before step D can begin.
# Step B must be finished before step E can begin.
# Step D must be finished before step E can begin.
# Step F must be finished before step E can begin.
# Visually, these requirements look like this:
#
#
#   -->A--->B--
#  /    \      \
# C      -->D----->E
#  \           /
#   ---->F-----
# Your first goal is to determine the order in which the steps should be completed. If more than one step is ready, choose the step which is first alphabetically. In this example, the steps would be completed as follows:
#
# Only C is available, and so it is done first.
# Next, both A and F are available. A is first alphabetically, so it is done next.
# Then, even though F was available earlier, steps B and D are now also available, and B is the first alphabetically of the three.
# After that, only D and F are available. E is not available because only some of its prerequisites are complete. Therefore, D is completed next.
# F is the only choice, so it is done next.
# Finally, E is completed.
# So, in this example, the correct order is CABDFE.
#
# In what order should the steps in your instructions be completed?

from functools import reduce
import re

def extractStepPair(line):
    steps = re.compile('[Ss]tep ([A-Z])').findall(line)
    return tuple(steps)

def getStepPairs(filename):
    f = open(filename)
    return [extractStepPair(line) for line in f.readlines()]

def getSteps(filename):
    f = open(filename)
    steps = re.compile('[Ss]tep ([A-Z])').findall(f.read())
    return set(steps)

def availableSteps(steps, step_pairs):
    return set(steps) - {x for _, x in step_pairs}

def getFirst(available):
    return min(available)

def removePairs(step, pairs):
    return [(a, b) for (a, b) in pairs if a != step]

def getOrder(steps, step_pairs):
    if len(steps) <= 1:
        return ''.join(steps)
    else:
        available = availableSteps(steps, step_pairs)
        first = getFirst(available)
        step_pairs = removePairs(first, step_pairs)
        steps.remove(first)
        return first + getOrder(steps, step_pairs)

def getTimeWithBaseTime(baseTime):
    def closure(step):
        return ord(step) - ord('A') + baseTime + 1
    return closure

def allWorkersIdle(workers):
    busy_workers = sum((1 for step, _ in workers if step is not None))
    return busy_workers == 0

def workersToString(workers):
    def format(worker):
        step, time_left = worker
        return '.' if step is None else '{}({})'.format(step, time_left)
    return '\t'.join([format(worker) for worker in workers])

def freeWorkersFromFinishedTasks(workers):
    def aux(acc, worker):
        finished_tasks, workers = acc
        step, time_left = worker
        if time_left <= 0 and step is not None:
            finished_tasks.add(step)
            worker = (None, time_left)
        workers.append(worker)
        return finished_tasks, workers
    return reduce(aux, workers, (set(),[]))

def assignTasksToWorkers(available, workers, getTime):
    def aux(acc, worker):
        assigned_tasks, available, workers = acc
        step, time_left = worker
        if step == None and len(available) > 0:
            step = getFirst(available)
            time_left = getTime(step)
            available.remove(step)
            assigned_tasks.add(step)
        workers.append((step, time_left))
        return assigned_tasks, available, workers
    return reduce(aux, workers, (set(), available, []))

def timeForwardOneSec(workers):
    return [(step, time_left - 1 if time_left > 0 else 0) for (step, time_left) in workers]

def getWorkTime(workers, steps, step_pairs, getTime):
    if allWorkersIdle(workers) and len(steps) == 0:
        return -1
    else:
        finished_tasks, workers = freeWorkersFromFinishedTasks(workers)
        step_pairs = [(a, b) for (a, b) in step_pairs if a not in finished_tasks]
        available = availableSteps(steps, step_pairs)
        assigned_tasks, available, workers = assignTasksToWorkers(available, workers, getTime)
        steps = {step for step in steps if step not in assigned_tasks}
        # print(workersToString(workers))
        workers = timeForwardOneSec(workers)
        return 1 + getWorkTime(workers, steps, step_pairs, getTime)

def part1(filename):
    step_pairs = getStepPairs(filename)
    steps = getSteps(filename)
    order = getOrder(steps, step_pairs)
    print(order)
    return order

def part2(filename, base_time, nb_of_workers):
    steps = getSteps(filename)
    step_pairs = getStepPairs(filename)
    getTime = getTimeWithBaseTime(base_time)
    # step_times = {step: getTime(step) for step in steps}
    workers = [(None, 0) for _ in range(0, nb_of_workers)]

    time = getWorkTime(workers, steps, step_pairs, getTime)
    print(time)
    return time

print(part1('day7/input_test') == 'CABDFE')
print(part1('day7/input') == 'CFMNLOAHRKPTWBJSYZVGUQXIDE')

print(part2('day7/input_test', 0, 2) == 15)
print(part2('day7/input', 60, 5) == 971)
