from functools import reduce
import re

def extract_step_pair(line):
    steps = re.compile('[Ss]tep ([A-Z])').findall(line)
    return tuple(steps)

def get_step_pairs(filename):
    file = open(filename)
    return [extract_step_pair(line) for line in file.readlines()]

def get_steps(filename):
    file = open(filename)
    steps = re.compile('[Ss]tep ([A-Z])').findall(file.read())
    return set(steps)

def available_steps(steps, step_pairs):
    return set(steps) - {x for _, x in step_pairs}

def get_first(available):
    return min(available)

def remove_pairs(step, pairs):
    return [(a, b) for (a, b) in pairs if a != step]

def get_order(steps, step_pairs):
    if len(steps) <= 1:
        return ''.join(steps)
    available = available_steps(steps, step_pairs)
    first = get_first(available)
    step_pairs = remove_pairs(first, step_pairs)
    steps.remove(first)
    return first + get_order(steps, step_pairs)

def get_time_with_basetime(basetime):
    def closure(step):
        return ord(step) - ord('A') + basetime + 1
    return closure

def all_workers_idle(workers):
    busy_workers = sum((1 for step, _ in workers if step is not None))
    return busy_workers == 0

# debug
def workers_to_string(workers):
    def to_string(worker):
        step, time_left = worker
        return '.' if step is None else '{}({})'.format(step, time_left)
    return '\t'.join([to_string(worker) for worker in workers])

def free_workers_from_finished_tasks(workers):
    def aux(acc, worker):
        finished_tasks, workers = acc
        step, time_left = worker
        if time_left <= 0 and step is not None:
            finished_tasks.add(step)
            worker = (None, time_left)
        workers.append(worker)
        return finished_tasks, workers
    return reduce(aux, workers, (set(), []))

def assign_tasks_to_workers(available, workers, get_time):
    def aux(acc, worker):
        assigned_tasks, available, workers = acc
        step, time_left = worker
        if step is None and available:
            step = get_first(available)
            time_left = get_time(step)
            available.remove(step)
            assigned_tasks.add(step)
        workers.append((step, time_left))
        return assigned_tasks, available, workers
    return reduce(aux, workers, (set(), available, []))

def time_forward_one_sec(workers):
    return [(step, time_left - 1 if time_left > 0 else 0) for (step, time_left) in workers]

def get_work_time(workers, steps, step_pairs, get_time):
    if all_workers_idle(workers) and not steps:
        return -1    
    finished_tasks, workers = free_workers_from_finished_tasks(workers)
    step_pairs = [(a, b) for (a, b) in step_pairs if a not in finished_tasks]
    available = available_steps(steps, step_pairs)
    assigned_tasks, available, workers = assign_tasks_to_workers(available, workers, get_time)
    steps = {step for step in steps if step not in assigned_tasks}
    # print(workers_to_string(workers))
    workers = time_forward_one_sec(workers)
    return 1 + get_work_time(workers, steps, step_pairs, get_time)

def part1(filename):
    step_pairs = get_step_pairs(filename)
    steps = get_steps(filename)
    order = get_order(steps, step_pairs)
    return order

def part2(filename, base_time, nb_of_workers):
    steps = get_steps(filename)
    step_pairs = get_step_pairs(filename)
    get_time = get_time_with_basetime(base_time)
    workers = [(None, 0) for _ in range(0, nb_of_workers)]
    time = get_work_time(workers, steps, step_pairs, get_time)
    return time

print(part1('day7/input_test') == 'CABDFE')
print(part1('day7/input') == 'CFMNLOAHRKPTWBJSYZVGUQXIDE')

print(part2('day7/input_test', 0, 2) == 15)
print(part2('day7/input', 60, 5) == 971)
