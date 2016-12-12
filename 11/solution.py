from collections import namedtuple
from itertools import chain, combinations


State = namedtuple('State', ('e', 'devices'))


def normalize(state):
    n = len(state.devices) // 2
    sorted_pairs = sorted(zip(state.devices[:n], state.devices[-n:]))
    devices = tuple(chain(*zip(*sorted_pairs)))
    e = state.e
    return State(e, devices)


def is_allowed(state):
    if state.e in (-1, 4):
        return False
    n = len(state.devices) // 2
    for m, g in zip(state.devices[:n], state.devices[-n:]):
        if m != g and m in state.devices[-n:]:
            return False
    return True


def new_states(state):
    n = len(state.devices) // 2
    available_devices = list(k for k in range(2*n) if state.devices[k] == state.e)
    for d in (1, -1):
        for ii in combinations(available_devices, 2):
            devices = tuple(state.devices[k] + d*(k in ii) for k in range(2*n))
            yield State(state.e + d, devices)
        for i in available_devices:
            devices = tuple(state.devices[k] + d*(k == i) for k in range(2*n))
            yield State(state.e + d, devices)


def breadth_first_search(state, target, max_depth=200):
    state = normalize(state)
    cache = {}
    cache[state] = 0

    for n in range(max_depth):
        for state in {normalize(state) for state, dist in cache.items() if dist == n}:
            new_allowed_states = {normalize(new_state) for new_state in filter(is_allowed, new_states(state))}
            for new_state in new_allowed_states:
                if new_state not in cache:
                    if new_state == target:
                        return n + 1
                    cache[new_state] = n + 1


if __name__ == '__main__':
    state = State(0, (0, 0, 1, 1, 2, 0, 0, 1, 1, 1))
    target = State(3, (3, 3, 3, 3, 3, 3, 3, 3, 3, 3))
    print('Answer part 1:', breadth_first_search(state, target))
    state = State(0, (0, 0, 1, 1, 2, 0, 0, 0, 0, 1, 1, 1, 0, 0))
    target = State(3, (3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3))
    print('Answer part 2:', breadth_first_search(state, target))
