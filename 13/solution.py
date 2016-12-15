from collections import deque


def is_open(x, y, p):
    s = x*x + 3*x + 2*x*y + y + y*y + p
    b = bin(s)[2:]
    return sum(map(int, b)) % 2 == 0

def in_maze(x, y):
    return x >= 0 and y >= 0

def is_allowed(x, y, p):
    return is_open(x, y, p) and in_maze(x, y)

def new_states(x, y):
    for dx, dy in ((0, 1), (1, 0), (-1, 0), (0, -1)):
        yield x + dx, y + dy


def breadth_first_search(state, target, p):
    if state == target:
        return 0
    cache = {}
    cache[state] = 0
    states = deque([state])

    while states:
        state = states.popleft()
        new_allowed_states = filter(lambda s: is_allowed(*s, p),
                    new_states(*state))
        for new_state in new_allowed_states:
            if new_state in cache:
                continue
            if new_state == target:
                print(sum(value <= 50 for value in cache.values()))
                return cache[state] + 1
            cache[new_state] = cache[state] + 1
            states.append(new_state)


if __name__ == '__main__':
    target = (31, 39)
    state = (1, 1)
    p = 1362
    ans = breadth_first_search(state, target, p)
    print('Answer part 1:', ans)
