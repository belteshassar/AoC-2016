from collections import deque
from hashlib import md5

CODE = b'edjrjqaa'
DOORS = [
    (b'U', (0, -1)),
    (b'D', (0, 1)),
    (b'L', (-1, 0)),
    (b'R', (1, 0))
]


def successors(x, y, steps):
    m = md5(CODE + steps)
    h = m.hexdigest()

    for c, door in zip(h, DOORS):
        if c in 'bcdef':
            step, (dx, dy) = door
            x1, y1 = x + dx, y + dy
            if x1 in (-1, 4) or y1 in (-1, 4):
                # Outside maze, ignore!
                continue
            yield x1, y1, steps + step


def bfs(start, target):
    paths = []
    x, y = start
    steps = b''
    q = deque()
    q.append((x, y, steps))
    while len(q) > 0:
        for new_state in successors(*q.popleft()):
            if new_state[:2] == target:
                paths.append(new_state[2])
            else:
                q.append(new_state)
    return paths

# BFS guarantees that these come out sorted by length
paths = bfs((0, 0), (3, 3))

print('Answer part 1:', paths[0])
print('Answer part 2:', len(paths[-1]))
