from heapq import heappop, heappush
from itertools import count, product

import numpy as np

# Let's assume that it's possible for each step to be maximum 2 tiles long
# (euclidean distance), meaning that it's possible to jump over a trap to a
# safe tile or step diagonally from a safe tile to another. What is the
# shortest path from any safe tile on the first row to any safe tile on the
# last row?
#
# For example, given an input of .^^.^.^^^^ and 10 rows, the shortest
# path will be
#
# .^^.^O^^^^
# ^^^..O^..^
# ^.^^.^.^^.
# ..^^.O.^^^
# .^^^^O^^.^
# ^^..^O^^..
# ^^^^.O^^^.
# ^..^^^^.^^
# .^^^.O^.^^
# ^^.^^^O.^^
#
# for a total length of 9.4142 tiles or 8 + sqrt(2) if you prefer to be exact.
#
# Given the input and the number of rows in parts 1 and 2, how long is the
# shortest paths?


# We can reach tiles within euclidean distance 2 tiles:
#
#  #######
#  ###.###
#  ##...##
#  #..S..#
#  ##...##
#  ###.###
#  #######

REMOVED = '<removed-task>'


class PriorityQueue:
    """
    This class represents a priority queue. The code is taken from the
    Python manual (https://docs.python.org/3.5/library/queue.html) but it has
    been wrapped in a class for convenience.
    """
    def __init__(self):
        self._h = []
        self._finder = {}
        self._counter = count()

    def __bool__(self):
        "Return True if the queue is not empty."
        if self._h:
            return True
        else:
            return False

    def add_task(self, task, priority=0):
        "Add a new task or update the priority of an existing task"
        if task in self._finder:
            self.remove_task(task)
        count = next(self._counter)
        entry = [priority, count, task]
        self._finder[task] = entry
        heappush(self._h, entry)

    def remove_task(self, task):
        "Mark an existing task as REMOVED.  Raise KeyError if not found."
        entry = self._finder.pop(task)
        entry[-1] = REMOVED

    def pop_task(self):
        "Remove and return the lowest priority task. Raise KeyError if empty."
        while self._h:
            priority, count, task = heappop(self._h)
            if task is not REMOVED:
                del self._finder[task]
                return priority, task
        raise KeyError('pop from an empty priority queue')



def neighbours(tile, tiles):
    "Generator that yields the tiles that can be reached from a given tile."
    x, y = tile
    for dx, dy in product(range(-2, 3), range(-2, 3)):
        d = np.sqrt(dx**2 + dy**2)
        if 0.0 < d <= 2.0:
            x1, y1 = x + dx, y + dy
            try:
                if tiles[x1, y1] == 0:
                    if x1 < 0 or y1 < 0:
                        continue
                    if d == np.sqrt(2):
                        yield np.array((d, 0, 1)), (x1, y1)
                    else:
                        yield np.array((d, int(d), 0)), (x1, y1)
            except IndexError:
                pass


# There is certainly a better way to do this
# It stores redundant information, but saves computations
# Element 0 is the element 1 + element 2 * sqrt(2)
distance_dtype = np.dtype((np.float64, (3,)))


def dijkstra(tiles):
    "Search the shortest path using Dijkstra's algorithm"
    M, N = tiles.shape
    pq = PriorityQueue()
    # For a less regular search space, I'd use a dict.
    visited = np.empty([M, N], distance_dtype)
    visited.fill(np.inf)
    show_d = 0
    for tile in ((0, y) for y in range(N)):
        if tiles[tile] == 0:
            visited[tile] = (0, 0, 0)
            pq.add_task(tile)

    while pq:
        d, tile = pq.pop_task()
        dist = visited[tile]
        if d > show_d + 100:
            show_d += 100
            # Keep track of the progress
            print('\rDistance traveled:', "%d + %d*sqrt(2)" % (dist[1], dist[2]), end='')
        if tile[0] == M - 1:
            print('\rDistance traveled:', "%d + %d*sqrt(2)" % (dist[1], dist[2]))
            return d
        for delta, n in neighbours(tile, tiles):
            # if this path to the tile is shorter, update its entry in visited
            if d + delta[0] < visited[n][0]:
                visited[n] = visited[tile] + delta
                pq.add_task(n, d + delta[0])


def tiles_from_row(row, num_rows):
    rows = [row.strip()]

    while len(rows) < num_rows:
        row = '.' + rows[-1] + '.'
        next_row = ''
        for i in range(len(row) - 2):
            if (row[i] == '^') != (row[i+2] == '^'):
                next_row += '^'
            else:
                next_row += '.'
        rows.append(next_row)

    return np.array([[c == '^' for c in row] for row in rows], dtype=int)


if __name__ == '__main__':
    with open('input.txt') as f:
        first_row = f.read()
    tiles = tiles_from_row(first_row, 40)
    print('Part 1')
    dijkstra(tiles)
    tiles = tiles_from_row(first_row, 400000)
    print('Part 2')
    dijkstra(tiles)
