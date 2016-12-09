import re

import numpy as np


def rect(screen, x, y):
    screen[:y, :x] = 1


def rotrow(screen, row, by):
    screen[row, :] = np.roll(screen[row, :], by)


def rotcol(screen, col, by):
    screen[:, col] = np.roll(screen[:, col], by)


def render(screen):
    charmap = [' ', '\u2588']
    for row in screen:
        for pixel in row:
            print(charmap[pixel], end='')
        print()
    print()


instructions = [
    (rect, re.compile(r'rect (\d*)x(\d*)')),
    (rotrow, re.compile(r'rotate row y=(\d*) by (\d*)')),
    (rotcol, re.compile(r'rotate column x=(\d*) by (\d*)')),
]

screen = np.zeros((6, 50), dtype=int)

with open('input.txt') as f:
    for line in f:
        for func, r in instructions:
            m = r.match(line)
            if m:
                func(screen, *map(int, m.groups()))

render(screen)

print('Number of pixels lit:', np.sum(screen))
