from itertools import permutations
import re


def swap_pos(s, x, y):
    x, y = map(int, [x, y])
    x, y = sorted([x, y])
    return s[:x] + s[y] + s[x+1:y] + s[x] + s[y+1:]

def swap_letters(s, x, y):
    x, y = sorted([s.index(l) for l in [x, y]])
    return swap_pos(s, x, y)

def rotate_steps(s, d, steps):
    steps = int(steps)
    if d == 'left':
        return s[steps:] + s[:steps]
    if d == 'right':
        return s[-steps:] + s[:-steps]

def rot_letter(s, c):
    steps = s.index(c) + 1
    if steps > 4:
        steps += 1
    steps %= len(s)
    return rotate_steps(s, 'right', steps)

def move_pos(s, x, y):
    x, y = map(int, [x, y])
    c = s[x]
    s = s[:x] + s[x+1:]
    return s[:y] + c + s[y:]

def reverse(s, x, y):
    x, y = map(int, [x, y])
    return s[:x] + s[x:y+1][::-1] + s[y+1:]

INST = [
    (re.compile(r"swap position (\d) with position (\d)"), swap_pos),
    (re.compile(r"swap letter (\w) with letter (\w)"), swap_letters),
    (re.compile(r'rotate (left|right) (\d) step'), rotate_steps),
    (re.compile(r'move position (\d) to position (\d)'), move_pos),
    (re.compile(r'rotate based on position of letter (\w)'), rot_letter),
    (re.compile(r'reverse positions (\d) through (\d)'), reverse),
]


def scramble(s):
    with open('input.txt') as f:
        for line in f:
            for r, f in INST:
                m = r.match(line)
                if m:
                    s = f(s, *m.groups())
                    break
            else:
                print(line)
    return s


if __name__ == '__main__':
    s = 'abcdefgh'
    print('Answer part 1', scramble(s))

    for candidate in (''.join(perm) for perm in permutations('abcdefgh')):
        if scramble(candidate) == 'fbgdceah':
            print('Answer part 2', candidate)
            break
