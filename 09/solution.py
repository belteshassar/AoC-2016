import re


marker = re.compile(r'\((\d*)x(\d*)\)')


def part1():
    with open('input.txt') as f:
        compressed = f.read()

    decomp_len = 0
    i = 0

    while i < len(compressed):
        if compressed[i] == '(':
            m = marker.match(compressed, i)
            count, times = map(int, m.groups())
            decomp_len += times*count
            i = m.end() + count
        elif compressed[i].isspace():
            i += 1
        else:
            decomp_len += 1
            i += 1

    print(decomp_len)

def part2():
    with open('input.txt') as f:
        compressed = f.read()
    print(decomp_length(compressed))

def decomp_length(compressed):
    marker = re.compile(r'\((\d+)x(\d+)\)')
    decomp_len = 0
    i = 0
    while i < len(compressed):
        if compressed[i] == '(':
            m = marker.match(compressed, i)
            count, times = map(int, m.groups())
            i = m.end() + count
            decomp_len += times*decomp_length(compressed[m.end():i])
        elif compressed[i].isspace():
            i += 1
        else:
            decomp_len += 1
            i += 1
    return decomp_len


if __name__ == '__main__':
    part2()
