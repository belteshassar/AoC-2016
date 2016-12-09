import re


def is_valid_triangle(a, b, c):
    return a + b > c and b + c > a and a + c > b


def part1():
    pattern = re.compile(r'\s*(\d*)\s+(\d*)\s+(\d*)')
    count = 0
    with open('input.txt') as f:
        for line in f:
            count += is_valid_triangle(*map(int, pattern.match(line).groups()))
    print(count)


def part2():
    pattern = re.compile(r'\s*(\d*)\s+(\d*)\s+(\d*)')
    count = 0
    cols = [[], [], []]
    with open('input.txt') as f:
        for line in f:
            vals = map(int, pattern.match(line).groups())
            for col, val in zip(cols, vals):
                col.append(val)
            if len(cols[0]) == 3:
                count += sum(is_valid_triangle(*col) for col in cols)
                cols = [[], [], []]
    print(count)


if __name__ == '__main__':
    print('Answer part 1:')
    part1()
    print('Answer part 2:')
    part2()
