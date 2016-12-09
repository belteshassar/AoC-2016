from collections import Counter

def part1():
    with open('input.txt') as f:
        for col in zip(*f):
            (c, _), = Counter(col).most_common(1)
            print(c, end='')


def part2():
    with open('input.txt') as f:
        for col in zip(*f):
            c, _ = Counter(col).most_common()[-1]
            print(c, end='')


if __name__ == '__main__':
    print('The answer to part 1 is:')
    part1()
    print('The answer to part 2 is:')
    part2()
