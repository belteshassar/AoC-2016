import numpy as np

L = np.matrix([[0, -1], [1, 0]])
R = np.matrix([[0, 1], [-1, 0]])

N = np.matrix([0, 1]).T


def parse_instructions(instructions):
    for instruction in instructions.split(','):
        instruction = instruction.strip()
        turn = instruction[0]
        steps = instruction[1:]
        yield turn, int(steps)


def walker(instructions):
    pos = np.matrix([0, 0]).T
    d = N
    for turn, steps in instructions:
        if turn == 'R':
            d = R*d
        elif turn == 'L':
            d = L*d
        else:
            raise ValueError('Unexpected instruction "%s"' % turn)
        for _ in range(steps):
            pos = pos + d
            yield pos


def part1():
    with open('input.txt') as f:
        instructions = parse_instructions(f.read())
    for pos in walker(instructions):
        pass
    return int(sum(np.abs(pos)))


def part2():
    log = set()
    with open('input.txt') as f:
        instructions = parse_instructions(f.read())
    for pos in walker(instructions):
        pos = tuple(pos.flat)
        if pos in log:
            return int(sum(np.abs(pos)))
        else:
            log.add(pos)


if __name__ == '__main__':
    print('The answer to part 1 is', part1())
    print('The answer to part 2 is', part2())
