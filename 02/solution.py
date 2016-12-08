


class KeyPosition:
    keypad = {
        (0, 0): 1, (1, 0): 2, (2, 0): 3,
        (0, 1): 4, (1, 1): 5, (2, 1): 6,
        (0, 2): 7, (1, 2): 8, (2, 2): 9,
    }

    def __init__(self, pos=(1, 1)):
        if pos not in self.keypad:
            raise ValueError('pos not on keypad')
        self.pos = pos

    def move(self, d):
        if d == 'U':
            newpos = self.pos[0], self.pos[1] - 1
        elif d == 'D':
            newpos = self.pos[0], self.pos[1] + 1
        elif d == 'L':
            newpos = self.pos[0] - 1, self.pos[1]
        elif d == 'R':
            newpos = self.pos[0] + 1, self.pos[1]

        if newpos in self.keypad:
            self.pos = newpos

    @property
    def key(self):
        return self.keypad[self.pos]

    @key.setter
    def key(self, value):
        for pos, key in self.keypad.items():
            if key == value:
                break
        else:
            raise KeyError('No such key on keypad')


def part1():
    kp = KeyPosition()
    with open('input.txt') as f:
        for line in f:
            for c in line:
                if c == '\n':
                    print(kp.key)
                    break
                kp.move(c)


def part2():
    kp = KeyPosition()
    kp.keypad = {
                              (2, 0): 1,
                   (1, 1): 2, (2, 1): 3, (3, 1): 4,
        (0, 2): 5, (1, 2): 6, (2, 2): 7, (3, 2): 8, (4, 2): 9,
                   (1, 3): 'A', (2, 3): 'B', (3, 3): 'C',
                              (2, 4): 'D',
    }
    kp.key = 5
    with open('input.txt') as f:
        for line in f:
            for c in line:
                if c == '\n':
                    print(kp.key)
                    break
                kp.move(c)

if __name__ == '__main__':
    part1()
    part2()
