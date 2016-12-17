class Disc:
    def __init__(self, num_pos, start_pos=0):
        self.n = num_pos
        self.pos = start_pos % self.n

    def rotate(self, step=1):
        self.pos = (self.pos + step) % self.n

    def __repr__(self):
        return 'Disc({}, {})'.format(self.n, self.pos)


class Sculpture:
    def __init__(self, discs):
        self.discs = discs
        self.t = 0

    def step_time(self):
        self.t += 1
        for disc in self.discs:
            disc.rotate()

    def is_open(self):
        return all(disc.pos == (-i % disc.n) for i, disc in enumerate(self.discs, 1))


if __name__ == '__main__':
    sculpture = Sculpture([
        Disc(17, 5),
        Disc(19, 8),
        Disc(7, 1),
        Disc(13, 7),
        Disc(5, 1),
        Disc(3, 0),
    ])
    while not sculpture.is_open():
        sculpture.step_time()
    print('Answer part 1:', sculpture.t)
    sculpture = Sculpture([
        Disc(17, 5),
        Disc(19, 8),
        Disc(7, 1),
        Disc(13, 7),
        Disc(5, 1),
        Disc(3, 0),
    ])
    sculpture.discs.append(Disc(11, 0))
    while not sculpture.is_open():
        sculpture.step_time()
    print('Answer part 2:', sculpture.t)
