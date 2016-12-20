import re


def allowed_ips(ranges):
    sorted_ranges = list(sorted(ranges, reverse=True))
    cleaned = [sorted_ranges.pop()]
    for i in range(cleaned[-1][0]):
        yield i
    while sorted_ranges:
        r = sorted_ranges.pop()
        c = cleaned.pop()
        if r[0] <= c[1]:
            cleaned.append((c[0], max(r[1], c[1])))
        else:
            for i in range(c[1], r[0]):
                yield i
            cleaned.extend([c, r])
    for i in range(cleaned[-1][1], 2**32):
        yield i


if __name__ == '__main__':
    pattern = re.compile(r'(\d+)-(\d+)')
    ranges = []
    with open('input.txt') as f:
        for line in f:
            start, stop = map(int, pattern.match(line).groups())
            ranges.append((start, stop + 1))

    print('Answer part 1:', next(allowed_ips(ranges)))

    for i, _ in enumerate(allowed_ips(ranges), 1): pass
    print('Answer part 2:',  i)
