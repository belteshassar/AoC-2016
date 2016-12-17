t = str.maketrans({'1': '0', '0': '1'})

INPUT = '10010000000110000'


def dragon(seed, target_len):
    ret = seed
    while len(ret) < target_len:
        ret += '0' + ret[::-1].translate(t)
    return ret[:target_len]


def check_sum(data):
    while len(data) % 2 == 0:
        data = ''.join('1' if a == b else '0' for a, b in zip(data[::2], data[1::2]))
    return data


data = dragon(INPUT, 272)
chk = check_sum(data)
print('Answer part 1:', chk)


data = dragon(INPUT, 35651584)
chk = check_sum(data)
print('Answer part 2:', chk)
