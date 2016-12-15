from collections import defaultdict
from hashlib import md5
from itertools import count, islice
import re


salt = b'yjdafjpo'

three = re.compile(r'(.)\1\1')
five = re.compile(r'(.)\1\1\1\1')

def keys(salt):
    threes = defaultdict(list)
    for i in count():
        h = md5()
        h.update(salt + bytes(str(i), 'ascii'))
        s = h.hexdigest()
        m = five.search(s)
        if m:
            for triplet in threes[m.group(1)]:
                if i - 1000 <= triplet:
                    yield triplet
                    continue
        m = three.search(s)
        if m:
            threes[m.group(1)].append(i)


def stretched_keys(salt):
    threes = defaultdict(list)
    for i in count():
        h = md5()
        h.update(salt + bytes(str(i), 'ascii'))
        s = h.hexdigest()
        s = stretch(s)
        m = five.search(s)
        if m:
            for triplet in threes[m.group(1)]:
                if i - 1000 <= triplet:
                    yield triplet
                    continue
        m = three.search(s)
        if m:
            threes[m.group(1)].append(i)

def stretch(h):
    ret = bytes(h, 'ascii')
    for i in range(2016):
        m = md5()
        m.update(ret)
        ret = bytes(m.hexdigest(), 'ascii')
    return ret.decode('ascii')


print(next(islice(sorted(islice(keys(salt), 112)), 63, 65)))
print(next(islice(sorted(islice(stretched_keys(salt), 70)), 63, 65)))
