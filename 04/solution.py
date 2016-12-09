from collections import Counter, namedtuple
from itertools import islice
import json
import re


Room = namedtuple('Room', ('encrypted_name', 'sector_id', 'checksum'))


def is_real_room(room):
    c = Counter(room.encrypted_name.replace('-', ''))
    sorted_letter_counts = sorted(c.items(), key=lambda x: (-x[1], x[0]))
    expected_checksum = ''.join(x[0] for x in islice(sorted_letter_counts, 5))
    return expected_checksum == room.checksum


def decrypt(c, n):
    if c == '-':
        return ' '
    a = ord('a')
    return chr((ord(c) - a + n) % 26 + a)


def part1():
    pattern = re.compile(r'([a-z0-9\-]*)-(\d*)\[([a-z]{5})\]')
    sector_id_sum = 0
    with open('input.txt') as f:
        for line in f:
            room = Room(*pattern.match(line).groups())
            if is_real_room(room):
                sector_id_sum += int(room.sector_id)
    print(sector_id_sum)


def part2():
    pattern = re.compile(r'([a-z0-9\-]*)-(\d*)\[([a-z]{5})\]')
    decrypted_rooms = {}
    with open('input.txt') as f:
        for i, line in enumerate(f):
            room = Room(*pattern.match(line).groups())
            if is_real_room(room):
                decrypted_name = ''.join(decrypt(c, int(room.sector_id)) for c in room.encrypted_name)
                decrypted_rooms[decrypted_name] = room.sector_id
    with open('decrypted.txt', 'w') as f:
        json.dump(decrypted_rooms, f, indent=2)


if __name__ == '__main__':
    part1()
    part2()
