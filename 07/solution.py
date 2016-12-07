#/usr/bin/env python3

import re


def contains_abba(st):
    if re.search(r'(\w)(?!\1)(\w)\2\1', st):
        return True
    else:
        return False


def hypernets(ipv7str):
    regex = re.compile(r'\[(\w*)\]')
    yield from regex.findall(ipv7str)


def supports_tls(ipv7str):
    if not contains_abba(ipv7str):
        return False
    for hypernet in hypernets(ipv7str):
        if contains_abba(hypernet):
            return False
    return True


def supports_ssl(ipv7str):
    match = re.search(
        r'(\w)(?!\1)(\w)\1\w*[[\]]\w*([[\]]\w*[[\]]\w*)*\2\1\2',
        ipv7str)
    if match:
        return True
    else:
        return False


def main():
    inp = open('input.txt')
    count1 = 0
    count2 = 0
    for line in inp:
        if supports_tls(line):
            count1 += 1
        if supports_ssl(line):
            count2 += 1
    inp.close()
    print('Part 1 answer: {}'.format(count1))
    print('Part 2 answer: {}'.format(count2))


if __name__ == '__main__':
    main()
