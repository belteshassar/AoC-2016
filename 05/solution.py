from hashlib import md5
from itertools import count, islice


INPUT = b'wtnhxymk'


def generate_passcode_chars(room_id):
    for i in count():
        h = md5(room_id + str(i).encode()).hexdigest()
        if h[:5] == '00000':
            yield h[5]


def generate_passcode(room_id):
    passcode = ['*']*8
    print('Computing Passcode:')
    print('\r' + ''.join(passcode), end='')
    for i in count():
        h = md5(room_id + str(i).encode()).hexdigest()
        if h[:5] == '00000':
            try:
                if passcode[int(h[5])] == '*':
                    passcode[int(h[5])] = h[6]
            except (ValueError, IndexError):
                pass
            else:
                print('\r' + ''.join(passcode), end='')
            if all(c != '*' for c in passcode):
                print()
                return


if __name__ == '__main__':
    # print(*islice(generate_passcode_chars(INPUT), 8))
    generate_passcode(INPUT)
