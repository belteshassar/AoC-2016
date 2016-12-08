import re


def is_valid_triangle(a, b, c):
    return a + b > c and b + c > a and a + c > b


if __name__ == '__main__':
    pattern = re.compile(r'\s*(\d*)\s+(\d*)\s+(\d*)')
    count = 0
    with open('input.txt') as f:
        for line in f:
            count += is_valid_triangle(*map(int, pattern.match(line).groups()))
    print(count)
