from collections import Counter


def prep(row):
    return row.strip()


def tile_from_row(row, num_rows):
    rows = [prep(row)]

    while len(rows) < num_rows:
        row = '#' + rows[-1] + '#'
        next_row = ''
        for i in range(len(row) - 2):
            if (row[i] == '^') != (row[i+2] == '^'):
                next_row += '^'
            else:
                next_row += '.'
        rows.append(next_row)

    return '\n'.join(rows)


with open('input.txt') as f:
    row = f.read().strip()

tiles = tile_from_row(row, 40)
print('Answer part 1:', Counter(tiles)['.'])
tiles = tile_from_row(row, 400000)
print('Answer part 2:', Counter(tiles)['.'])
