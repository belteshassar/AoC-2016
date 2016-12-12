from collections import defaultdict
import re

val_to = re.compile(r'value (\d+) goes to (bot|output) (\d+)')
give_to = re.compile(r'bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)')

state = {
    'output': defaultdict(list),
    'bot': defaultdict(list),
}
downstream = {}

with open('input.txt') as f:
    for line in f:
        m = val_to.match(line)
        if m:
            val, typ, num = m.groups()
            state[typ][num].append(int(val))
        else:
            m = give_to.match(line)
            bot, typ_l, num_l, typ_h, num_h = m.groups()
            downstream[bot] = [state[typ_l][num_l], state[typ_h][num_h]]

while True:
    n = 0
    for bot, chips in state['bot'].items():
        if len(chips) == 2:
            n += 1
            if all(a == b for a, b in zip(sorted(chips), [17, 61])):
                print('Answer part 1:', bot)
            for i, chip in enumerate(sorted([chips.pop(), chips.pop()])):
                downstream[bot][i].append(chip)
    if n == 0:
        break

bins = list(state['output'][str(i)][0] for i in range(3))

print('Answer part 2:', bins[0]*bins[1]*bins[2])
