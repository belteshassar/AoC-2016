import re
from collections import defaultdict, deque, namedtuple

testcase = (
'''cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a
'''
)

REGEXES = [
    ('CPY', re.compile(r'cpy')),
    ('INC', re.compile(r'inc')),
    ('DEC', re.compile(r'dec')),
    ('JNZ', re.compile(r'jnz')),
    ('NUM', re.compile(r'-?\d+')),
    ('ID', re.compile(r'[a-z]')),
    ('NEWLINE', re.compile(r'\n')),
    ('WHITESPACE', re.compile(r' ')),
]

Token = namedtuple('Token', ('typ', 'text'))


def tokenize(code):
    tokens = []
    while code:
        for name, regex in REGEXES:
            m = regex.match(code)
            if m:
                if name != 'WHITESPACE':
                    tokens.append(Token(name, m.group(0)))
                code = code[m.end():]
                break
    return tokens


Program = namedtuple('Program', ('statements',))
Register = namedtuple('Register', ('id',))
Number = namedtuple('Number', ('val',))
Cpy = namedtuple('Cpy', ('val', 'reg'))
Inc = namedtuple('Inc', ('reg'))
Dec = namedtuple('Dec', ('reg'))
Jnz = namedtuple('Jnz', ('op', 'step'))


class UnexpectedTokenError(RuntimeError):
    pass

class Parser:
    def __init__(self, tokens):
        self.tokens = deque(tokens)

    def parse(self):
        statements = []
        while self.tokens:
            statement = self.parse_statement()
            statements.append(statement)
        return Program(statements)

    def parse_statement(self):
        typ, _ = self.pop(['CPY', 'INC', 'DEC', 'JNZ'])
        if typ == 'CPY':
            return self.parse_cpy()
        elif typ == 'INC':
            return self.parse_inc()
        elif typ == 'DEC':
            return self.parse_dec()
        elif typ == 'JNZ':
            return self.parse_jnz()

    def parse_cpy(self):
        val = self.parse_op()
        reg = self.parse_reg()
        self.pop('NEWLINE')
        return Cpy(val, reg)

    def parse_inc(self):
        reg = self.parse_reg()
        self.pop('NEWLINE')
        return Inc(reg)

    def parse_dec(self):
        reg = self.parse_reg()
        self.pop('NEWLINE')
        return Dec(reg)

    def parse_jnz(self):
        op = self.parse_op()
        step = self.parse_op()
        self.pop('NEWLINE')
        return Jnz(op, step)

    def parse_op(self):
        try:
            return self.parse_num()
        except UnexpectedTokenError:
            return self.parse_reg()

    def parse_reg(self):
        id_ = self.pop(['ID']).text
        return Register(id_)

    def parse_num(self):
        val = self.pop(['NUM']).text
        return Number(val)

    def expect(self, types):
        if self.tokens[0].typ not in types:
            raise UnexpectedTokenError

    def pop(self, types=None):
        if types:
            self.expect(types)
        return self.tokens.popleft()


def generate(node):
    if isinstance(node, Program):
        return list(compile(generate(child), '<string>', 'exec')
                    for child in node.statements)
    elif isinstance(node, Cpy):
        return '{reg} = {val}'.format(
                reg=generate(node.reg), val=generate(node.val))
    elif isinstance(node, Inc):
        return '{reg} += 1'.format(reg=generate(node.reg))
    elif isinstance(node, Dec):
        return '{reg} -= 1'.format(reg=generate(node.reg))
    elif isinstance(node, Jnz):
        return 'if {} != 0: self.pointer += {} - 1'.format(
                    generate(node.op), generate(node.step))
    elif isinstance(node, Register):
        return 'self.registers["{}"]'.format(node.id)
    elif isinstance(node, Number):
        return node.val
    else:
        return repr(node)


class VM:
    def __init__(self, program):
        self.program = program
        self.pointer = 0
        self.registers = defaultdict(int)

    def step(self):
        try:
            exec(self.program[self.pointer])
        except IndexError:
            raise StopIteration('End of program')
        self.pointer += 1

    def __iter__(self):
        while True:
            yield self.step()

    def run(self):
        for statement in self:
            pass


if __name__ == '__main__':
    with open('input.assembunny') as f:
        code = f.read()
    tokens = tokenize(code)
    tree = Parser(tokens).parse()
    program = generate(tree)
    vm = VM(program)
    vm.run()
    print('Answer part 1:', vm.registers['a'])

    vm = VM(program)
    vm.registers['c'] = 1
    vm.run()
    print('Answer part 2:', vm.registers['a'])
