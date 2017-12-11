from collections import namedtuple, defaultdict
from sys import stdin, argv
import operator
import re

Comparisons = {
    '<': operator.lt,
    '<=': operator.le,
    '==': operator.eq,
    '!=': operator.ne,
    '>=': operator.ge,
    '>': operator.gt,
}

PARSER_RE = re.compile('^(\w+) (inc|dec) (-?\d+) if (\w+) ([!<>=]+) (-?\d+)$')

Expression = namedtuple('Expression', ['reg', 'op', 'amt', 'cmpReg', 'cmpOp', 'cmpAmt'])


def idempotent(*args, **kwargs):
    pass


def conditional(registers, exp):
    reg, op, amt = exp.cmpReg, exp.cmpOp, exp.cmpAmt
    return Comparisons[op](registers[reg], amt)


def inc_op(registers, exp):
    registers[exp.reg] += exp.amt


def dec_op(registers, exp):
    registers[exp.reg] -= exp.amt


Operations = {
    'inc': inc_op,
    'dec': dec_op,
}


def process(registers, expressions, listener=idempotent):
    for exp in expressions:
        if conditional(registers, exp):
            old = registers[exp.reg]
            Operations[exp.op](registers, exp)
            new = registers[exp.reg]
            listener(exp.reg, old or 0, new or 0)
    return registers


def interpret(program, listener=idempotent):
    expressions = []
    for line in [x.strip() for x in program]:
        if line == '':
            continue
        parsed = list(PARSER_RE.findall(line)[0])
        parsed[2] = int(parsed[2])  # Inc/Dec Amount
        parsed[5] = int(parsed[5])  # Comparson Amount
        expressions.append(Expression(*parsed))
    registers = defaultdict(lambda: 0)
    return process(registers, expressions, listener)


def greatest_valued_register(program):
    registers = interpret(program)
    return max(registers.values())


def greatest_alltime_register(program):
    class Closure:
        largest_register_value = float('-inf')

    def listen(reg, old, new):
        if new > Closure.largest_register_value:
            Closure.largest_register_value = new

    interpret(program, listener=listen)
    return Closure.largest_register_value


if __name__ == '__main__':
    if '-p1' in argv:
        print greatest_valued_register(stdin.readlines())
    elif '-p2' in argv:
        print greatest_alltime_register(stdin.readlines())
    else:
        print 'Please specify -p1 or -p2 for part 1 or part 2, respectively.'
