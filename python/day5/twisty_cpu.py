from sys import stdin, argv


class CpuState(object):
    def __init__(self, program_counter=0):
        self.pc = program_counter
        self.cycles = 0


def increment_value(val):
    return val + 1


def decrease_if_exceeds(lim):
    def fn(val):
        return val - 1 if val >= 3 else val + 1
    return fn


def execute_jmp_program(jmp_set, post_op):
    def in_program_bounds():
        return 0 <= state.pc < len(jmp_set)

    state = CpuState()
    while in_program_bounds():
        delta = jmp_set[state.pc]
        jmp_set[state.pc] = post_op(jmp_set[state.pc])
        state.pc += delta
        state.cycles += 1

    return state.cycles


if __name__ == '__main__':
    def read_input():
        inp = stdin.readlines()
        return [int(x.strip()) for x in inp]

    if '-p1' in argv:
        print execute_jmp_program(read_input(), post_op=increment_value)
    elif '-p2' in argv:
        print execute_jmp_program(read_input(), post_op=decrease_if_exceeds(3))
    else:
        print 'Please specify -p1 or -p2 to use either the part 1 or part 2 solution.'
