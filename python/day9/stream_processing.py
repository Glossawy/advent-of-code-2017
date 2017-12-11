from sys import argv, stdin


def process_stream(stream):
    open_groups = 0
    groups = 0
    garbage_chars = 0
    in_garbage = False
    skip_next = False

    for c in stream:
        if c == '!':
            skip_next = not skip_next
            continue

        if skip_next:
            skip_next = False
            continue

        if in_garbage:
            if c == '>':
                in_garbage = False
            else:
                garbage_chars += 1
            continue

        if c == '{':
            open_groups += 1
        elif c == '}':
            if open_groups > 0:
                groups += open_groups
                open_groups -= 1
        elif c == '<':
            in_garbage = True
    return groups, garbage_chars


if __name__ == '__main__':
    def read_input():
        return stdin.readline().strip()
    if '-p1' in argv:
        print process_stream(read_input())[0]
    elif '-p2' in argv:
        print process_stream(read_input())[1]
    else:
        print 'Please specify -p1 or -p2 for part 1 or part 2, respectively.'
