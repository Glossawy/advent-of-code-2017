from __future__ import print_function

from sys import argv, stdin

from itertools import izip_longest
from operator import xor

DEBUG = '-debug' in argv


def igroup(n, iterable, filler=''):
    xs = [iter(iterable)] * n
    return izip_longest(*xs, fillvalue=filler)


def reverse(xs, start, length):
    low = start
    high = (start + length) - 1
    size = len(xs)

    while low < high:
        xs[low % size], xs[high % size] = xs[high % size], xs[low % size]
        low += 1
        high -= 1


def shuffle_bytes_with_rounds(rounds=1):
    def fn(xs, ls):
        pos = 0
        skip = 0
        size = len(xs)

        for round in range(rounds):
            for length in ls:
                if length > size:
                    continue

                reverse(xs, pos, length)
                pos = (pos + length + skip) % size
                skip += 1
        return xs
    return fn


basic_shuffle = shuffle_bytes_with_rounds(rounds=1)
repeated_shuffle = shuffle_bytes_with_rounds(rounds=64)


def densify_knot(xs):
    groups = [g for g in igroup(16, xs)]
    return [reduce(xor, g) for g in groups]


def standard_shuffle(xs, ls):
    repeated_shuffle(xs, ls)
    return densify_knot(xs)


def shuffle_standard_knot(ls, shuffle=basic_shuffle):
    return shuffle(standard_knot(), ls)


def checkproduct(xs):
    return xs[0] * xs[1]


def standard_checkproduct(ls, shuffle=basic_shuffle):
    shuffled_knot = shuffle_standard_knot(ls, shuffle=shuffle)
    return checkproduct(shuffled_knot)


def knot_hash(ls, shuffle=basic_shuffle):
    standard_hash = shuffle_standard_knot(ls, shuffle=shuffle)
    return ''.join('%0.2X' % x for x in standard_hash).lower()


def standard_knot():
    return list(range(0, 256))


def prepare_input(chars):
    return [ord(c) for c in chars] + [17, 31, 73, 47, 23]


if __name__ == '__main__':
    def read_input():
        chars = filter(lambda x: x.strip() != '', stdin.readline().strip().split(','))
        return [int(c) for c in chars]

    def ascii_input():
        chars = stdin.readline().strip()
        return prepare_input(chars)

    if '-p1' in argv:
        print('Standard Checkproduct:', standard_checkproduct(read_input(), shuffle=basic_shuffle))
        print('Shuffle done on standard knot using lengths given')
    elif '-p2' in argv:
        print('Knot Hash:', knot_hash(ascii_input(), shuffle=standard_shuffle))
    else:
        print('Please specify -p1 or -p2 for part 1 or part 2, respectively.')
