from itertools import chain, islice
from sys import argv

import operator


# ===================================
#            Utilities
# ===================================


def to_tuple(*args):
    return tuple(args)


def first_arg(x, *_):
    return x


def itail(xs):
    return islice(xs, 1, None)


def head(xs):
    return xs[0]


def sum_eq(xs, ys, transform=to_tuple):
    xys = [transform(x, y) for x, y in zip(xs, ys) if operator.eq(x, y)]
    return sum(xys)


# ===================================
#           Main Functions
# ===================================


# Part 1
def inverseCaptchaNeighbors(xs):
    rotated = chain(itail(xs), [head(xs)])
    return sum_eq(xs, rotated, transform=first_arg)


# Part 2
def inverseCaptchaHalfsies(xs):
    halfpoint = len(xs) // 2
    front = islice(xs, halfpoint)       # Lazy O(1) space slicing
    tail = islice(xs, halfpoint, None)
    return sum_eq(front, tail, transform=operator.add)


def read_input():
    return [int(c) for c in raw_input()]


if '-p1' in argv:
    print inverseCaptchaNeighbors(read_input())
elif '-p2' in argv:
    print inverseCaptchaHalfsies(read_input())
else:
    print 'Please specify part 1 (-p1) or part 2 (-p2)'
