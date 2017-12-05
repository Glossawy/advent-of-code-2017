from sys import argv
from math import sqrt, ceil
from itertools import cycle

from day3.vectors import Vector2, Directions


def manhattan_distance(u, v):
    (dx, dy) = u - v
    return abs(dx) + abs(dy)


def ulam_dist_from_center(n):
    if n < 1:
        raise ValueError('N may not be lesser than 1. (%d)' % (n))

    direction_cycle = cycle([Directions.EAST, Directions.NORTH, Directions.WEST, Directions.SOUTH])
    transition_directions = [Directions.NORTH, Directions.SOUTH]
    direction = next(direction_cycle)

    level = 1
    counter = 1
    position = Vector2.ZERO

    for i in range(n-1):
        position += direction
        counter -= 1

        if counter == 0:
            if direction in transition_directions:
                level += 1
            counter = level
            direction = next(direction_cycle)

    return manhattan_distance(Vector2.ZERO, position)


# See description in haskell part of code
def ulam_dist_explicit(n):
    if n < 1:
        raise ValueError('N may not be lesser than 1. (%d)' % (n))
    elif n == 1:
        return 0

    axis_offset = int(ceil(sqrt(n)) / 2)
    side_length = 2 * axis_offset + 1
    prev_square_area = (side_length - 2) ** 2

    corner_offset = (n - prev_square_area) % (side_length - 1)
    return axis_offset + abs(corner_offset - axis_offset)


if __name__ == '__main__':
    def read_input():
        n = raw_input('N: ')
        return int(n)

    if '-i' in argv or '--implicit' in argv:
        print ulam_dist_from_center(read_input())
    elif '-e' in argv or '--explicit' in argv:
        print ulam_dist_explicit(read_input())
    else:
        print 'Please enter provide either the --implict or --explicit flag (-i/-e, respectively)'
