class Vector2(object):
    ZERO = None

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Vector2(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        return self + -other

    def __neg__(self):
        return Vector2(-self.x, -self.y)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __iter__(self):
        return (x for x in [self.x, self.y])


Vector2.ZERO = Vector2(0, 0)


class Directions:
    NORTH = Vector2(0, 1)
    SOUTH = -NORTH
    EAST = Vector2(1, 0)
    WEST = -EAST
