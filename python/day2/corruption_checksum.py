import sys

# ===================================
#          Main Functions
# ===================================


def find_divisible_pair(values):
    test_pairs = ((x, y) if x >= y else (y, x)
                  for x in values
                  for y in values if x != y)

    return next((i, j) for i, j in test_pairs if i % j == 0)


def divisible_checksum(spreadsheet):
    divisible_pairs = map(find_divisible_pair, spreadsheet)
    return sum(i / j for i, j in divisible_pairs)


def minmax(values):
    return min(values), max(values)


def corruption_checksum(spreadsheet):
    minmax_pairs = map(minmax, spreadsheet)
    return sum((b - a) for a, b in minmax_pairs)


# ===================================
#           Driver Code
# ===================================


def clean_input(raw):
    return [[int(x) for x in rowstring.split()] for rowstring in raw.splitlines()]


if __name__ == '__main__':
    def read_input():
        return clean_input(sys.stdin.read())

    if '-p1' in sys.argv:
        print corruption_checksum(read_input())
    elif '-p2' in sys.argv:
        print divisible_checksum(read_input())
    else:
        print 'Please specify part 1 (-p1) or part 2 (-p2)'
