from sys import stdin, argv
from itertools import count


def reallocate_memory(mem_banks):
    resources = max(mem_banks)
    mem_cpy = [x for x in mem_banks]

    idx = mem_cpy.index(resources)
    mem_cpy[idx] = 0

    min_redistribution = resources // len(mem_banks)
    resources -= min_redistribution * len(mem_banks)
    mem_cpy = [x + min_redistribution for x in mem_cpy]
    for i in count(idx+1):
        if resources <= 0:
            break
        mem_cpy[i % len(mem_cpy)] += 1
        resources -= 1
    return mem_cpy


def find_cycle_steps(mem_banks):
    """
        Brent's Algorithm (Adaptation of Floyd's Tortoise and Hare Algorithm)

        Based on the assumption that there is a power of 2 (2^k) such that
        lambda and mu are <= 2^k, where lambda is the loop length and mu is the
        length of the list leading up to the loop.

        Every power of two the tortoise teleports  to the hare's location. Eventually 2^k
        will be large enough such that the hare meets up with the tortoise indicating a cycle.

        From there we reset to the beginning, advance the hare lambda units and
        advance until they meet (at the beginning of the loop) where the distance
        traveled by the tortoise is mu.

        Better since Brent's Algorithm does fewer evaluations of f then Floyd's Algorithm,
        generally.
    """
    power = lam = 1
    tortoise = mem_banks
    hare = reallocate_memory(mem_banks)

    while tortoise != hare:
        if power == lam:
            tortoise = hare
            power *= 2
            lam = 0
        hare = reallocate_memory(hare)
        lam += 1

    mu = 0
    tortoise = hare = mem_banks
    for i in range(lam):
        hare = reallocate_memory(hare)

    while tortoise != hare:
        tortoise = reallocate_memory(tortoise)
        hare = reallocate_memory(hare)
        mu += 1

    return lam, mu


if __name__ == '__main__':
    def read_input():
        return [int(x.strip()) for x in stdin.read().split()]
    if '-p1' in argv:
        print sum(find_cycle_steps(read_input()))
    elif '-p2' in argv:
        print find_cycle_steps(read_input())[0]
    else:
        print 'Please use -p1 or -p2 to specify part 1 or part 2.'
