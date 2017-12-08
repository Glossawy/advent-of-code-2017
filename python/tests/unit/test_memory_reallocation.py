from unittest import TestCase
from util.data import read_advent_data_file

from day6.memory_reallocation import find_cycle_steps


class MemoryAllocationTest(TestCase):
    def setUp(self):
        self.example_input = [0, 2, 7, 0]
        self.puzzle_input = read_advent_data_file('p1-registers', day=6)
        self.puzzle_input = [int(x.strip()) for x in self.puzzle_input.split()]

    def test_part1_example(self):
        lam, mu = find_cycle_steps(self.example_input)
        self.assertEqual(5, lam + mu)

    def test_part1_puzzle_solution(self):
        lam, mu = find_cycle_steps(self.puzzle_input)
        self.assertEqual(7864, lam + mu)

    def test_part2_example(self):
        self.assertEqual(4, find_cycle_steps(self.example_input)[0])

    def test_part2_puzzle_solution(self):
        self.assertEqual(1695, find_cycle_steps(self.puzzle_input)[0])
