from unittest import TestCase
from util.data import read_advent_data_file

from day7.recursive_circus import get_base_disc, find_balance_resolving_weight, read_discs_to_nodes


class RecursiveCircusTest(TestCase):
    def setUp(self):
        self.example_input = read_advent_data_file('example-input', day=7).splitlines()
        self.example_input = read_discs_to_nodes(self.example_input)
        self.puzzle_input = read_advent_data_file('puzzle-input', day=7).splitlines()
        self.puzzle_input = read_discs_to_nodes(self.puzzle_input)

    def test_example_base_disc(self):
        self.assertEqual('tknk', get_base_disc(self.example_input))

    def test_puzzle_base_disc(self):
        self.assertEqual('rqwgj', get_base_disc(self.puzzle_input))

    def test_example_balance_resolving_weight(self):
        self.assertEqual(60, find_balance_resolving_weight(self.example_input))

    def test_puzzle_balance_resolving_weight(self):
        self.assertEqual(333, find_balance_resolving_weight(self.puzzle_input))
