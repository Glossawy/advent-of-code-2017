from unittest import TestCase
from day8.unusual_registers import greatest_alltime_register, greatest_valued_register

from util.data import read_advent_data_file


class UnusualRegistersTest(TestCase):
    def setUp(self):
        self.example_input = read_advent_data_file('example-input', day=8).splitlines()
        self.puzzle_input = read_advent_data_file('puzzle-input', day=8).splitlines()

    def test_greatest_valued_register_example(self):
        self.assertEqual(1, greatest_valued_register(self.example_input))

    def test_greatest_valued_register_puzzle(self):
        self.assertEqual(4877, greatest_valued_register(self.puzzle_input))

    def test_greatest_alltime_register_example(self):
        self.assertEqual(10, greatest_alltime_register(self.example_input))

    def test_Greatest_alltime_register_puzzle(self):
        self.assertEqual(5471, greatest_alltime_register(self.puzzle_input))
