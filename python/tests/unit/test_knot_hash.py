from unittest import TestCase

from day10.knot_hash import knot_hash, checkproduct, standard_checkproduct
from day10.knot_hash import prepare_input, standard_shuffle


class KnotHashTest(TestCase):
    def setUp(self):
        self.puzzle_input = [88, 88, 211, 106, 141, 1, 78, 254, 2, 111, 77, 255, 90, 0, 54, 205]
        self.puzzle_str = ','.join(map(str, self.puzzle_input))

    def test_checkproduct(self):
        self.assertEqual(20, checkproduct([5, 4, 3, 20, 1]))

    def test_standard_checkproduct_puzzle(self):
        self.assertEqual(11375, standard_checkproduct(self.puzzle_input))

    def test_knot_hash_empty_string(self):
        actual = knot_hash(prepare_input(''), shuffle=standard_shuffle)
        self.assertEqual('a2582a3a0e66e6e86e3812dcb672a272', actual)

    def test_knot_hash_aoc_string(self):
        actual = knot_hash(prepare_input('AoC 2017'), shuffle=standard_shuffle)
        self.assertEqual('33efeb34ea91902bb2f59c9920caa6cd', actual)

    def test_knot_hash_puzzle(self):
        actual = knot_hash(prepare_input(self.puzzle_str), shuffle=standard_shuffle)
        self.assertEqual('e0387e2ad112b7c2ef344e44885fe4d8', actual)
