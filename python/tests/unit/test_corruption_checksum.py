from unittest import TestCase

from day2.corruption_checksum import corruption_checksum, clean_input, find_divisible_pair, divisible_checksum
from util.data import read_advent_data_file


class CorruptionChecksumTest(TestCase):
    @classmethod
    def setUpClass(cls):
        cls.puzzle_data = read_advent_data_file('corruption-checksum', day=2)
        cls.example_data = read_advent_data_file('corruption-example', day=2)
        cls.divisible_example_data = read_advent_data_file('divisible-example', day=2)

    def setUp(self):
        self.puzzle_input = clean_input(type(self).puzzle_data)
        self.example_input = clean_input(type(self).example_data)
        self.divisible_example_input = clean_input(type(self).divisible_example_data)

    def test_clean_input(self):
        cleaned = self.example_input
        self.assertEqual(3, len(cleaned))

        expected_spreadsheet = [
            [5, 1, 9, 5],
            [7, 5, 3],
            [2, 4, 6, 8]
        ]

        for expected, actual in zip(expected_spreadsheet, cleaned):
            self.assertListEqual(expected, actual)

    def test_find_divisible_pair(self):
        row = self.divisible_example_input[0]
        self.assertTupleEqual((8, 2), find_divisible_pair(row))

    def test_corruption_checksum_example(self):
        self.assertEqual(18, corruption_checksum(self.example_input))

    def test_corruption_checksum_puzzle(self):
        self.assertEqual(48357, corruption_checksum(self.puzzle_input))

    def test_divisible_checksum_example(self):
        result = divisible_checksum(self.divisible_example_input)
        self.assertEqual(9, result)

    def test_divisble_checksum_puzzle(self):
        self.assertEqual(351, divisible_checksum(self.puzzle_input))
