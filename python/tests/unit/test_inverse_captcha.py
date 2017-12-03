from unittest import TestCase

from day1 import inverseCaptchaNeighbors, inverseCaptchaHalfsies
from util import read_advent_data_file


class InverseCaptchaTest(TestCase):
    @classmethod
    def setUpClass(cls):
        cls.puzzle_input = [int(x) for x in read_advent_data_file('inversecaptcha', day=1).strip()]

    def setUp(self):
        self.puzzle_input = type(self).puzzle_input

    def test_part1_puzzle_input(self):
        self.assertEqual(1175, inverseCaptchaNeighbors(self.puzzle_input))

    def test_part2_puzzle_input(self):
        self.assertEqual(1166, inverseCaptchaHalfsies(self.puzzle_input))
