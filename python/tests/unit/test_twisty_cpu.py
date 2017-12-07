from unittest import TestCase, skip
from util.data import read_advent_data_file

from day5.twisty_cpu import execute_jmp_program, increment_value, decrease_if_exceeds


decrease_at_3 = decrease_if_exceeds(3)


class TwistyCpuTest(TestCase):
    def setUp(self):
        self.puzzle_input = read_advent_data_file('puzzle-instruction-set', day=5)
        self.puzzle_input = [int(x.strip()) for x in self.puzzle_input.splitlines()]

    def test_jmp_program_example(self):
        self.assertEqual(5, execute_jmp_program([0, 3, 0, 1, -3], increment_value))

    def test_jmp_program_puzzle_solution(self):
        self.assertEqual(372139, execute_jmp_program(self.puzzle_input, increment_value))

    def test_jmp_strange_program_example(self):
        self.assertEqual(10, execute_jmp_program([0, 3, 0, 1, -3], decrease_at_3))

    @skip('Long running test, should only be run locally')
    def test_jmp_strange_program_puzzle_solution(self):
        self.assertEqual(29629538, execute_jmp_program(self.puzzle_input, decrease_at_3))
