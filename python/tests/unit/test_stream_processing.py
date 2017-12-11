from unittest import TestCase

from day9.stream_processing import process_stream
from util.data import read_advent_data_file


class StreamProcessingTest(TestCase):
    def setUp(self):
        self.puzzle_input = read_advent_data_file('puzzle-input', day=9).splitlines()[0]

    def test_process_stream_example_1(self):
        self.assertTupleEqual((16, 0), process_stream('{{{},{},{{}}}}'))

    def test_process_stream_example_2(self):
        self.assertTupleEqual((0, 10), process_stream('<{o"i!a,<{i<a>'))

    def test_process_stream_example_3(self):
        self.assertTupleEqual((3, 17), process_stream('{{<a!>},{<a!>},{<a!>},{<ab>}}'))

    def test_process_stream_puzzle(self):
        self.assertTupleEqual((11089, 5288), process_stream(self.puzzle_input))
