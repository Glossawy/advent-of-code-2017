from unittest import TestCase
from day3.spiral_memory import ulam_dist_from_center, ulam_dist_explicit


class SpiralMemoryTest(TestCase):
    # Implicit approach tests:

    def test_ulam_dist_from_center_example_1(self):
        self.assertEqual(0, ulam_dist_from_center(1))

    def test_ulam_dist_from_center_example_2(self):
        self.assertEqual(3, ulam_dist_from_center(12))

    def test_ulam_dist_from_center_example_3(self):
        self.assertEqual(2, ulam_dist_from_center(23))

    def test_ulam_dist_from_center_example_4(self):
        self.assertEqual(31, ulam_dist_from_center(1024))

    def test_ulam_dist_from_center_fails_on_0(self):
        with self.assertRaises(ValueError):
            ulam_dist_from_center(0)

    # Same but for explicit calculation:

    def test_ulam_dist_explicit_example_1(self):
        self.assertEqual(0, ulam_dist_explicit(1))

    def test_ulam_dist_explicit_example_2(self):
        self.assertEqual(3, ulam_dist_explicit(12))

    def test_ulam_dist_explicit_example_3(self):
        self.assertEqual(2, ulam_dist_explicit(23))

    def test_ulam_dist_explicit_example_4(self):
        self.assertEqual(31, ulam_dist_explicit(1024))

    def test_ulam_dist_explicit_fails_on_0(self):
        with self.assertRaises(ValueError):
            ulam_dist_explicit(0)
