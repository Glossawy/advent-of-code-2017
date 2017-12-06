from unittest import TestCase
from util.data import read_advent_data_file

from day4.high_entropy_passphrase import is_valid_passphrase, count_valid_passphrases
from day4.high_entropy_passphrase import is_particularly_valid_passphrase


def mock_alternation_validity_check(first_is_valid=True):
    class Closure:
        next_valid = first_is_valid

    def validity_fn(passphrase):
        valid = Closure.next_valid
        Closure.next_valid = not Closure.next_valid
        return valid
    return validity_fn


class HighEntropyPassphraseTest(TestCase):
    def setUp(self):
        self.puzzle_input = read_advent_data_file('p1-passphrases', day=4).splitlines()

    def test_valid_passphrases_are_valid(self):
        for phrase in ['aa bb cc dd', 'aa bb cc dd aaa', 'this is a unique passphrase', 'no', '']:
            self.assertTrue(is_valid_passphrase(phrase),
                            msg='{} was considered invalid but is valid'.format(phrase))

    def test_invalid_passphrases_are_invalid(self):
        for phrase in ['aa bb cc dd aa', 'non unique pass pass phrase', 'no no no no']:
            self.assertFalse(is_valid_passphrase(phrase),
                             msg='{} was considered invalid but is valid'.format(phrase))

    def test_particularly_valid_passphrases_are_valid(self):
        for phrase in ['abcde fghi', 'a ab abc abd abf abj', 'iiii oiii ooii oooi oooo', 'foo', '']:
            self.assertTrue(is_particularly_valid_passphrase(phrase),
                            msg='{} should be particularly valid'.format(phrase))

    def test_particularly_invalid_passphrases_are_invalid(self):
        for phrase in ['abcde xyz ecdab', 'oiii ioii iioi iiio', 'invalid is kinda like valiidn']:
            self.assertFalse(is_particularly_valid_passphrase(phrase),
                             msg='{} should be particularly invalid'.format(phrase))

    def test_count_valid_passphrases_zero_for_empty_list(self):
        self.assertEqual(0, count_valid_passphrases([], mock_alternation_validity_check()))

    def test_count_valid_passphrases(self):
        check_fn = mock_alternation_validity_check()
        self.assertEqual(5, count_valid_passphrases([1, 2, 3, 4, 5, 6, 7, 8, 9], check_fn))

    def test_count_valid_passphrases_p1_puzzle(self):
        self.assertEqual(383, count_valid_passphrases(self.puzzle_input, is_valid_passphrase))

    def test_count_valid_passphrases_p2_puzzle(self):
        validity_fn = is_particularly_valid_passphrase
        self.assertEqual(265, count_valid_passphrases(self.puzzle_input, validity_fn))
