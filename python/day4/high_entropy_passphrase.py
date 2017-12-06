from sys import stdin, argv


def is_valid_passphrase(line):
    words = line.split()
    uniqwords = set(line.split())
    return len(uniqwords) == len(words)


def is_particularly_valid_passphrase(line):
    if not is_valid_passphrase(line):
        return False
    words = line.split()
    sorted_words = {''.join(sorted(w)) for w in words}
    return len(sorted_words) == len(words)


def count_valid_passphrases(passphrases, validity_fn):
    return sum(1 for x in passphrases if validity_fn(x))


if __name__ == '__main__':
    def read_input():
        return [x for x in stdin.readlines() if x.strip() != '']

    if '-p1' in argv:
        print count_valid_passphrases(read_input(), is_valid_passphrase)
    elif '-p2' in argv:
        print count_valid_passphrases(read_input(), is_particularly_valid_passphrase)
    else:
        print 'Please use -p1 or -p2 to specify which part to run (Part 1, Part 2, respectively)'
