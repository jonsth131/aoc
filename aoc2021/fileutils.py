def read_as_ints(filename):
    with open(filename, 'r') as data:
        return [int(x) for x in data.readlines()]


def read_lines(filename):
    with open(filename, 'r') as data:
        return data.readlines()
