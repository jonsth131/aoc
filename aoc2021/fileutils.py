def read_as_ints(filename):
    with open(filename, 'r') as input:
        data = input.readlines()
        return [int(x) for x in data]