from day9 import part1, part2


test_input = """2199943210
3987894921
9856789892
8767896789
9899965678""".splitlines()


def test_part1():
    assert part1(test_input) == 15


def test_part2():
    assert part2(test_input) == 1134
