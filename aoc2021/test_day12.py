from day12 import part1, part2


test_input = """start-A
start-b
A-c
A-b
b-d
A-end
b-end""".splitlines()


test_input2 = """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc""".splitlines()


def test_part1():
    assert part1(test_input) == 10


def test_part1_input2():
    assert part1(test_input2) == 19


def test_part2():
    assert part2(test_input) == 36


def test_part2_input2():
    assert part2(test_input2) == 103
