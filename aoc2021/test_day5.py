from day5 import part1, part2


test_input = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2""".splitlines()


def test_part1():
    assert part1(test_input) == 5


def test_part2():
    assert part2(test_input) == 12
