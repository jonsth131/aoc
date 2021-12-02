from day2 import part1, part2


test_input = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]


def test_part1():
    assert part1(test_input) == 150


def test_part2():
    assert part2(test_input) == 900
