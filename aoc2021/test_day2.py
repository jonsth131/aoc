from day2 import part1, part2


def test_part1():
    data = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
    assert part1(data) == 150


def test_part2():
    data = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
    assert part2(data) == 900
