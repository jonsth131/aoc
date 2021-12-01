from day1 import measure


def test_part1():
    data = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
    assert measure(data, 1) == 7


def test_part2():
    data = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
    assert measure(data, 3) == 5
