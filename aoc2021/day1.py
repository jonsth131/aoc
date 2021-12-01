#!/usr/bin/env python3


def part1(input):
    return measure(input, 1)


def part2(input):
    return measure(input, 3)


def measure(input, size):
    win = window(input, size)
    increases = 0
    prev = None

    for item in win:
        value = sum(item)

        if prev != None and value > prev:
            increases += 1

        prev = value 

    return increases


def window(lst, size):
    for i in range(len(lst) - size + 1):
        yield lst[i:i+size]

def test_part1():
    input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

    assert part1(input) == 7


def test_part2():
    input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

    assert part2(input) == 5


def read_as_ints(filename):
    with open(filename, 'r') as input:
        data = input.readlines()
        return [int(x) for x in data]


if __name__ == "__main__":
    data = read_as_ints("inputs/day1.txt")

    print("=== Day 1 ===")
    print("Part 1:", part1(data))
    print("Part 2:", part2(data))
