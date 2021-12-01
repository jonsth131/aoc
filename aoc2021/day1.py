#!/usr/bin/env python3
from fileutils import read_as_ints


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


if __name__ == "__main__":
    data = read_as_ints("inputs/day1.txt")

    print("=== Day 1 ===")
    print("Part 1:", part1(data))
    print("Part 2:", part2(data))
