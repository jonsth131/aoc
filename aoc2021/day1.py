#!/usr/bin/env python3
import fileutils


def part1(lst):
    return measure(window(lst, 1))


def part2(lst):
    return measure(window(lst, 3))


def measure(windows):
    increases = 0
    prev = None

    for value in [sum(x) for x in windows]:
        if prev is not None and value > prev:
            increases += 1
        prev = value

    return increases


def window(lst, size):
    for i in range(len(lst) - size + 1):
        yield lst[i:i + size]


if __name__ == "__main__":
    data = fileutils.read_as_ints("inputs/day1.txt")

    print("=== Day 1 ===")
    print("Part 1:", part1(data))
    print("Part 2:", part2(data))
