#!/usr/bin/env python3
import fileutils


def measure(lst, size):
    windows = window(lst, size)
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
    print("Part 1:", measure(data, 1))
    print("Part 2:", measure(data, 3))
