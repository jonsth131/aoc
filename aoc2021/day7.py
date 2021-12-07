#!/usr/bin/env python3
import fileutils


def part1(lst):
    positions = parse_positions(lst)
    return get_min_fuel(positions, True)


def part2(lst):
    positions = parse_positions(lst)
    return get_min_fuel(positions, False)


def get_min_fuel(positions, is_part1):
    fuel = [None] * max(positions.keys())
    for i in range(len(fuel)):
        value = 0
        if is_part1 is True:
            for x in positions.keys():
                value += abs(x - i) * positions.get(x)
        else:
            for x in positions.keys():
                n = abs(x - i)
                value += (n * (n + 1) / 2) * positions.get(x)
        fuel[i] = int(value)
    return min(fuel)


def parse_positions(data):
    positions = {}
    for value in [int(x) for x in data.split(',')]:
        existing = positions.get(value)
        if existing is None:
            positions.update({value: 1})
        else:
            positions.update({value: existing + 1})
    return positions


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day7.txt")[0]

    print("=== Day 7 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
