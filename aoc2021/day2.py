#!/usr/bin/env python3
import fileutils


def part1(lines):
    data = gen_data(lines)
    h_pos = 0
    depth = 0
    for (direction, amount) in data:
        if direction == "forward":
            h_pos += amount
        elif direction == "up":
            depth -= amount
        elif direction == "down":
            depth += amount

    return h_pos * depth


def part2(lines):
    data = gen_data(lines)
    h_pos = 0
    depth = 0
    aim = 0
    for (direction, amount) in data:
        if direction == "forward":
            h_pos += amount
            depth += amount * aim
        elif direction == "up":
            aim -= amount
        elif direction == "down":
            aim += amount

    return h_pos * depth


def gen_data(lines):
    for line in [x.strip().split(' ') for x in lines]:
        yield line[0], int(line[1])


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day2.txt")

    print("=== Day 2 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
