#!/usr/bin/env python3
import fileutils


error_score_map = {')': 3, ']': 57, '}': 1197, '>': 25137}
incomplete_score_map = {')': 1, ']': 2, '}': 3, '>': 4}


def part1(lst):
    acc = 0
    for line in lst:
        (_, err) = parse_line(line)
        if err is None:
            continue
        acc += error_score_map[err]
    return acc


def part2(lst):
    scores = []
    for line in lst:
        acc = 0
        (incomplete, _) = parse_line(line)
        if incomplete is None:
            continue
        while len(incomplete) != 0:
            o = incomplete.pop()
            acc = (acc * 5) + incomplete_score_map[get_closing(o)]
        scores.append(acc)
    scores = sorted(scores)
    return scores[(len(scores) - 1)//2]


def get_closing(o):
    val = ord(o)
    if val > 0x30:
        return chr(val + 2)
    else:
        return chr(val + 1)


def parse_line(line):
    seq = []
    for c in line:
        if c in '([{<':
            seq.append(c)
        else:
            o = seq.pop()
            if c != get_closing(o):
                return None, c
    return seq, None


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day10.txt")

    print("=== Day 10 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
