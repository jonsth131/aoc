#!/usr/bin/env python3
import fileutils
import re


def part1(lst):
    parsed = parse_input(lst)
    return count(parsed)


def part2(lst):
    parsed = parse_input(lst)
    c = 0
    for line in parsed:
        mapping = get_mapping(line[0].split())
        res = ''
        for output in line[1].split():
            res += mapping.get(''.join(sorted(output)))
        c += int(res)

    return c


def get_mapping(values):
    one = get_values_with_length(values, 2)[0]
    four = get_values_with_length(values, 4)[0]
    seven = get_values_with_length(values, 3)[0]
    eight = get_values_with_length(values, 7)[0]
    zero_six_nine = get_values_with_length(values, 6)
    two_three_five = get_values_with_length(values, 5)

    three = [i for i in two_three_five if one[0] in i and one[1] in i][0]
    two_three_five.remove(three)
    six = [i for i in zero_six_nine if one[0] not in i or one[1] not in i][0]
    zero_six_nine.remove(six)

    (zero, nine) = get_rest(three, zero_six_nine)
    (five, two) = get_rest(six, two_three_five)

    return {
        ''.join(sorted(zero)): '0',
        ''.join(sorted(one)): '1',
        ''.join(sorted(two)): '2',
        ''.join(sorted(three)): '3',
        ''.join(sorted(four)): '4',
        ''.join(sorted(five)): '5',
        ''.join(sorted(six)): '6',
        ''.join(sorted(seven)): '7',
        ''.join(sorted(eight)): '8',
        ''.join(sorted(nine)): '9'
    }


def get_rest(check, lst):
    first = ''
    second = ''
    for value in lst:
        diff = get_diff(check, value)
        if len(diff) == 1:
            first = value
        else:
            second = value

    return first, second


def get_diff(first, second):
    res = ''
    for i in first:
        if not re.search(i, second):
            res += i
    return res


def get_values_with_length(values, length):
    return [i for i in values if len(i) == length]


def count(lst):
    c = 0
    for line in lst:
        answers = line[1].split()
        for answer in answers:
            answer_len = len(answer)
            if answer_len == 2 or answer_len == 3 or answer_len == 4 or answer_len == 7:
                c += 1
    return c


def parse_input(lst):
    return [i.split(' | ') for i in lst]


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day8.txt")

    print("=== Day 8 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
