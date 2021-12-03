#!/usr/bin/env python3
import fileutils


def part1(lst):
    (gamma_rate, epsilon_rate) = get_min_max(lst)
    return int(gamma_rate, 2) * int(epsilon_rate, 2)


def part2(lst):
    (oxygen, co2) = get_life_support_rating(lst)
    return int(oxygen, 2) * int(co2, 2)


def get_life_support_rating(lst):
    oxygen = lst
    co2 = lst
    for pos in range(len(lst[0])):
        if len(oxygen) != 1:
            oxygen_check_val = get_max_occurrences(oxygen, pos)
            oxygen = [i for i in oxygen if i[pos] == oxygen_check_val]
        if len(co2) != 1:
            co2_check_val = get_min_occurrences(co2, pos)
            co2 = [i for i in co2 if i[pos] == co2_check_val]
    return oxygen[0], co2[0]


def get_max_occurrences(lst, pos):
    (ones, zeroes) = get_occurrences(lst, pos)
    if ones == zeroes:
        return '1'
    elif ones > zeroes:
        return '1'
    else:
        return '0'


def get_min_occurrences(lst, pos):
    (ones, zeroes) = get_occurrences(lst, pos)
    if ones == zeroes:
        return '0'
    elif ones > zeroes:
        return '0'
    else:
        return '1'


def get_occurrences(lst, pos):
    pos_values = [x[pos] for x in lst]
    ones = pos_values.count('1')
    zeroes = pos_values.count('0')
    return ones, zeroes


def get_min_max(lst):
    max_val = ''
    min_val = ''
    length = len(lst[0])
    for pos in range(length):
        max_val += get_max_occurrences(lst, pos)
        min_val += get_min_occurrences(lst, pos)
    return max_val, min_val


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day3.txt")

    print("=== Day 3 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
