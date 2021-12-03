#!/usr/bin/env python3
import fileutils


def part1(lst):
    (bit_length, data) = gen_data(lst)
    gamma_rate = get_max(bit_length, data)
    epsilon_rate = ~gamma_rate & (pow(2, bit_length) - 1)
    return gamma_rate * epsilon_rate


def part2(lst):
    (bit_length, data) = gen_data(lst)
    (oxygen, co2) = get_life_support_rating(bit_length, data)
    return oxygen * co2


def get_life_support_rating(bit_length, lst):
    oxygen = lst
    co2 = lst
    for pos in range(bit_length - 1, -1, -1):
        mask = pow(2, pos)
        if len(oxygen) != 1:
            oxygen_check_val = get_max(bit_length, oxygen) & mask
            oxygen = [i for i in oxygen if i & mask == oxygen_check_val]
        if len(co2) != 1:
            co2_check_val = get_max(bit_length, co2) & mask
            co2 = [i for i in co2 if i & mask != co2_check_val]
    return oxygen[0], co2[0]


def get_max(bit_length, data):
    max_value = 0
    for i in range(bit_length - 1, -1, -1):
        mask = pow(2, i)
        ones = len([i for i in data if i & mask == mask])
        zeros = len([i for i in data if i & mask != mask])
        if ones == zeros:
            max_value += mask
        elif ones > zeros:
            max_value += mask
    return max_value


def gen_data(data):
    return len(data[0]), [int(i, 2) for i in data]


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day3.txt")

    print("=== Day 3 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
