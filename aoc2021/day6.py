#!/usr/bin/env python3
import fileutils


def part1(lst):
    state = parse_initial_state(lst)
    state = simulate(state, 80)
    return sum(state.values())


def part2(lst):
    state = parse_initial_state(lst)
    state = simulate(state, 256)
    return sum(state.values())


def simulate(state, times):
    for n in range(times):
        state = tick_state(state)
    return state


def tick_state(state):
    new_state = {}
    new = 0
    for key in state.keys():
        value = state.get(key)
        if key == 0:
            new = value
        else:
            new_state.update({key - 1: value})
    update_state(new_state, 6, new)
    update_state(new_state, 8, new)
    return new_state


def parse_initial_state(data):
    state = {}
    values = [int(x) for x in data.split(',')]
    for i in range(len(values)):
        update_state(state, values[i], 1)
    return state


def update_state(state, key, value):
    existing = state.get(key)
    if existing is None:
        state.update({key: value})
    else:
        state.update({key: existing + value})


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day6.txt")[0]

    print("=== Day 6 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
