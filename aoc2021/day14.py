#!/usr/bin/env python3
import fileutils
from collections import Counter, defaultdict


def part1(lst):
    state = parse_input(lst)
    for _ in range(10):
        state['template'] = process_rules(state['template'], state['rules'])

    res = list(get_values(state['template']))
    return max(res) - min(res)


def part2(lst):
    state = parse_input(lst)
    for _ in range(40):
        state['template'] = process_rules(state['template'], state['rules'])

    res = list(get_values(state['template']))
    return max(res) - min(res)


def process_rules(template, rules):
    new_template = defaultdict(int)
    for pair, count in template.items():
        new_template[pair[0] + rules[pair]] += count
        new_template[rules[pair] + pair[1]] += count
    return new_template


def get_values(template):
    for e in set(''.join(template)):
        yield max(sum(count for (first, _), count in template.items() if e == first),
                  sum(count for (_, second), count in template.items() if e == second))


def parse_input(lst):
    template = []
    initial = lst[0]
    for first, second in zip(initial, initial[1:]):
        template.append(first+second)
    rules = dict()
    for line in lst[2:]:
        rules[line[:2]] = line[-1]
    return {'template': dict(Counter(template)), 'rules': rules}


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day14.txt")

    print("=== Day 14 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
