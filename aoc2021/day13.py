#!/usr/bin/env python3
import fileutils


def part1(lst):
    state = parse_state(lst)
    fold(state, state['fold'][0])
    return len(state['points'])


def part2(lst):
    state = parse_state(lst)
    fold_all(state)
    print_state(state)


def print_state(state):
    for y in range(state['height']):
        for x in range(state['width']):
            if state['points'].count((x, y)) == 0:
                print('.', end='')
            else:
                print('#', end='')
        print()


def fold_all(state):
    for ins in state['fold']:
        fold(state, ins)


def fold(state, ins):
    (axis, pos) = ins
    if axis == 'y':
        fold_y(state['points'], pos, state['height'])
        state['height'] -= pos + 1
    if axis == 'x':
        fold_x(state['points'], pos, state['width'])
        state['width'] -= pos + 1


def fold_x(points, pos, width):
    for point in points.copy():
        (x, y) = point
        if x > pos:
            new_point = (abs(x-width+1), y)
            if points.count(new_point) == 0:
                points.append(new_point)
            points.remove(point)


def fold_y(points, pos, height):
    for point in points.copy():
        (x, y) = point
        if y > pos:
            new_point = (x, abs(y-height+1))
            if points.count(new_point) == 0:
                points.append(new_point)
            points.remove(point)


def parse_state(lst):
    state = {'points': [], 'fold': []}
    i = 0
    height = 0
    width = 0
    while lst[i] != '':
        values = lst[i].split(',')
        x = int(values[0])
        y = int(values[1])
        height = max([height, y])
        width = max([width, x])
        state['points'].append((x, y))
        i += 1
    i += 1
    while i < len(lst):
        axis = lst[i][11:12]
        pos = lst[i][13:]
        state['fold'].append((axis, int(pos)))
        i += 1
    state['height'] = height + 1
    state['width'] = width + 1
    return state


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day13.txt")

    print("=== Day 13 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:")
    part2(challenge_input)
