#!/usr/bin/env python3
import fileutils


def part1(lst):
    lines = gen_lines(lst)
    lines = filter_lines(lines)
    return get_overlapping(lines)


def part2(lst):
    lines = [line for line in gen_lines(lst)]
    return get_overlapping(lines)


def get_overlapping(lines):
    diagram = {}
    while len(lines) != 0:
        current = lines.pop()
        points = gen_line_points(current)
        for point in points:
            existing = diagram.get(point)
            if existing is None:
                diagram.update({point: 1})
            else:
                diagram.update({point: existing + 1})

    values = [x for x in diagram.values() if x > 1]
    return len(values)


def gen_line_points(line):
    x_values = [line[0][0], line[1][0]]
    y_values = [line[0][1], line[1][1]]
    max_x = max(x_values)
    min_x = min(x_values)
    max_y = max(y_values)
    min_y = min(y_values)

    points = []
    if max_x == min_x:
        for n in range(min_y, max_y + 1):
            points.append((max_x, n))
    elif max_y == min_y:
        for n in range(min_x, max_x + 1):
            points.append((n, max_y))
    else:
        (x1, y1) = line[0]
        (x2, y2) = line[1]
        i = 0
        for n in range(min_x, max_x + 1):
            if y1 > y2:
                if x1 > x2:
                    points.append((n, min_y + i))
                else:
                    points.append((n, max_y - i))
            elif y2 >= y1:
                if x1 > x2:
                    points.append((n, max_y - i))
                else:
                    points.append((n, min_y + i))
            i += 1

    return points


def filter_lines(lines):
    return [line for line in lines if line[0][0] == line[1][0] or line[0][1] == line[1][1]]


def gen_lines(lst):
    for line in lst:
        points = line.split(' -> ')
        yield parse_point(points[0]), parse_point(points[1])


def parse_point(p):
    point = p.split(',')
    return int(point[0]), int(point[1])


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day5.txt")

    print("=== Day 5 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
