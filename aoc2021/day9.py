#!/usr/bin/env python3
import fileutils


def part1(lst):
    height_map = parse_heightmap(lst)
    low_points = get_low_points(height_map)
    risk_levels = get_risk_levels(low_points)
    return sum(risk_levels)


def part2(lst):
    height_map = parse_heightmap(lst)
    basins = get_basins(height_map)
    basin_sizes = [get_basin_size(b) for b in basins]
    top3 = (sorted(basin_sizes)[-3:])
    return top3[0] * top3[1] * top3[2]


def get_basins(height_map):
    basins = []
    for y in range(len(height_map)):
        for x in range(len(height_map[0])):
            if point_exists_in_basin(x, y, basins):
                continue
            basin = [[False for _ in range(len(height_map[0]))] for _ in range(len(height_map))]
            get_basin(height_map, basin, x, y)
            if should_add_basin(basin, basins) and get_basin_size(basin) != 0:
                basins.append(basin)
    return basins


def point_exists_in_basin(x, y, basins):
    for b in basins:
        if b[y][x] is True:
            return True
    return False


def should_add_basin(new, existing):
    for b in existing:
        if are_same(new, b):
            return False
    return True


def are_same(m1, m2):
    for i in range(len(m1)):
        for j in range(len(m1[0])):
            if m1[i][j] != m2[i][j]:
                return False
    return True


def get_basin_size(basin):
    c = 0
    for line in basin:
        for value in line:
            if value is True:
                c += 1
    return c


def get_basin(height_map, basin, x, y):
    if height_map[y][x] == 9:
        return
    elif basin[y][x] is True:
        return
    else:
        basin[y][x] = True
        adj = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        for n in adj:
            (x, y) = n
            if 0 <= x <= len(height_map[0]) - 1 and 0 <= y <= len(height_map) - 1:
                get_basin(height_map, basin, x, y)


def get_risk_levels(low_points):
    return [i + 1 for i in low_points]


def get_low_points(height_map):
    low_points = []
    for y in range(len(height_map)):
        for x in range(len(height_map[0])):
            curr = height_map[y][x]
            adj = get_adjacent(height_map, x, y)
            if is_low(curr, adj):
                low_points.append(curr)
    return low_points


def is_low(curr, adj):
    for x in adj:
        if curr >= x:
            return False
    return True


def get_adjacent(height_map, x, y):
    adj = []
    if x != 0:
        adj.append(height_map[y][x - 1])
    if x != len(height_map[0]) - 1:
        adj.append(height_map[y][x + 1])
    if y != 0:
        adj.append(height_map[y - 1][x])
    if y != len(height_map) - 1:
        adj.append(height_map[y + 1][x])
    return adj


def parse_heightmap(lst):
    lines = []
    for line in lst:
        parsed_line = [int(i) for i in line]
        lines.append(parsed_line)
    return lines


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day9.txt")

    print("=== Day 9 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
