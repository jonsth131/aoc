#!/usr/bin/env python3
import fileutils


def part1(lst):
    grid = parse_grid(lst)
    total_flashes = 0
    for _ in range(100):
        total_flashes += flash_grid(grid)
    return total_flashes


def part2(lst):
    grid = parse_grid(lst)
    cell_count = len(grid) * len(grid[0])
    step = 1
    while True:
        flashes = flash_grid(grid)
        if flashes == cell_count:
            return step
        step += 1


def flash_grid(grid):
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            val = grid[y][x] + 1
            grid[y][x] = val
            flash_cell(grid, y, x)
    flashes = update_flashed(grid)
    return flashes


def update_flashed(grid):
    flashes = 0
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] > 9:
                grid[y][x] = 0
                flashes += 1
    return flashes


def flash_cell(grid, y, x):
    val = grid[y][x]
    if val > 10 or val < 10:
        return
    adj = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x + 1, y + 1), (x + 1, y - 1),
           (x - 1, y + 1)]
    for n in adj:
        (x, y) = n
        if 0 <= x <= len(grid[0]) - 1 and 0 <= y <= len(grid) - 1:
            grid[y][x] += 1
            flash_cell(grid, y, x)


def parse_grid(lst):
    grid = []
    for line in lst:
        grid.append([int(i) for i in line])
    return grid


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day11.txt")

    print("=== Day 11 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
