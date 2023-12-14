import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day13";
import { readInput } from "../utils/file";

describe("day13", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(13, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(405);
    });

    test("part1 - vertical", async () => {
        const input = `###..##..###.
..#.####.#...
..#.####.#..#
###..##..###.
##........###
#.##.##.##.##
#.########.##`;
        const result = solve(input);
        expect(result[0]).toBe(6);
    });

    test("part1 - horizontal", async () => {
        const input = `##..#..#....#
##..#..##.###
##..#..##.###
##..#..#....#
###.#......#.
....#..#.###.
###..###.#...
####.#.#####.
..##.######.#
##.#..##.#.##
##..#####..#.
##.#.###..#..
...#####...##
.#..#..##.#..
..#......#.##
..#.#####....
##...#.#.####`;
        const result = solve(input);
        expect(result[0]).toBe(200);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(400);
    });

    test("part2 - first input", async () => {
        const result = solve(input.split("\n\n")[0]);
        expect(result[1]).toBe(300);
    });

    test("part2 - second input", async () => {
        const result = solve(input.split("\n\n")[1]);
        expect(result[1]).toBe(100);
    });
});
