import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day12";
import { readInput } from "../utils/file";

describe("day12", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(12, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(21);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(525152);
    });
});
