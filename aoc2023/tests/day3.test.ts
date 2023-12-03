import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day3";
import { readInput } from "../utils/file";

describe("day03", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(3, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(4361);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(467835);
    });
});
