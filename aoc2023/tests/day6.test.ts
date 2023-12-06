import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day6";
import { readInput } from "../utils/file";

describe("day06", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(6, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(288);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(71503);
    });
});
