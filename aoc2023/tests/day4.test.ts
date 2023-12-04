import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day4";
import { readInput } from "../utils/file";

describe("day04", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(4, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(13);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(30);
    });
});
