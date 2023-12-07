import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day7";
import { readInput } from "../utils/file";

describe("day07", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(7, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(6440);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(5905);
    });
});
