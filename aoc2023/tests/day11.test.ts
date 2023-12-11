import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day11";
import { readInput } from "../utils/file";

describe("day11", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(11, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(374);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(82000210);
    });
});
