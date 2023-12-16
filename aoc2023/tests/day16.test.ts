import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day16";
import { readInput } from "../utils/file";

describe("day16", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(16, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(46);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(51);
    });
});
