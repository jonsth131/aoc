import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day14";
import { readInput } from "../utils/file";

describe("day14", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(14, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(136);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(64);
    });
});
