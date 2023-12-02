import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day2";
import { readInput } from "../utils/file";

describe("day02", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(2, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(8);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(2286);
    });
});
