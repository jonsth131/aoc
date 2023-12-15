import { test, expect, describe, beforeAll } from "bun:test";
import { solve } from "../days/day15";
import { readInput } from "../utils/file";

describe("day15", () => {
    let input: string;

    beforeAll(async () => {
        input = await readInput(15, true);
    });

    test("part1", async () => {
        const result = solve(input);
        expect(result[0]).toBe(1320);
    });

    test("part2", async () => {
        const result = solve(input);
        expect(result[1]).toBe(145);
    });
});
