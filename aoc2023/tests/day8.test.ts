import { test, expect, describe } from "bun:test";
import { solve } from "../days/day8";

describe("day08", () => {
    test("part1", async () => {
        const input = `LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)`;
        const result = solve(input);
        expect(result[0]).toBe(6);
    });

    test("part2", async () => {
        const input = `LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)`;
        const result = solve(input);
        expect(result[1]).toBe(6);
    });
});
