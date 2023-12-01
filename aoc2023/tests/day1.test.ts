import { test, expect, describe } from "bun:test";
import { solve } from "../days/day1";

describe("day01", () => {
    test("part1", async () => {
        const input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet";
        const result = solve(input);
        expect(result[0]).toBe(142);
    });

    test("part2", async () => {
        const input = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
        const result = solve(input);
        expect(result[1]).toBe(281);
    });
});
