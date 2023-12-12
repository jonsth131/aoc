function part1(input: string[]): number {
    let possibilities = 0;
    for (const line of input) {
        const [row, rest] = line.split(" ");
        const springs = rest.split(",").map(Number);
        const result = getPossibilities(row, springs, new Map());
        possibilities += result;
    }
    return possibilities;
}

function part2(input: string[]): number {
    let possibilities = 0;
    for (const line of input) {
        const [row, rest] = line.split(" ");
        const springs = rest.split(",").map(Number);
        const expandedRow = row + "?" + row + "?" + row + "?" + row + "?" + row;
        const expandedSprings = [...springs, ...springs, ...springs, ...springs, ...springs];
        const result = getPossibilities(expandedRow, expandedSprings, new Map());
        possibilities += result;
    }
    return possibilities;
}

function getPossibilities(line: string, springs: number[], cache: Map<string, number>): number {
    const key = line + "|" + springs.join(",");
    if (cache.has(key)) {
        return cache.get(key)!;
    }

    if (line.length === 0) {
        if (springs.length === 0) {
            return 1;
        }
        return 0;
    }

    if (springs.length === 0) {
        if (line.includes("#")) {
            return 0;
        }
        return 1;
    }

    if (line.length < springs.reduce((a, b) => a + b)) {
        return 0;
    }

    if (line[0] === ".") {
        const result = getPossibilities(line.substring(1), springs, cache);
        cache.set(key, result);
        return result;
    }

    if (line[0] === "#") {
        const spring = springs[0];
        const checkLine = line.substring(0, spring);
        if (checkLine.includes(".")) {
            return 0;
        }
        if (line[spring] === "#") {
            return 0;
        }
        const result = getPossibilities(line.substring(spring + 1), springs.slice(1), cache);
        cache.set(key, result);
        return result;
    }

    return getPossibilities("#" + line.substring(1), springs, cache)
        + getPossibilities("." + line.substring(1), springs, cache);
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
