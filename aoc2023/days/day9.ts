function part1(input: number[][]): number {
    let sum = 0;
    for (const line of input) {
        const differences = getDifferences(line);
        sum += calculateNextValue(differences);
    }
    return sum;
}

function part2(input: number[][]): number {
    let sum = 0;
    for (const line of input) {
        const differences = getDifferences(line);
        sum += calculatePrevValue(differences);
    }
    return sum;
}

function calculatePrevValue(input: number[][]): number {
    let previous = 0;
    for (let i = input.length - 1; i >= 0; i--) {
        const current = input[i][0];
        previous = current - previous;
    }
    return previous;
}

function calculateNextValue(input: number[][]): number {
    let previous = 0;
    for (let i = input.length - 1; i >= 0; i--) {
        const current = input[i][input[i].length - 1];
        previous = current + previous;
    }
    return previous;
}

function getDifferences(input: number[]): number[][] {
    const result: number[][] = [input];
    while (true) {
        const differences = calculateDifference(input);
        if (differences.every((d) => d === 0)) {
            return result;
        }
        result.push(differences);
        input = differences;
    }
}

function calculateDifference(input: number[]): number[] {
    const result: number[] = [];
    for (let i = 1; i < input.length; i++) {
        result.push(input[i] - input[i - 1]);
    }
    return result;
}

export function solve(input: string): [number, number] {
    const lines = input
        .trim()
        .split("\n")
        .map((line) => line.split(" ").map(Number));

    return [part1(lines), part2(lines)];
}
