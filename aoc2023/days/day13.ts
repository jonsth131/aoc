function part1(input: string[]): number {
    let sum = 0;
    for (const block of input) {
        const lines = block.split("\n");
        const transposed = lines[0]
            .split("")
            .map((_, i) => lines.map(x => x[i]).reverse().join(""));

        sum += getMirror(lines) * 100;
        sum += getMirror(transposed);
    }

    return sum;
}

function part2(input: string[]): number {
    let sum = 0;
    for (const block of input) {
        const lines = block.split("\n");
        const transposed = lines[0]
            .split("")
            .map((_, i) => lines.map(x => x[i]).reverse().join(""));

        sum += getMirror(lines, 1) * 100;
        sum += getMirror(transposed, 1);
    }

    return sum;
}

function getMirror(input: string[], tolerance: number = 0): number {
    for (let i = 0; i < input.length - 1; i++) {
        const differences = getTotalDiff(input, i);
        if (differences === 0 && tolerance === 0) {
            return i + 1;
        } else if (tolerance !== 0 && differences === tolerance) {
            return i + 1;
        }
    }
    return 0;
}

function getTotalDiff(input: string[], start: number): number {
    let differences = 0;
    for (let i = 0; i < input.length - start - 1; i++) {
        const first = input[start - i];
        const second = input[start + i + 1];
        if (first === undefined || second === undefined) {
            return differences;
        }
        if (first !== second) {
            differences += getDifferencesInString(first, second);
        }
    }
    return differences;
}

function getDifferencesInString(s1: string, s2: string): number {
    let differences = 0;
    for (let i = 0; i < s1.length; i++) {
        if (s1[i] !== s2[i]) {
            differences++;
        }
    }
    return differences;
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n\n");

    return [part1(lines), part2(lines)];
}
