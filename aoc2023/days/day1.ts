function part1(input: string[]): number {
    return input
        .map((line) => line.replace(/\D/g, ""))
        .map((line) => line[0] + line[line.length - 1])
        .map(Number)
        .reduce((sum, current) => sum + current, 0);
}

function part2(input: string[]): number {
    return input
        .map(convert_line)
        .map((line) => line[0] + line[line.length - 1])
        .map(Number)
        .reduce((sum, current) => sum + current, 0);
    /*
    .map((line) => line.replace(/\D/g, ""))
    .map((line) => line[0] + line[line.length - 1])
    .map(Number)
    .reduce((sum, current) => sum + current, 0);
    */
}

function convert_line(line: string): string {
    let result = "";
    for (let i = 0; i < line.length; i++) {
        const sub = line.slice(i);
        if (sub.startsWith("one")) {
            result += "1";
        } else if (sub.startsWith("two")) {
            result += "2";
        } else if (sub.startsWith("three")) {
            result += "3";
        } else if (sub.startsWith("four")) {
            result += "4";
        } else if (sub.startsWith("five")) {
            result += "5";
        } else if (sub.startsWith("six")) {
            result += "6";
        } else if (sub.startsWith("seven")) {
            result += "7";
        } else if (sub.startsWith("eight")) {
            result += "8";
        } else if (sub.startsWith("nine")) {
            result += "9";
        } else if (sub[0] >= "0" && sub[0] <= "9") {
            result += sub[0];
        }
    }

    return result;
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
