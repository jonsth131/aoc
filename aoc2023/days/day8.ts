import { lcm } from "../utils/math";

type NetworkMap = {
    instructions: string;
    map: Map<string, [string, string]>;
}

function part1(input: NetworkMap): number {
    let currentKey = "AAA";
    return getHops(input, [currentKey], "ZZZ")[0];
}

function part2(input: NetworkMap): number {
    let keys: string[] = [];
    for (const [key, _] of input.map.entries()) {
        if (key.endsWith("A")) {
            keys.push(key);
        }
    }

    const counts = getHops(input, keys, "Z");
    return lcm(counts);
}

function getHops(input: NetworkMap, keys: string[], end: string): number[] {
    let counts = [];
    for (const key of keys) {
        let count = 0;
        let currentKey = key;
        while (!currentKey.endsWith(end)) {
            const current = input.map.get(currentKey)!;
            if (current === undefined) {
                return [];
            }

            const instruction = input.instructions[count++ % input.instructions.length];
            if (instruction === "L") {
                currentKey = current[0];
            } else {
                currentKey = current[1];
            }
        }
        counts.push(count);
    }
    return counts;
}

function parseNodes(input: string): NetworkMap {
    const [path, map] = input.trim().split("\n\n");
    const nodes = new Map<string, [string, string]>();
    for (const line of map.split("\n")) {
        const [label, rest] = line.split(" = ");
        const [left, right] = rest.replace("(", "").replace(")", "").split(", ");
        nodes.set(label, [left, right]);
    }
    return { instructions: path, map: nodes };
}

export function solve(input: string): [number, number] {
    const nodes = parseNodes(input);

    return [part1(nodes), part2(nodes)];
}
