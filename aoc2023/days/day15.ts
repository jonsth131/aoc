type Operation = {
    name: string;
    type: "-" | "=";
    value?: number;
}

type LensData = {
    name: string;
    value: number;
}

function part1(input: string[]): number {
    return input
        .map(hash)
        .reduce((a, b) => a + b, 0);
}

function part2(input: string[]): number {
    const operations = input.map(parseOperation);
    const map = createMap(operations);
    return calculateTotalPower(map);
}

function calculateTotalPower(map: Map<number, LensData[]>): number {
    let sum = 0;
    for (const entry of map.entries()) {
        const [key, value] = entry;
        for (let i = 0; i < value.length; i++) {
            sum += (key + 1) * (i + 1) * value[i].value;
        }
    }
    return sum;
}

function createMap(operations: Operation[]): Map<number, LensData[]> {
    const map = new Map<number, LensData[]>();
    for (const operation of operations) {
        const h = hash(operation.name);
        if (operation.type === "=") {
            const m = map.get(h) || [];
            const existing = m.find((x) => x.name === operation.name);
            if (existing) {
                existing.value = operation.value!;
            } else {
                m.push({
                    name: operation.name,
                    value: operation.value!,
                });
            }
            map.set(h, m);
        } else if (operation.type === "-") {
            const m = map.get(h) || [];
            const index = m.findIndex((x) => x.name === operation.name);
            if (index === -1) {
                continue;
            }
            m.splice(index, 1);
            if (m.length === 0) {
                map.delete(h);
            } else {
                map.set(h, m);
            }
        }
    }
    return map;
}

function parseOperation(input: string): Operation {
    if (input.endsWith("-")) {
        return {
            name: input.substring(0, input.length - 1),
            type: "-",
        };
    } else {
        const [name, value] = input.split("=");
        return {
            name,
            type: "=",
            value: Number(value),
        };
    }
}

function hash(input: string): number {
    let sum = 0;
    for (let i = 0; i < input.length; i++) {
        sum += input.charCodeAt(i);
        sum *= 17;
        sum %= 256;
    }
    return sum;
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split(",");

    return [part1(lines), part2(lines)];
}
