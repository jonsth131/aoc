type Almanac = {
    seeds: number[];
    maps: Map<string, MapLine[]>;
};

type MapLine = {
    destinationRange: number;
    sourceRange: number;
    len: number;
};

function part1(input: Almanac): number {
    let low = Number.MAX_SAFE_INTEGER;
    for (let i = 0; i < input.seeds.length; i++) {
        let v = input.seeds[i];
        for (const map of input.maps.values()) {
            v = getMappedValue(v, map);
        }
        low = Math.min(low, v);
    }
    return low;
}

function part2(input: Almanac): number {
    const reversedMaps = [...input.maps.values()].reverse();
    let i = 0;
    while (true) {
        let v = i;
        for (const map of reversedMaps) {
            v = getReversedMappedValue(v, map);
        }
        if (inRange(v, input.seeds)) {
            for (const map of input.maps.values()) {
                v = getMappedValue(v, map);
            }
            return v;
        }
        i++;
    }
}

function inRange(v: number, seeds: number[]): boolean {
    for (let i = 0; i < seeds.length; i += 2) {
        if (seeds[i] <= v && v <= seeds[i] + seeds[i + 1]) {
            return true;
        }
    }
    return false;
}

function getReversedMappedValue(input: number, map: MapLine[]): number {
    for (const line of map) {
        const { destinationRange, sourceRange, len } = line;
        if (destinationRange <= input && destinationRange + len > input) {
            return sourceRange + input - destinationRange;
        }
    }
    return input;
}

function getMappedValue(input: number, map: MapLine[]): number {
    for (const line of map) {
        const { destinationRange, sourceRange, len } = line;
        if (sourceRange <= input && sourceRange + len > input) {
            return destinationRange + input - sourceRange;
        }
    }
    return input;
}

function parseAlmanac(input: string): Almanac {
    const blocks = input.split("\n\n");
    const maps = new Map<string, MapLine[]>();

    for (const block of blocks.slice(1)) {
        const [name, ...lines] = block.split("\n");
        const ranges = lines.map((line) => line.split(" ").map(Number));
        const map = createMap(ranges);

        maps.set(name.split(" ")[0], map);
    }

    return {
        seeds: blocks[0].split(": ")[1].split(" ").map(Number),
        maps: maps,
    };
}

function createMap(ranges: number[][]): MapLine[] {
    const map: MapLine[] = [];
    for (const range of ranges) {
        const [destinationRange, sourceRange, length] = range;
        map.push({ destinationRange, sourceRange, len: length });
    }
    return map;
}

export function solve(input: string): [number, number] {
    const almanac = parseAlmanac(input.trim());

    return [part1(almanac), part2(almanac)];
}
