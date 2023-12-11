import { Point } from "../utils/point";

type GalaxyMap = {
    galaxies: Point[];
    map: string[];
    emptyCols: number[];
    emptyRows: number[];
}

function part1(map: GalaxyMap): number {
    return calculateTotalDistance(map, 2);
}

function part2(map: GalaxyMap): number {
    return calculateTotalDistance(map, 1000000);
}

function parseGalaxyMap(input: string[]): GalaxyMap {
    return {
        galaxies: getGalaxies(input),
        map: input,
        emptyCols: getEmptyCols(input),
        emptyRows: getEmptyRows(input),
    };
}

function calculateTotalDistance(map: GalaxyMap, age: number): number {
    let total = 0;
    for (let i = 0; i < map.galaxies.length - 1; i++) {
        for (let j = i + 1; j < map.galaxies.length; j++) {
            const shortestPath = getShortestPath(map.galaxies[i], map.galaxies[j]);
            const emptyColsInPath = getEmptyColsInPath(map.galaxies[i], map.galaxies[j], map.emptyCols);
            const emptyRowsInPath = getEmptyRowsInPath(map.galaxies[i], map.galaxies[j], map.emptyRows);
            total += shortestPath + (emptyColsInPath * (age - 1)) + (emptyRowsInPath * (age - 1));
        }
    }
    return total;
}

function getEmptyRowsInPath(start: Point, end: Point, emptyRows: number[]): number {
    const result: number[] = [];
    const minY = Math.min(start.y, end.y);
    const maxY = Math.max(start.y, end.y);
    for (let i = minY; i <= maxY; i++) {
        if (emptyRows.includes(i)) {
            result.push(i);
        }
    }
    return result.length;;
}

function getEmptyColsInPath(start: Point, end: Point, emptyCols: number[]): number {
    const result: number[] = [];
    const minX = Math.min(start.x, end.x);
    const maxX = Math.max(start.x, end.x);
    for (let i = minX; i <= maxX; i++) {
        if (emptyCols.includes(i)) {
            result.push(i);
        }
    }
    return result.length;;
}

function getShortestPath(start: Point, end: Point): number {
    return Math.abs(start.x - end.x) + Math.abs(start.y - end.y);
}

function getGalaxies(input: string[]): Point[] {
    const result: Point[] = [];
    for (let y = 0; y < input.length; y++) {
        const row = input[y];
        for (let x = 0; x < row.length; x++) {
            if (row[x] === "#") {
                result.push({ x, y });
            }
        }
    }
    return result;
}

function getEmptyCols(input: string[]): number[] {
    const result: number[] = [];
    for (let i = 0; i < input[0].length; i++) {
        if (colEmpty(input, i)) {
            result.push(i);
        }
    }
    return result;
}

function getEmptyRows(input: string[]): number[] {
    const result: number[] = [];
    for (let i = 0; i < input.length; i++) {
        if (input[i].split("").every((c) => c === ".")) {
            result.push(i);
        }
    }
    return result;
}

function colEmpty(input: string[], col: number): boolean {
    for (const line of input) {
        if (line[col] !== ".") {
            return false;
        }
    }
    return true;
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");
    const map = parseGalaxyMap(lines);

    return [part1(map), part2(map)];
}
