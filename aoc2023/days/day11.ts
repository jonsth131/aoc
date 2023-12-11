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
            const emptyColsInPath = getEmptyInPath(map.galaxies[i].x, map.galaxies[j].x, map.emptyCols);
            const emptyRowsInPath = getEmptyInPath(map.galaxies[i].y, map.galaxies[j].y, map.emptyRows);
            total += shortestPath + (emptyColsInPath * (age - 1)) + (emptyRowsInPath * (age - 1));
        }
    }
    return total;
}

function getEmptyInPath(start: number, end: number, empty: number[]): number {
    let result = 0;
    const min = Math.min(start, end);
    const max = Math.max(start, end);
    for (let i = min; i <= max; i++) {
        if (empty.includes(i)) {
            result++;
        }
    }
    return result;
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
