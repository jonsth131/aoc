enum Types {
    None,
    Rounded,
    Cube,
}

function part1(input: string[]): number {
    const grid = parseGrid(input);
    tilt(grid);
    return calculateLoad(grid);
}

function part2(input: string[]): number {
    const grid = parseGrid(input);
    const cycled = cycle(grid, 1000000000);
    return calculateLoad(cycled);
}

function transpose(grid: Types[][]): Types[][] {
    return grid[0].map((_, i) => grid.map(x => x[i]).reverse());
}

function calculateLoad(grid: Types[][]): number {
    const max = grid.length;
    let sum = 0;
    for (let i = 0; i < grid.length; i++) {
        for (const type of grid[i]) {
            if (type === Types.Rounded) {
                sum += max - i;
            }
        }
    }
    return sum;
}

function cycle(grid: Types[][], times: number): Types[][] {
    const min = getMinCycles([...grid], times);
    for (let i = 0; i < min; i++) {
        for (let j = 0; j < 4; j++) {
            tilt(grid);
            grid = transpose(grid);
        }
    }
    return grid;
}

function getMinCycles(grid: Types[][], times: number): number {
    const seen: Map<string, number> = new Map();
    for (let i = 0; i < times; i++) {
        for (let j = 0; j < 4; j++) {
            tilt(grid);
            grid = transpose(grid);
        }
        const key = grid.map(x => x.join("")).join("");
        if (seen.has(key)) {
            const val = seen.get(key)!;
            return val + ((times - val) % (i + 1 - val));
        }
        seen.set(key, i + 1);
    }
    return 0;
}

function tilt(grid: Types[][]): void {
    for (let y = 1; y < grid.length; y++) {
        for (let x = 0; x < grid[y].length; x++) {
            if (grid[y][x] === Types.Rounded) {
                move(grid, x, y);
            }
        }
    }
}

function move(grid: Types[][], x: number, y: number): void {
    let next = grid[y - 1][x];
    while (next === Types.None) {
        grid[y - 1][x] = grid[y][x];
        grid[y][x] = Types.None;
        y--;
        if (y === 0) {
            break;
        }
        next = grid[y - 1][x];
    }
}

function parseGrid(input: string[]): Types[][] {
    const grid: Types[][] = [];
    for (const line of input) {
        const row: Types[] = [];
        for (const char of line) {
            switch (char) {
                case ".":
                    row.push(Types.None);
                    break;
                case "O":
                    row.push(Types.Rounded);
                    break;
                case "#":
                    row.push(Types.Cube);
                    break;
            }
        }
        grid.push(row);
    }
    return grid;
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
