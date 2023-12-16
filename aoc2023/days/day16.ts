enum Direction {
    Up,
    Right,
    Down,
    Left
}

function part1(input: string[]): number {
    return getEnergizedFrom(input, 0, 0, Direction.Right);
}

function part2(input: string[]): number {
    let max = 0;
    for (let i = 0; i < input[0].length; i++) {
        const energizedDown = getEnergizedFrom(input, i, 0, Direction.Down);
        max = Math.max(max, energizedDown);
        const energizedUp = getEnergizedFrom(input, i, input.length - 1, Direction.Up);
        max = Math.max(max, energizedUp);
    }
    for (let i = 0; i < input.length; i++) {
        const energizedRight = getEnergizedFrom(input, 0, i, Direction.Right);
        max = Math.max(max, energizedRight);
        const energizedLeft = getEnergizedFrom(input, input[0].length - 1, i, Direction.Left);
        max = Math.max(max, energizedLeft);
    }
    return max;
}

function getEnergizedFrom(input: string[], x: number, y: number, direction: Direction): number {
    const seen = new Map<string, Direction[]>();
    walk(input, x, y, direction, seen);
    return seen.size;
}

function walk(grid: string[], x: number, y: number, direction: Direction, seen: Map<string, Direction[]>): void {
    if (x < 0 || y < 0 || x >= grid[0].length || y >= grid.length) {
        return;
    }

    const key = `${x},${y}`;

    if (seen.has(key)) {
        const directions = seen.get(key)!;
        if (directions.includes(direction)) {
            return;
        }
        seen.set(key, [...directions, direction]);
    } else {
        seen.set(key, [direction]);
    }

    const current = grid[y][x];

    if (current === ".") {
        const [newX, newY] = getCoordinates(x, y, direction);
        walk(grid, newX, newY, direction, seen);
    } else if (current === "|") {
        if (direction === Direction.Up || direction === Direction.Down) {
            const [newX, newY] = getCoordinates(x, y, direction);
            walk(grid, newX, newY, direction, seen);
        } else {
            const [newXup, newYup] = getCoordinates(x, y, Direction.Up);
            walk(grid, newXup, newYup, Direction.Up, seen);
            const [newXdown, newYdown] = getCoordinates(x, y, Direction.Down);
            walk(grid, newXdown, newYdown, Direction.Down, seen);
        }
    } else if (current === "-") {
        if (direction === Direction.Left || direction === Direction.Right) {
            const [newX, newY] = getCoordinates(x, y, direction);
            walk(grid, newX, newY, direction, seen);
        } else {
            const [newXleft, newYleft] = getCoordinates(x, y, Direction.Left);
            walk(grid, newXleft, newYleft, Direction.Left, seen);
            const [newXright, newYright] = getCoordinates(x, y, Direction.Right);
            walk(grid, newXright, newYright, Direction.Right, seen);
        }
    } else if (current === "/") {
        if (direction === Direction.Up) {
            const [newX, newY] = getCoordinates(x, y, Direction.Right);
            walk(grid, newX, newY, Direction.Right, seen);
        } else if (direction === Direction.Right) {
            const [newX, newY] = getCoordinates(x, y, Direction.Up);
            walk(grid, newX, newY, Direction.Up, seen);
        } else if (direction === Direction.Down) {
            const [newX, newY] = getCoordinates(x, y, Direction.Left);
            walk(grid, newX, newY, Direction.Left, seen);
        } else if (direction === Direction.Left) {
            const [newX, newY] = getCoordinates(x, y, Direction.Down);
            walk(grid, newX, newY, Direction.Down, seen);
        }
    } else if (current === "\\") {
        if (direction === Direction.Up) {
            const [newX, newY] = getCoordinates(x, y, Direction.Left);
            walk(grid, newX, newY, Direction.Left, seen);
        } else if (direction === Direction.Right) {
            const [newX, newY] = getCoordinates(x, y, Direction.Down);
            walk(grid, newX, newY, Direction.Down, seen);
        } else if (direction === Direction.Down) {
            const [newX, newY] = getCoordinates(x, y, Direction.Right);
            walk(grid, newX, newY, Direction.Right, seen);
        } else if (direction === Direction.Left) {
            const [newX, newY] = getCoordinates(x, y, Direction.Up);
            walk(grid, newX, newY, Direction.Up, seen);
        }
    }
}

function getCoordinates(x: number, y: number, direction: Direction): [number, number] {
    switch (direction) {
        case Direction.Up:
            return [x, y - 1];
        case Direction.Right:
            return [x + 1, y];
        case Direction.Down:
            return [x, y + 1];
        case Direction.Left:
            return [x - 1, y];
    }
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
