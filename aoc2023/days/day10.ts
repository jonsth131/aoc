import { Point } from "../utils/point";

enum Direction {
    Up,
    Right,
    Down,
    Left,
    None,
}

type Start = {
    point: Point;
    direction: Direction;
};

type Maze = {
    grid: string[];
    start: Start;
    end: Point;
};

function part1(maze: Maze): number {
    const seen = maze.grid.map((_) => _.split("").map((_) => false));
    const path: Point[] = [];
    walk(maze, maze.start.point, maze.start.direction, seen, path);
    return path.length / 2;
}

function part2(maze: Maze): number {
    const seen = maze.grid.map((_) => _.split("").map((_) => false));
    const path: Point[] = [];
    walk(maze, maze.start.point, maze.start.direction, seen, path);
    const area = getTotalArea(maze.grid, path);
    return area - (path.length / 2) + 1;
}

function getTotalArea(grid: string[], sections: Point[]): number {
    const corners = sections.filter((pos) => grid[pos.y][pos.x] !== "." && grid[pos.y][pos.x] !== "|" && grid[pos.y][pos.x] !== "-");
    let sum = 0;
    for (let i = 1; i < corners.length; i++) {
        const prev = corners[i - 1];
        const curr = corners[i];
        sum += (prev.y + curr.y) * (prev.x - curr.x);
    }
    sum += (corners[corners.length - 1].y + corners[0].y) * (corners[corners.length - 1].x - corners[0].x);
    return Math.abs(sum) / 2;
}

function walk(maze: Maze, curr: Point, prevDirection: Direction, seen: boolean[][], path: Point[]): boolean {
    if (curr.x < 0 || curr.x >= maze.grid[0].length || curr.y < 0 || curr.y >= maze.grid.length) {
        return false;
    }
    if (seen[curr.y][curr.x]) {
        return false;
    }
    if (curr.x === maze.end.x && curr.y === maze.end.y && prevDirection !== Direction.None) {
        path.push(curr);
        return true;
    }

    seen[curr.y][curr.x] = true;
    path.push(curr);
    const nextDirection = getNextDirection(maze, curr, prevDirection);
    const nextPoint = getNextPoint(curr, nextDirection);

    if (walk(maze, nextPoint, nextDirection, seen, path)) {
        return true;
    }

    path.pop();

    return true;
}

function getNextPoint(curr: Point, direction: Direction): Point {
    switch (direction) {
        case Direction.Up:
            return { x: curr.x, y: curr.y - 1 };
        case Direction.Right:
            return { x: curr.x + 1, y: curr.y };
        case Direction.Down:
            return { x: curr.x, y: curr.y + 1 };
        case Direction.Left:
            return { x: curr.x - 1, y: curr.y };
        default:
            return curr;
    }
}

function getNextDirection(maze: Maze, curr: Point, prevDirection: Direction): Direction {
    const currTile = maze.grid[curr.y][curr.x];
    switch (currTile) {
        case "|":
            return prevDirection === Direction.Up ? Direction.Up : Direction.Down;
        case "-":
            return prevDirection === Direction.Left ? Direction.Left : Direction.Right;
        case "L":
            return prevDirection === Direction.Left ? Direction.Up : Direction.Right;
        case "J":
            return prevDirection === Direction.Right ? Direction.Up : Direction.Left;
        case "7":
            return prevDirection === Direction.Right ? Direction.Down : Direction.Left;
        case "F":
            return prevDirection === Direction.Left ? Direction.Down : Direction.Right;
        default:
            return Direction.None;
    }
}

function parseMaze(input: string): Maze {
    const grid = input.split("\n");
    const start = findStart(grid);
    const end = findEnd(grid);
    return { grid, start, end };
}

function findStart(grid: string[]): Start {
    const end = findEnd(grid);
    if (end.y > 0 && canWalk(grid[end.y - 1][end.x], Direction.Up)) {
        return { direction: Direction.Up, point: { x: end.x, y: end.y - 1 } };
    } else if (end.x < grid[0].length && canWalk(grid[end.y][end.x + 1], Direction.Right)) {
        return { direction: Direction.Right, point: { x: end.x + 1, y: end.y } };
    } else if (end.y < grid.length && canWalk(grid[end.y + 1][end.x], Direction.Down)) {
        return { direction: Direction.Down, point: { x: end.x, y: end.y + 1 } };
    } else if (end.x > 0 && canWalk(grid[end.y][end.x - 1], Direction.Left)) {
        return { direction: Direction.Left, point: { x: end.x - 1, y: end.y } };
    }
    return { direction: Direction.None, point: { x: 0, y: 0 } };
}

function canWalk(tile: string, direction: Direction): boolean {
    switch (tile) {
        case "|":
            return direction === Direction.Up || direction === Direction.Down;
        case "-":
            return direction === Direction.Left || direction === Direction.Right;
        case "L":
            return direction === Direction.Left || direction === Direction.Down;
        case "J":
            return direction === Direction.Right || direction === Direction.Down;
        case "7":
            return direction === Direction.Right || direction === Direction.Up;
        case "F":
            return direction === Direction.Left || direction === Direction.Up;
        case "S":
            return false;
        default:
            return false;
    }
}

function findEnd(grid: string[]): Point {
    for (let y = 0; y < grid.length; y++) {
        const row = grid[y];
        for (let x = 0; x < row.length; x++) {
            if (row[x] === "S") {
                return { x, y };
            }
        }
    }
    return { x: 0, y: 0 };
}

export function solve(input: string): [number, number] {
    const maze = parseMaze(input);

    return [part1(maze), part2(maze)];
}
