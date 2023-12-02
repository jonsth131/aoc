type GameRecord = {
    id: number;
    cubes: Map<Colors, number>[];
};

type Cubes = {
    red: number;
    green: number;
    blue: number;
};

type Colors = "red" | "green" | "blue";

function part1(input: string[]): number {
    const game = new Map<Colors, number>([["red", 12], ["green", 13], ["blue", 14]]);
    const records = input.map(parse_game_record);

    const sum = records
        .filter((record) => check_game(game, record))
        .reduce((sum, record) => sum + record.id, 0);

    return sum;
}

function part2(input: string[]): number {
    const records = input.map(parse_game_record);

    let sum = 0;
    for (const record of records) {
        const cubes: Cubes = { red: 0, green: 0, blue: 0 };
        for (const c of record.cubes) {
            cubes.red = Math.max(cubes.red, c.get("red") ?? 0);
            cubes.green = Math.max(cubes.green, c.get("green") ?? 0);
            cubes.blue = Math.max(cubes.blue, c.get("blue") ?? 0);
        }
        sum += cubes.red * cubes.green * cubes.blue;
    }

    return sum;
}

function parse_game_record(line: string): GameRecord {
    const parts = line.split(": ");
    const sets = parts[1].split("; ").map((game) => game.split(", "));
    const cubes = sets.map(parse_cubes);

    return {
        id: Number(parts[0].slice(5)),
        cubes: cubes
    };
}

function parse_cubes(set: string[]): Map<Colors, number> {
    const cubes = new Map<Colors, number>();
    set.forEach((s) => {
        const [count, color] = s.split(" ");
        cubes.set(color as Colors, Number(count));
    });

    return cubes;
}

function check_game(game: Map<Colors, number>, record: GameRecord): boolean {
    for (const [color, count] of game.entries()) {
        for (const cubes of record.cubes) {
            const c = cubes.get(color);
            if (c === undefined) {
                continue;
            }
            if (c > count) {
                return false;
            }
        }
    }

    return true;
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
