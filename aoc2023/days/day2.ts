type GameRecord = {
    id: number;
    cubes: Cubes[];
};

type Cubes = {
    red: number;
    green: number;
    blue: number;
};

function part1(input: string[]): number {
    const game: Cubes = { red: 12, green: 13, blue: 14 };
    const records = input.map(parse_game_record);

    return records
        .filter((record) => check_game(game, record))
        .reduce((sum, record) => sum + record.id, 0);
}

function part2(input: string[]): number {
    const records = input.map(parse_game_record);

    let sum = 0;
    for (const record of records) {
        const cubes: Cubes = { red: 0, green: 0, blue: 0 };
        for (const c of record.cubes) {
            cubes.red = Math.max(cubes.red, c.red);
            cubes.green = Math.max(cubes.green, c.green);
            cubes.blue = Math.max(cubes.blue, c.blue);
        }
        sum += cubes.red * cubes.green * cubes.blue;
    }

    return sum;
}

function parse_game_record(line: string): GameRecord {
    const parts = line.split(": ");
    const sets = parts[1].split("; ").map((game) => game.split(", "));

    return {
        id: Number(parts[0].slice(5)),
        cubes: sets.map(parse_cubes)
    };
}

function parse_cubes(set: string[]): Cubes {
    const cubes: Cubes = { red: 0, green: 0, blue: 0 };
    set.forEach((s) => {
        const [count, color] = s.split(" ");
        if (color === "red") {
            cubes.red = Number(count);
        } else if (color === "green") {
            cubes.green = Number(count);
        } else if (color === "blue") {
            cubes.blue = Number(count);
        }
    });

    return cubes;
}

function check_game(game: Cubes, record: GameRecord): boolean {
    for (const cubes of record.cubes) {
        if (cubes.red > game.red || cubes.green > game.green || cubes.blue > game.blue) {
            return false;
        }
    }
    return true;
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
