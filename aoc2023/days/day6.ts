type Race = {
    duration: number;
    record: number;
};

type Range = {
    start: number;
    end: number;
};

function part1(input: string[]): number {
    return parseRaces(input)
        .map(getWinRange)
        .map(calcNumberOfWays)
        .reduce((a, b) => a * b);
}

function part2(input: string[]): number {
    const race = parseRace(input);
    const range = getWinRange(race);
    return calcNumberOfWays(range);
}

function calcNumberOfWays(range: Range): number {
    return range.end - range.start + 1;
}

function getWinRange(race: Race): Range {
    const first = getFirstWin(race);
    const last = getLastWin(race);
    return { start: first, end: last };
}

function getFirstWin(race: Race): number {
    for (let i = race.duration; i > 0; i--) {
        const t = race.duration - i;
        const distance = i * t;
        if (distance > race.record) {
            return t;
        }
    }
    return 0;
}

function getLastWin(race: Race): number {
    for (let i = 1; i < race.duration; i++) {
        const t = race.duration - i;
        const distance = i * t;
        if (distance > race.record) {
            return t;
        }
    }
    return 0;
}

function parseRaces(input: string[]): Race[] {
    const timeRow = input[0].split(":")[1].match(/\d+/g)!.map(Number);
    const distanceRow = input[1].split(":")[1].match(/\d+/g)!.map(Number);
    const races = [];

    for (let i = 0; i < timeRow.length; i++) {
        races.push({
            duration: timeRow[i],
            record: distanceRow[i],
        });
    }

    return races;
}

function parseRace(input: string[]): Race {
    const time = input[0].split(":")[1].match(/\d+/g)!.join("");
    const distance = input[1].split(":")[1].match(/\d+/g)!.join("");

    return {
        duration: Number(time),
        record: Number(distance),
    };
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
