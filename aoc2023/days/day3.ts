type PartNumber = {
    x: number;
    y: number;
    len: number;
    partNumber: number;
};

type Symbol = {
    x: number;
    y: number;
    symbol: string;
};

function getPartNumbersFromLine(line: string, rowNumber: number): PartNumber[] {
    const partNumbers: PartNumber[] = [];
    const createPartNumber = (x: number, len: number, pn: number) => {
        partNumbers.push({ x, y: rowNumber, len, partNumber: pn });
    }

    let currPartNumber = "";
    for (let i = 0; i < line.length; i++) {
        const c = line[i];
        if (isDigit(c)) {
            currPartNumber += c;
        } else if (currPartNumber !== "") {
            createPartNumber(i - currPartNumber.length, currPartNumber.length, Number(currPartNumber));
            currPartNumber = "";
        }
    }
    if (currPartNumber !== "") {
        createPartNumber(line.length - currPartNumber.length, currPartNumber.length, Number(currPartNumber));
    }
    return partNumbers;
}

function isDigit(c: string): boolean {
    return isNaN(Number(c)) === false;
}

function getAdjacent(partNumber: PartNumber, schematic: string[], check: (c: string) => boolean): Symbol | null {
    const { x, y, len } = partNumber;
    for (let i = y - 1; i <= y + 1; i++) {
        for (let j = x - 1; j <= x + len; j++) {
            if (check(schematic[i]?.[j] ?? '.')) return { x: j, y: i, symbol: schematic[i][j] };
        }
    }
    return null;
}

function isSymbol(c: string): boolean {
    return isDigit(c) === false && c !== ".";
}

function part1(input: string[]): number {
    const partNumbers = input.map(getPartNumbersFromLine).flat();

    let sum = 0;
    for (const partNumber of partNumbers) {
        if (getAdjacent(partNumber, input, (c) => isSymbol(c)) !== null) {
            sum += partNumber.partNumber;
        }
    }

    return sum;
}

function part2(input: string[]): number {
    const partNumbers = input.map(getPartNumbersFromLine).flat();
    const foundSymbols: Map<string, PartNumber[]> = new Map();

    for (const partNumber of partNumbers) {
        const symbol = getAdjacent(partNumber, input, (c) => c === "*")
        if (symbol === null) continue;
        const symbolName = `${symbol.x},${symbol.y}`;
        const partNumbers = foundSymbols.get(symbolName) ?? [];
        partNumbers.push(partNumber);
        foundSymbols.set(symbolName, partNumbers);
    }

    let sum = 0;
    for (const [_, partNumbers] of foundSymbols) {
        if (partNumbers.length === 2) {
            sum += partNumbers[0].partNumber * partNumbers[1].partNumber;
        }
    }

    return sum;
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
