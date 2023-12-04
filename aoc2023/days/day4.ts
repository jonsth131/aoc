type ScratchCard = {
    id: number;
    winningNumbers: number[];
    guessedNumbers: number[];
    numberOfMatches: number;
    copies: number;
};

function parseScratchCard(line: string): ScratchCard {
    const parts = line.split(": ");
    const [game, rest] = parts;
    const [winningNumbers, guessedNumbers] = rest.split(" | ");

    return {
        id: Number(game.slice(5)),
        winningNumbers: winningNumbers.match(/\d\d?/g)?.map(Number) ?? [],
        guessedNumbers: guessedNumbers.match(/\d\d?/g)?.map(Number) ?? [],
        numberOfMatches: 0,
        copies: 1,
    };
}

function matchScratchCard(card: ScratchCard): ScratchCard {
    const { winningNumbers, guessedNumbers } = card;
    const numberOfMatches = winningNumbers.filter((n) => guessedNumbers.includes(n)).length;
    return { ...card, numberOfMatches };
}

function calculateWinningAmount(card: ScratchCard): number {
    return card.numberOfMatches === 0 ? 0 : 1 << card.numberOfMatches - 1;
}

function calculateCopies(card: ScratchCard, all: ScratchCard[]): void {
    const { id, numberOfMatches, copies } = card;
    for (let copy = 0; copy < copies; copy++) {
        for (let i = 0; i < numberOfMatches; i++) {
            all[i + id].copies++;
        }
    }
}

function part1(input: string[]): number {
    return input
        .map(parseScratchCard)
        .map(matchScratchCard)
        .map(calculateWinningAmount)
        .reduce((sum, v) => sum + v, 0);
}

function part2(input: string[]): number {
    const cards = input.map(parseScratchCard).map(matchScratchCard);
    cards.forEach((card) => calculateCopies(card, cards));
    return cards.reduce((sum, card) => sum + card.copies, 0);
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
