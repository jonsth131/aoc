enum Result {
    FiveOfAKind = 6,
    FourOfAKind = 5,
    FullHouse = 4,
    ThreeOfAKind = 3,
    TwoPairs = 2,
    OnePair = 1,
    HighCard = 0,
};

type Hand = {
    cards: string;
    result: Result;
    cardValues: number[];
    bid: number;
}

function part1(input: string[]): number {
    const cardValues = new Map<string, number>([
        ["2", 2], ["3", 3], ["4", 4], ["5", 5], ["6", 6], ["7", 7], ["8", 8],
        ["9", 9], ["T", 10], ["J", 11], ["Q", 12], ["K", 13], ["A", 14]
    ]);

    const hands = input.map((x) => parseHand(x, cardValues));
    quickSort(hands, 0, hands.length - 1);
    return calculateTotalWinnings(hands);
}

function part2(input: string[]): number {
    const cardValues = new Map<string, number>([
        ["2", 2], ["3", 3], ["4", 4], ["5", 5], ["6", 6], ["7", 7], ["8", 8],
        ["9", 9], ["T", 10], ["J", 1], ["Q", 12], ["K", 13], ["A", 14]
    ]);

    const hands = input.map((x) => parseHand(x, cardValues));
    quickSort(hands, 0, hands.length - 1);
    return calculateTotalWinnings(hands);
}

function calculateTotalWinnings(hands: Hand[]): number {
    let sum = 0;
    for (let i = 0; i < hands.length; i++) {
        sum += (hands[i].bid * (i + 1));
    }
    return sum;
}

function parseHand(input: string, cardValues: Map<string, number>): Hand {
    const parts = input.split(" ");
    const cards = parts[0];

    const values = [];
    for (const card of cards) {
        values.push(cardValues.get(card)!);
    }

    return {
        cards,
        result: getHandResult([...values]),
        cardValues: values,
        bid: Number(parts[1]),
    };
}

function getHandResult(cardValues: number[]): Result {
    const counts = new Map<number, number>();
    
    let jokers = 0;
    for (let i = 0; i < cardValues.length; i++) {
        if (cardValues[i] === 1) {
            jokers++;
        }
    }
    if (jokers === 5) {
        return Result.FiveOfAKind;
    }

    for (const value of cardValues) {
        if (value === 1) {
            continue;
        }
        counts.set(value, (counts.get(value) || 0) + 1);
    }
    const sortedCounts = [...counts.entries()].sort((a, b) => b[1] - a[1]);
    const [_, count] = sortedCounts[0];
    const newCount = count + jokers;
    if (newCount === 5) {
        return Result.FiveOfAKind;
    }
    if (newCount === 4) {
        return Result.FourOfAKind;
    }
    if (newCount === 3) {
        if (sortedCounts[1][1] === 2) {
            return Result.FullHouse;
        }
        return Result.ThreeOfAKind;
    }
    if (newCount === 2) {
        if (sortedCounts[1][1] === 2) {
            return Result.TwoPairs;
        }
        return Result.OnePair;
    }
    return Result.HighCard;
}

function isSmaller(a: Hand, b: Hand): boolean {
    if (a.result !== b.result) {
        return a.result < b.result;
    }
    for (let i = 0; i < a.cardValues.length; i++) {
        if (a.cardValues[i] !== b.cardValues[i]) {
            return a.cardValues[i] < b.cardValues[i];
        }
    }
    return false;
}

function partition(arr: Hand[], low: number, high: number): number {
    let pivot = arr[high];
   
    let i = low - 1;
   
    for (let j = low; j <= high - 1; j++) {
        if (isSmaller(arr[j], pivot)) {
            i++;
            [arr[i], arr[j]] = [arr[j], arr[i]];
        }
    }
   
    [arr[i + 1], arr[high]] = [arr[high], arr[i + 1]];
    return i + 1;
}
 
function quickSort(arr: Hand[], low: number, high: number) {
    if (low < high) {
        let pi = partition(arr, low, high);
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}

export function solve(input: string): [number, number] {
    const lines = input.trim().split("\n");

    return [part1(lines), part2(lines)];
}
