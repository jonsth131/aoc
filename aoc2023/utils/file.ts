export async function readInput(day: number, test: boolean = false): Promise<string> {
    const filePath = `./input/day${day}.${test ? "test" : "input"}.txt`;
    const file = Bun.file(filePath);
    return await file.text();
}
