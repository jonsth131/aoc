import { readInput } from "./utils/file";

async function runSolution(day: number): Promise<void> {
    try {
        const solution = await import(`./days/day${day}`);
        const input = await readInput(day);

        console.time(`Day ${day}`);
        const result = solution.solve(input.trim());
        console.timeEnd(`Day ${day}`);

        console.log(`Day ${day}: ${result}`);
    } catch (e: any) {
        console.error(`Error running day ${day}: ${e.message}`);
    }
};

const day = process.argv[2];
if (day) {
    await runSolution(parseInt(day));
} else {
    console.log("Running all days");
    for (let i = 1; i <= 25; i++) {
        await runSolution(i);
    }
}
