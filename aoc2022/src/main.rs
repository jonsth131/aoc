use std::env;
use std::fs;
use std::io::{self, Read};
use took::Timer;

pub mod days;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let day: u8 = args
        .get(1)
        .expect("No day number provided")
        .parse()
        .expect("Day number must be an integer");

    let input_file = format!("inputs/day{:02}.txt", day);
    let mut input = String::new();

    fs::File::open(input_file)?.read_to_string(&mut input)?;

    let timer = Timer::new();
    let (part1, part2) = match day {
        1 => days::day01::run(&input),
        2 => days::day02::run(&input),
        3 => days::day03::run(&input),
        4 => days::day04::run(&input),
        5 => days::day05::run(&input),
        6 => days::day06::run(&input),
        7 => days::day07::run(&input),
        8 => days::day08::run(&input),
        9 => days::day09::run(&input),
        _ => panic!("Invalid day number"),
    };
    let elapsed = timer.took().into_std();

    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
    println!("Elapsed time: {:?}", elapsed);

    Ok(())
}
