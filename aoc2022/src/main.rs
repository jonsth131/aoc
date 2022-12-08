use took::Timer;

pub mod days;

fn main() {
    let full_timer = Timer::new();
    days::day01::run(include_str!("../inputs/day1.txt"));
    days::day02::run(include_str!("../inputs/day2.txt"));
    days::day03::run(include_str!("../inputs/day3.txt"));
    days::day04::run(include_str!("../inputs/day4.txt"));
    days::day05::run(include_str!("../inputs/day5.txt"));
    days::day06::run(include_str!("../inputs/day6.txt"));
    days::day07::run(include_str!("../inputs/day7.txt"));
    days::day08::run(include_str!("../inputs/day8.txt"));
    println!("Total: {:?}", full_timer.took().into_std());
}
