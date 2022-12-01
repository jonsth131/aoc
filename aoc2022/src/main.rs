use took::Timer;

pub mod days;

fn main() {
    let full_timer = Timer::new();
    days::day01::run(include_str!("../inputs/day1.txt"));
    println!("Total: {:?}", full_timer.took().into_std());
}
