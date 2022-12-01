use std::{fs, io::{self, BufReader, BufRead}};

use took::Timer;

pub mod days;

fn read_lines(filename: String) -> io::Result<Vec<String>> {
    let file_in = fs::File::open(filename)?;
    let file_reader = BufReader::new(file_in);
    Ok(file_reader.lines().filter_map(io::Result::ok).collect())
}

fn main() {
    let full_timer = Timer::new();
    days::day01::run(read_lines("inputs/day1.txt".to_string()).unwrap());
    println!("Total: {:?}", full_timer.took().into_std());
}
