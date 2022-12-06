use took::Timer;

fn get_match(input: &str, len: usize) -> Option<usize> {
    for (i, window) in input
        .chars()
        .collect::<Vec<char>>()
        .windows(len)
        .enumerate()
    {
        let mut chars = std::collections::HashSet::new();
        for c in window.iter() {
            chars.insert(c);
            if chars.len() == len {
                return Some(i + len);
            }
        }
    }

    None
}

fn part1(input: &str) -> usize {
    get_match(input, 4).unwrap()
}

fn part2(input: &str) -> usize {
    get_match(input, 14).unwrap()
}

pub fn run(input: &str) {
    println!("==== DAY 6 ====");

    let p1_timer = Timer::new();
    let p1 = part1(input);
    println!(
        "Part 1 answer: {}, time: {:?}",
        p1,
        p1_timer.took().into_std()
    );

    let p2_timer = Timer::new();
    let p2 = part2(input);
    println!(
        "Part 2 answer: {}, time: {:?}",
        p2,
        p2_timer.took().into_std()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input: &str = include_str!("../../inputs/test_day6.txt");

        assert_eq!(part1(input), 7);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day6.txt");

        assert_eq!(part2(input), 19);
    }
}
