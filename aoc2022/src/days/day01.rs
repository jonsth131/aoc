use took::Timer;

fn part1(data: &Vec<usize>) -> usize {
    data[0]
}

fn part2(data: &Vec<usize>) -> usize {
    data[0..3].iter().sum()
}

fn parse_data(input: &str) -> Vec<usize> {
    let mut parsed = input
        .split("\n\n")
        .map(|block| {
            block
                .lines()
                .flat_map(|s| s.parse::<usize>())
                .sum::<usize>()
        })
        .collect::<Vec<usize>>();

    parsed.sort_by(|a, b| b.cmp(a));
    parsed
}

pub fn run(input: &str) {
    println!("==== DAY 1 ====");
    let data = parse_data(&input);

    let p1_timer = Timer::new();
    let p1 = part1(&data);
    println!(
        "Part 1 answer: {}, time: {:?}",
        p1,
        p1_timer.took().into_std()
    );

    let p2_timer = Timer::new();
    let p2 = part2(&data);
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
        let input: &str = include_str!("../../inputs/test_day1.txt");

        let data = parse_data(input);
        assert_eq!(part1(&data), 24000);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day1.txt");

        let data = parse_data(input);
        assert_eq!(part2(&data), 45000);
    }
}
