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
        let input: &str = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000";

        let data = parse_data(input);
        assert_eq!(part1(&data), 24000);
    }

    #[test]
    fn test_part_two() {
        let input: &str = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000";

        let data = parse_data(input);
        assert_eq!(part2(&data), 45000);
    }
}
