fn part1(data: &[usize]) -> usize {
    data[0]
}

fn part2(data: &[usize]) -> usize {
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

pub fn run(input: &str) -> (String, String) {
    let data = parse_data(input);
    (part1(&data).to_string(), part2(&data).to_string())
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
