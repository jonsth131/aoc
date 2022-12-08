struct Range(u32, u32);

impl Range {
    pub fn overlap(&self, other: &Range) -> bool {
        self.1 >= other.0 && self.0 <= other.1
    }

    pub fn full_overlap(&self, other: &Range) -> bool {
        self.0 <= other.0 && self.1 >= other.1
    }
}

fn part1(data: &[(Range, Range)]) -> u32 {
    return data
        .iter()
        .map(|(range1, range2)| (range1.full_overlap(range2) || range2.full_overlap(range1)) as u32)
        .sum::<u32>();
}

fn part2(data: &[(Range, Range)]) -> u32 {
    return data
        .iter()
        .map(|(range1, range2)| range1.overlap(range2) as u32)
        .sum::<u32>();
}

fn parse_data(input: &str) -> Vec<(Range, Range)> {
    input
        .lines()
        .map(|row| {
            let mut row_split = row.split(',');
            let mut p1_split = row_split.next().unwrap().split('-');
            let mut p2_split = row_split.next().unwrap().split('-');
            (
                Range(
                    p1_split.next().unwrap().parse::<u32>().unwrap(),
                    p1_split.next().unwrap().parse::<u32>().unwrap(),
                ),
                Range(
                    p2_split.next().unwrap().parse::<u32>().unwrap(),
                    p2_split.next().unwrap().parse::<u32>().unwrap(),
                ),
            )
        })
        .collect()
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
        let input: &str = include_str!("../../inputs/test_day4.txt");

        let data = parse_data(input);
        assert_eq!(part1(&data), 2);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day4.txt");

        let data = parse_data(input);
        assert_eq!(part2(&data), 4);
    }
}
