use took::Timer;

struct Range(u32, u32);

fn part1(data: &Vec<(Range, Range)>) -> u32 {
    return data
        .iter()
        .map(|(range1, range2)| full_overlap(range1, range2) as u32)
        .sum::<u32>();
}

fn part2(data: &Vec<(Range, Range)>) -> u32 {
    return data
        .iter()
        .map(|(range1, range2)| overlap(range1, range2) as u32)
        .sum::<u32>();
}

fn overlap(r1: &Range, r2: &Range) -> bool {
    return r1.1 >= r2.0 && r1.0 <= r2.1;
}

fn full_overlap(r1: &Range, r2: &Range) -> bool {
    return (r1.0 <= r2.0 && r1.1 >= r2.1) || (r2.0 <= r1.0 && r2.1 >= r1.1);
}

fn parse_data(input: &str) -> Vec<(Range, Range)> {
    let data = input.split("\n").collect::<Vec<&str>>();
    return data[..data.len() - 1]
        .iter()
        .map(|row| {
            let mut row_split = row.split(",");
            let mut p1_split = row_split.next().unwrap().split("-");
            let mut p2_split = row_split.next().unwrap().split("-");
            return (
                Range(
                    p1_split.next().unwrap().parse::<u32>().unwrap(),
                    p1_split.next().unwrap().parse::<u32>().unwrap(),
                ),
                Range(
                    p2_split.next().unwrap().parse::<u32>().unwrap(),
                    p2_split.next().unwrap().parse::<u32>().unwrap(),
                ),
            );
        })
        .collect();
}

pub fn run(input: &str) {
    println!("==== DAY 4 ====");
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
