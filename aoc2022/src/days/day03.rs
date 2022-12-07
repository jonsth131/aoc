use took::Timer;

fn part1(data: &[&str]) -> u32 {
    data.iter()
        .map(|row| get_matching_char(row))
        .map(calc_priority)
        .sum::<u32>()
}

fn part2(data: &[&str]) -> u32 {
    data.chunks(3)
        .map(|chunk| {
            let p1_matches = get_matching_chars(chunk[0], chunk[1]);
            get_matching_chars(&p1_matches, chunk[2])
                .chars()
                .next()
                .unwrap() as u8
        })
        .map(calc_priority)
        .sum::<u32>()
}

fn get_matching_char(row: &str) -> u8 {
    let half = row.len() / 2;
    let (c1, c2) = (&row[..half], &row[half..]);
    let matches = get_matching_chars(c1, c2);
    matches.chars().next().unwrap() as u8
}

fn get_matching_chars(s1: &str, s2: &str) -> String {
    let mut chars: Vec<char> = vec![];

    for c in s1.chars() {
        if s2.contains(c) {
            chars.push(c);
        }
    }

    chars.iter().cloned().collect::<String>()
}

fn calc_priority(value: u8) -> u32 {
    if value > 96 {
        (value - 96) as u32
    } else {
        (value - 38) as u32
    }
}

fn parse_data(input: &str) -> Vec<&str> {
    input.lines().collect::<Vec<&str>>()
}

pub fn run(input: &str) {
    println!("==== DAY 3 ====");
    let data = parse_data(input);

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
        let input: &str = include_str!("../../inputs/test_day3.txt");

        let data = parse_data(input);
        assert_eq!(part1(&data), 157);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day3.txt");

        let data = parse_data(input);
        assert_eq!(part2(&data), 70);
    }
}
