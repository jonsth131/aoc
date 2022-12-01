use took::Timer;

fn part1(data: &mut Vec<u32>) -> u32 {
    data.pop().unwrap()
}

fn part2(data: &mut Vec<u32>) -> u32 {
    data.pop().unwrap() + data.pop().unwrap() + data.pop().unwrap()
}

fn parse_data(input: Vec<String>) -> Vec<u32> {
    let mut result: Vec<u32> = Vec::new();
    let mut acc = 0;
    for line in input {
        if line.is_empty() {
            result.push(acc);
            acc = 0;
        } else {
            acc += line.parse::<u32>().unwrap();
        }
    }
    result.push(acc);

    result.sort();
    result
}

pub fn run(input: Vec<String>) {
    let data = parse_data(input);

    let p1_timer = Timer::new();
    let p1 = part1(&mut data.clone());
    println!(
        "Part 1 answer: {}, time: {:?}",
        p1,
        p1_timer.took().into_std()
    );

    let p2_timer = Timer::new();
    let p2 = part2(&mut data.clone());
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
        let input: Vec<String> =
            "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
                .split("\n")
                .map(|v| v.to_owned())
                .collect();

        let data = parse_data(input);
        assert_eq!(part1(&mut data.clone()), 24000);
    }

    #[test]
    fn test_part_two() {
        let input: Vec<String> =
            "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
                .split("\n")
                .map(|v| v.to_owned())
                .collect();

        let data = parse_data(input);
        assert_eq!(part2(&mut data.clone()), 45000);
    }
}
