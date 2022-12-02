use took::Timer;

fn part1(data: &Vec<(u8, u8)>) -> u32 {
    return data
        .iter()
        .map(|(opponent, my)| calc_points(*opponent, *my) as u32)
        .sum::<u32>();
}

fn part2(data: &Vec<(u8, u8)>) -> u32 {
    return data
        .iter()
        .map(|(opponent, result)| calc_points(*opponent, get_my_value(*opponent, *result)) as u32)
        .sum::<u32>();
}

fn parse_data(input: &str) -> Vec<(u8, u8)> {
    let data = input.split("\n");
    let mut result: Vec<(u8, u8)> = vec![];
    for line in data {
        if line == "" {
            continue;
        }
        let mut chars = line.chars();
        let opponent = chars.next().unwrap() as u8 - 64;
        let my = chars.nth(1).unwrap() as u8 - 87;
        result.push((opponent, my));
    }

    result
}

fn calc_points(opponent: u8, my: u8) -> u8 {
    if my == get_win_value(opponent) {
        return my + 6;
    }
    if my == get_loss_value(opponent) {
        return my;
    }
    return my + 3;
}

fn get_win_value(value: u8) -> u8 {
    let win = value + 1;
    return if win == 4 { 1 } else { win };
}

fn get_loss_value(value: u8) -> u8 {
    let loss = value - 1;
    return if loss == 0 { 3 } else { loss };
}

fn get_my_value(opponent: u8, result: u8) -> u8 {
    return match result {
        1 => get_loss_value(opponent),
        2 => opponent,
        3 => get_win_value(opponent),
        _ => panic!("Got invalid value!"),
    };
}

pub fn run(input: &str) {
    println!("==== DAY 2 ====");
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
        let input: &str = include_str!("../../inputs/test_day2.txt");
        let data = parse_data(input);

        assert_eq!(part1(&data), 15);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day2.txt");
        let data = parse_data(input);

        assert_eq!(part2(&data), 12);
    }
}
