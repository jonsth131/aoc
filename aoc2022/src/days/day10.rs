#[derive(Debug)]
enum Instruction {
    Noop(),
    Addx(i32),
}

fn get_state(instructions: &Vec<Instruction>) -> Vec<i32> {
    let mut result = Vec::new();
    let mut x = 1;

    for instruction in instructions {
        match instruction {
            Instruction::Noop() => result.push(x),
            Instruction::Addx(val) => {
                result.push(x);
                result.push(x);
                x += val;
            }
        }
    }

    result
}

fn part1(state: &Vec<i32>) -> usize {
    let mut start = 20;
    let mut result = 0;

    while start < state.len() {
        result += state[start - 1] as usize * start;
        start += 40;
    }

    result
}

fn part2(state: &[i32]) -> String {
    let mut result = String::new();

    for row in state.chunks(40) {
        result.push('\n');
        for (cycle, pos) in row.iter().enumerate() {
            if *pos == cycle as i32 - 1 || *pos == cycle as i32 || *pos == cycle as i32 + 1 {
                result.push('#');
            } else {
                result.push('.');
            }
        }
    }

    result
}

fn parse_data(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .map(
            |line| match line.split(' ').collect::<Vec<&str>>().as_slice() {
                ["noop"] => Instruction::Noop(),
                ["addx", val] => Instruction::Addx(val.parse().unwrap()),
                _ => panic!("Invalid instruction"),
            },
        )
        .collect()
}

pub fn run(input: &str) -> (String, String) {
    let instructions = parse_data(input);
    let state = get_state(&instructions);
    (part1(&state).to_string(), part2(&state))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input: &str = include_str!("../../inputs/test_day10.txt");
        let instructions = parse_data(input);
        let state = get_state(&instructions);

        assert_eq!(part1(&state), 13140);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day10.txt");
        let expected: String = "\n##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######.....".to_string();
        let instructions = parse_data(input);
        let state = get_state(&instructions);

        assert_eq!(part2(&state), expected);
    }
}
