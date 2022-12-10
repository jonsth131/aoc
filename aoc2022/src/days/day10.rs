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
    result.push(x);

    result
}

fn part1(instructions: &Vec<Instruction>) -> usize {
    let state = get_state(instructions);
    let mut start = 20;
    let mut result = 0;

    while start < state.len() {
        result += state[start - 1] as usize * start;
        start += 40;
    }

    result
}

fn part2(instructions: &Vec<Instruction>) -> String {
    let state = get_state(instructions);
    let mut rows = Vec::new();
    rows.push("".to_string());
    for row in state.chunks(40) {
        if row.len() != 40 {
            break;
        }
        let mut ctr_line = Vec::new();
        for (cycle, pos) in row.iter().enumerate() {
            if *pos == cycle as i32 - 1 || *pos == cycle as i32 || *pos == cycle as i32 + 1 {
                ctr_line.push('#');
            } else {
                ctr_line.push('.');
            }
        }
        rows.push(ctr_line.iter().cloned().collect::<String>());
    }

    rows.join("\n")
}

fn parse_data(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .map(|line| {
            let r: Vec<&str> = line.split(' ').collect::<Vec<&str>>();
            match r.as_slice() {
                ["noop"] => Instruction::Noop(),
                ["addx", val] => Instruction::Addx(val.parse().unwrap()),
                _ => panic!("Invalid instruction"),
            }
        })
        .collect()
}

pub fn run(input: &str) -> (String, String) {
    let instructions = parse_data(input);
    (part1(&instructions).to_string(), part2(&instructions))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input: &str = include_str!("../../inputs/test_day10.txt");
        let instructions = parse_data(input);

        assert_eq!(part1(&instructions), 13140);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day10.txt");
        let expected: String = "\n##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######.....".to_string();
        let instructions = parse_data(input);

        assert_eq!(part2(&instructions), expected);
    }
}
