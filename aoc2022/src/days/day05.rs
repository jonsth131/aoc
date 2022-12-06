use took::Timer;

#[derive(Debug, Clone)]
struct State {
    state: Vec<Vec<char>>,
}

#[derive(Debug, Clone)]
struct Move {
    amount: u8,
    from: u8,
    to: u8,
}

impl State {
    pub fn add_stacks(&mut self, count: u8) {
        for _ in 0..count {
            self.state.push(vec![]);
        }
    }

    pub fn add_value(&mut self, stack: usize, val: char) {
        self.state[stack].push(val);
    }

    pub fn process_moves_p1(&mut self, moves: Vec<Move>) {
        for mv in moves {
            for _ in 0..mv.amount {
                let val = self.state[(mv.from - 1) as usize].pop().unwrap();
                self.state[(mv.to - 1) as usize].push(val);
            }
        }
    }

    pub fn process_moves_p2(&mut self, moves: Vec<Move>) {
        for mv in moves {
            let mut tmp = vec![];
            for _ in 0..mv.amount {
                let val = self.state[(mv.from - 1) as usize].pop().unwrap();
                tmp.insert(0, val);
            }

            for val in tmp {
                self.state[(mv.to - 1) as usize].push(val);
            }
        }
    }

    pub fn get_top(&mut self) -> String {
        self.state
            .clone()
            .iter()
            .map(|stack| stack.to_owned().pop().unwrap())
            .collect::<String>()
    }
}

fn part1(mut state: State, moves: Vec<Move>) -> String {
    state.process_moves_p1(moves);
    state.get_top()
}

fn part2(mut state: State, moves: Vec<Move>) -> String {
    state.process_moves_p2(moves);
    state.get_top()
}

fn parse_data(input: &str) -> (State, Vec<Move>) {
    let mut parts = input.split("\n\n");
    let mut starting_stacks: Vec<&str> = parts.next().unwrap().lines().collect::<Vec<&str>>();
    let move_lines = parts.next().unwrap().lines().collect::<Vec<&str>>();
    let moves = move_lines
        .iter()
        .map(|row| {
            let split = row.split(" ").collect::<Vec<&str>>();
            return Move {
                amount: split[1].parse::<u8>().unwrap(),
                from: split[3].parse::<u8>().unwrap(),
                to: split[5].parse::<u8>().unwrap(),
            };
        })
        .collect::<Vec<Move>>();

    let mut state = State { state: vec![] };
    let stacks = (starting_stacks.pop().unwrap().len() as f32 / 4.0).ceil() as u8;
    state.add_stacks(stacks);
    starting_stacks.reverse();

    for line in starting_stacks {
        for (i, val) in line
            .chars()
            .collect::<Vec<char>>()
            .chunks(4)
            .map(|c| c[1])
            .enumerate()
        {
            if val == ' ' {
                continue;
            }
            state.add_value(i, val);
        }
    }

    (state, moves.clone())
}

pub fn run(input: &str) {
    println!("==== DAY 5 ====");
    let (state, moves) = parse_data(&input);

    let p1_timer = Timer::new();
    let p1 = part1(state.clone(), moves.clone());
    println!(
        "Part 1 answer: {}, time: {:?}",
        p1,
        p1_timer.took().into_std()
    );

    let p2_timer = Timer::new();
    let p2 = part2(state.clone(), moves.clone());
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
        let input: &str = include_str!("../../inputs/test_day5.txt");

        let (state, moves) = parse_data(input);
        assert_eq!(part1(state, moves), "CMZ");
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day5.txt");

        let (state, moves) = parse_data(input);
        assert_eq!(part2(state, moves), "MCD");
    }
}
