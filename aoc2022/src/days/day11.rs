use std::{cell::RefCell, rc::Rc};

type MonkeyRef = Rc<RefCell<Monkey>>;

#[derive(Debug, Clone)]
struct State {
    monkeys: Vec<MonkeyRef>,
}

impl State {
    fn process(&mut self, divide: bool) {
        let mut mod_val: u128 = 1;
        for monkey in &self.monkeys {
            mod_val *= monkey.borrow().test as u128;
        }
        for monkey in &self.monkeys {
            let thrown = monkey.borrow_mut().throw_items_to(divide, mod_val);
            for (worry_value, monkey) in thrown {
                let monkey = &mut self.monkeys[monkey].borrow_mut();
                monkey.items.push(worry_value);
            }
        }
    }

    fn get_top_two(&self) -> u128 {
        let mut result = Vec::new();

        for monkey in &self.monkeys {
            result.push(monkey.borrow().times_inspected);
        }

        result.sort_by(|a, b| b.cmp(a));
        result[0] * result[1]
    }
}

#[derive(Debug, Clone)]
struct Monkey {
    items: Vec<u128>,
    operation: String,
    test: u32,
    monkey_true: usize,
    monkey_false: usize,
    times_inspected: u128,
}

impl Monkey {
    fn throw_items_to(&mut self, divide: bool, mod_val: u128) -> Vec<(u128, usize)> {
        fn execute_operation(old: u128, expression: &str) -> u128 {
            let split = expression.split(' ').collect::<Vec<&str>>();
            let operator = split[3];
            let last = split[4];
            let value = match last {
                "old" => old,
                _ => last.parse::<u128>().unwrap(),
            };

            match operator {
                "*" => old * value,
                "+" => old + value,
                _ => panic!("Invalid operator!"),
            }
        }

        let mut result = Vec::new();
        while !self.items.is_empty() {
            let item = self.items.pop().unwrap();
            let mut new_worry_value = execute_operation(item, &self.operation);

            if divide {
                new_worry_value /= 3;
            }
            new_worry_value %= mod_val;

            if new_worry_value % (self.test as u128) == 0 {
                result.push((new_worry_value, self.monkey_true));
            } else {
                result.push((new_worry_value, self.monkey_false));
            }
            self.times_inspected += 1;
        }

        result
    }
}

fn part1(input: &str) -> u128 {
    let mut state = parse_data(input);
    for _ in 0..20 {
        state.process(true);
    }
    state.get_top_two()
}

fn part2(input: &str) -> u128 {
    let mut state = parse_data(input);
    for _ in 0..10000 {
        state.process(false);
    }
    state.get_top_two()
}

fn parse_data(input: &str) -> State {
    let monkeys = input.split("\n\n").map(parse_monkey).collect();

    State { monkeys }
}

fn parse_monkey(input: &str) -> MonkeyRef {
    let mut items = Vec::new();
    let mut operation = String::new();
    let mut test = 0;
    let mut monkey_true = 0;
    let mut monkey_false = 0;

    for line in input.lines() {
        let mut split = line.trim().split(':');
        let first = split.next().unwrap();
        let data = split.next().unwrap().trim();
        if first.starts_with("Monkey") {
            continue;
        }
        match first {
            "Starting items" => {
                for item in data.split(", ") {
                    items.push(item.parse::<u128>().unwrap());
                }
            }
            "Operation" => operation = data.to_string(),
            "Test" => test = data.split(' ').last().unwrap().parse::<u32>().unwrap(),
            "If true" => monkey_true = data.split(' ').last().unwrap().parse::<usize>().unwrap(),
            "If false" => monkey_false = data.split(' ').last().unwrap().parse::<usize>().unwrap(),
            _ => panic!("Invalid data"),
        }
    }

    let monkey = Monkey {
        items,
        operation,
        test,
        monkey_true,
        monkey_false,
        times_inspected: 0,
    };

    Rc::new(RefCell::new(monkey))
}

pub fn run(input: &str) -> (String, String) {
    (part1(input).to_string(), part2(input).to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input: &str = include_str!("../../inputs/test_day11.txt");

        assert_eq!(part1(input), 10605);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day11.txt");

        assert_eq!(part2(input), 2713310158);
    }
}
