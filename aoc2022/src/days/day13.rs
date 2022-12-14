use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map, map_res},
    multi::separated_list0,
    sequence::delimited,
    IResult,
};
use std::cmp::Ordering;

#[derive(Debug, Eq, PartialEq, Clone)]
enum Signal {
    List(Vec<Signal>),
    Value(u32),
}

impl Ord for Signal {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Signal::Value(a), Signal::Value(b)) => a.cmp(b),
            (Signal::List(a), Signal::List(b)) => a.cmp(b),
            (Signal::List(a), Signal::Value(b)) => a.cmp(&vec![Signal::Value(*b)]),
            (Signal::Value(a), Signal::List(b)) => vec![Signal::Value(*a)].cmp(b),
        }
    }
}

impl PartialOrd for Signal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
struct SignalPairs {
    left: Signal,
    right: Signal,
}

fn part1(data: &[SignalPairs]) -> usize {
    let mut result = 0;

    for (i, pair) in data.iter().enumerate() {
        if pair.right.cmp(&pair.left) == Ordering::Less {
            result += i + 1;
        }
    }

    result
}

fn part2(data: &Vec<SignalPairs>) -> usize {
    let mut packet_idx_2 = 0;
    let mut packet_idx_6 = 0;
    let packet_2 = Signal::List(vec![Signal::List(vec![Signal::Value(2)])]);
    let packet_6 = Signal::List(vec![Signal::List(vec![Signal::Value(6)])]);

    let mut list = vec![packet_2.clone(), packet_6.clone()];

    for pair in data {
        list.push(pair.left.clone());
        list.push(pair.right.clone());
    }

    list.sort();

    for (i, item) in list.iter().enumerate() {
        if item.cmp(&packet_2) == Ordering::Equal {
            packet_idx_2 = i + 1;
        } else if item.cmp(&packet_6) == Ordering::Equal {
            packet_idx_6 = i + 1;
        }

        if packet_idx_2 != 0 && packet_idx_6 != 0 {
            break;
        }
    }

    packet_idx_2 * packet_idx_6
}

fn parse_data(input: &str) -> Vec<SignalPairs> {
    input
        .split("\n\n")
        .map(|pair| {
            let mut signals = pair
                .lines()
                .map(|s| parse_signal(s).unwrap().1)
                .collect::<Vec<Signal>>();
            SignalPairs {
                left: signals.pop().unwrap(),
                right: signals.pop().unwrap(),
            }
        })
        .collect::<Vec<SignalPairs>>()
}

fn parse_signal(input: &str) -> IResult<&str, Signal> {
    alt((
        map(|i| map_res(digit1, str::parse)(i), Signal::Value),
        map(
            |i| delimited(tag("["), separated_list0(tag(","), parse_signal), tag("]"))(i),
            Signal::List,
        ),
    ))(input)
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
        let input: &str = include_str!("../../inputs/test_day13.txt");
        let data = parse_data(input);

        assert_eq!(part1(&data), 13);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day13.txt");
        let data = parse_data(input);

        assert_eq!(part2(&data), 140);
    }

    #[test]
    fn parse_data_works() {
        let input: &str = include_str!("../../inputs/test_day13.txt");
        let actual = parse_data(input);

        assert_eq!(actual.len(), 8);
    }

    #[test]
    fn test_parse_empty_array() {
        let actual = parse_signal("[]").unwrap().1;
        let expected = Signal::List(Vec::new());

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_array_with_one_value() {
        let actual = parse_signal("[1]").unwrap().1;
        let expected = Signal::List(vec![Signal::Value(1)]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_array_with_multiple_value() {
        let actual = parse_signal("[1,2,3]").unwrap().1;
        let expected = Signal::List(vec![Signal::Value(1), Signal::Value(2), Signal::Value(3)]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_array_with_array() {
        let actual = parse_signal("[[]]").unwrap().1;
        let expected = Signal::List(vec![Signal::List(Vec::new())]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_array_with_multiple_arrays() {
        let actual = parse_signal("[[],[]]").unwrap().1;
        let expected = Signal::List(vec![Signal::List(Vec::new()), Signal::List(Vec::new())]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_array_mixed() {
        let actual = parse_signal("[1,[2,[3]],4]").unwrap().1;
        let expected = Signal::List(vec![
            Signal::Value(1),
            Signal::List(vec![Signal::Value(2), Signal::List(vec![Signal::Value(3)])]),
            Signal::Value(4),
        ]);

        assert_eq!(actual, expected);
    }
    /*
        #[test]
        fn test_parse_signal_only_values() {
            let actual = parse_signal("[1,1,3,1,1]".chars().collect::<VecDeque<char>>());
            let list = vec![
                Signal::Value(1),
                Signal::Value(1),
                Signal::Value(3),
                Signal::Value(1),
                Signal::Value(1),
            ];
            let expected = Signal::List(list);

            assert_eq!(actual, expected);
        }

        #[test]
        fn test_parse_signal_mixed_values() {
            let actual = parse_signal("[[1],[2,3,4]]".chars().collect::<VecDeque<char>>());
            let l1 = vec![Signal::Value(1)];
            let l2 = vec![Signal::Value(2), Signal::Value(3), Signal::Value(4)];
            let expected = Signal::List(vec![Signal::List(l1), Signal::List(l2)]);

            assert_eq!(actual, expected);
        }
    */
}
