use std::collections::HashSet;

#[derive(Debug, Clone, Copy)]
struct Move {
    direction: char,
    count: usize,
}

struct State {
    rope: Vec<(isize, isize)>,
    tail_visited: HashSet<(isize, isize)>,
}

impl State {
    fn new(rope_length: usize) -> Self {
        let mut rope = Vec::new();
        for _ in 0..rope_length {
            rope.push((0, 0));
        }
        let mut tail_visited = HashSet::new();
        tail_visited.insert((0, 0));
        State { rope, tail_visited }
    }

    fn process_move(&mut self, mv: Move) {
        match mv.direction {
            'R' => self.move_right(mv.count),
            'L' => self.move_left(mv.count),
            'U' => self.move_up(mv.count),
            'D' => self.move_down(mv.count),
            _ => panic!("Invalid move"),
        }
    }

    fn move_right(&mut self, count: usize) {
        for _ in 0..count {
            let (x, y) = self.rope[0];
            self.rope[0] = (x + 1, y);
            self.move_rope();
            self.tail_visited.insert(self.rope[self.rope.len() - 1]);
        }
    }

    fn move_left(&mut self, count: usize) {
        for _ in 0..count {
            let (x, y) = self.rope[0];
            self.rope[0] = (x - 1, y);
            self.move_rope();
            self.tail_visited.insert(self.rope[self.rope.len() - 1]);
        }
    }

    fn move_up(&mut self, count: usize) {
        for _ in 0..count {
            let (x, y) = self.rope[0];
            self.rope[0] = (x, y + 1);
            self.move_rope();
            self.tail_visited.insert(self.rope[self.rope.len() - 1]);
        }
    }

    fn move_down(&mut self, count: usize) {
        for _ in 0..count {
            let (x, y) = self.rope[0];
            self.rope[0] = (x, y - 1);
            self.move_rope();
            self.tail_visited.insert(self.rope[self.rope.len() - 1]);
        }
    }

    fn move_rope(&mut self) {
        for i in 1..self.rope.len() {
            self.move_part(i);
        }
    }

    fn move_part(&mut self, part: usize) {
        fn get_value(a: isize, b: isize) -> isize {
            if a == b {
                return 0;
            }
            if a > b {
                1
            } else {
                -1
            }
        }

        let prev = self.rope[part - 1];
        let curr = self.rope[part];

        if !self.parts_touching(prev, curr) {
            let x = get_value(prev.0, curr.0);
            let y = get_value(prev.1, curr.1);
            self.rope[part] = (curr.0 + x, curr.1 + y);
        }
    }

    fn parts_touching(&self, first: (isize, isize), second: (isize, isize)) -> bool {
        let (tail_x, tail_y) = second;

        first == second
            || first == (tail_x - 1, tail_y + 1)
            || first == (tail_x, tail_y + 1)
            || first == (tail_x + 1, tail_y + 1)
            || first == (tail_x - 1, tail_y)
            || first == (tail_x + 1, tail_y)
            || first == (tail_x - 1, tail_y - 1)
            || first == (tail_x, tail_y - 1)
            || first == (tail_x + 1, tail_y - 1)
    }
}

fn part1(moves: &Vec<Move>) -> usize {
    let mut state = State::new(2);
    for mv in moves {
        state.process_move(*mv);
    }
    state.tail_visited.len()
}

fn part2(moves: &Vec<Move>) -> usize {
    let mut state = State::new(10);
    for mv in moves {
        state.process_move(*mv);
    }
    state.tail_visited.len()
}

fn parse_data(input: &str) -> Vec<Move> {
    input
        .lines()
        .map(|line| {
            let mut r = line.split(' ');
            Move {
                direction: r.next().unwrap().chars().next().unwrap(),
                count: r.last().unwrap().parse::<usize>().unwrap(),
            }
        })
        .collect()
}

pub fn run(input: &str) -> (String, String) {
    let moves = parse_data(input);
    (part1(&moves).to_string(), part2(&moves).to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input: &str = include_str!("../../inputs/test_day9.txt");
        let moves = parse_data(input);

        assert_eq!(part1(&moves), 13);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day9.txt");
        let moves = parse_data(input);

        assert_eq!(part2(&moves), 1);
    }

    #[test]
    fn test_part_two_large() {
        let input: &str = include_str!("../../inputs/test_day9_large.txt");
        let moves = parse_data(input);

        assert_eq!(part2(&moves), 36);
    }

    #[test]
    fn test_move_right() {
        let mut state = State::new(2);
        state.move_right(4);

        assert_eq!(state.rope[0], (4, 0));
        assert_eq!(state.rope[1], (3, 0));
        assert_eq!(state.tail_visited.len(), 4);
    }

    #[test]
    fn test_process_move_right() {
        let mut state = State::new(2);
        let mv = Move {
            direction: 'R',
            count: 4,
        };
        state.process_move(mv);

        assert_eq!(state.rope[0], (4, 0));
        assert_eq!(state.rope[1], (3, 0));
        assert_eq!(state.tail_visited.len(), 4);
    }

    #[test]
    fn test_move_left() {
        let mut state = State::new(2);
        state.move_left(4);

        assert_eq!(state.rope[0], (-4, 0));
        assert_eq!(state.rope[1], (-3, 0));
        assert_eq!(state.tail_visited.len(), 4);
    }

    #[test]
    fn test_process_move_left() {
        let mut state = State::new(2);
        let mv = Move {
            direction: 'L',
            count: 4,
        };
        state.process_move(mv);

        assert_eq!(state.rope[0], (-4, 0));
        assert_eq!(state.rope[1], (-3, 0));
        assert_eq!(state.tail_visited.len(), 4);
    }

    #[test]
    fn test_move_up() {
        let mut state = State::new(2);
        state.move_up(4);

        assert_eq!(state.rope[0], (0, 4));
        assert_eq!(state.rope[1], (0, 3));
        assert_eq!(state.tail_visited.len(), 4);
    }

    #[test]
    fn test_process_move_up() {
        let mut state = State::new(2);
        let mv = Move {
            direction: 'U',
            count: 4,
        };
        state.process_move(mv);

        assert_eq!(state.rope[0], (0, 4));
        assert_eq!(state.rope[1], (0, 3));
        assert_eq!(state.tail_visited.len(), 4);
    }

    #[test]
    fn test_move_down() {
        let mut state = State::new(2);
        state.move_down(4);

        assert_eq!(state.rope[0], (0, -4));
        assert_eq!(state.rope[1], (0, -3));
        assert_eq!(state.tail_visited.len(), 4);
    }

    #[test]
    fn test_process_move_down() {
        let mut state = State::new(2);
        let mv = Move {
            direction: 'D',
            count: 4,
        };
        state.process_move(mv);

        assert_eq!(state.rope[0], (0, -4));
        assert_eq!(state.rope[1], (0, -3));
        assert_eq!(state.tail_visited.len(), 4);
    }

    #[test]
    fn test_process_move_rigth_up() {
        let mut state = State::new(2);
        let move_right = Move {
            direction: 'R',
            count: 4,
        };
        state.process_move(move_right);
        let move_up = Move {
            direction: 'U',
            count: 4,
        };
        state.process_move(move_up);

        assert_eq!(state.rope[0], (4, 4));
        assert_eq!(state.rope[1], (4, 3));
        assert_eq!(state.tail_visited.len(), 7);
    }

    #[test]
    fn test_move_right_rope_length_three() {
        let mut state = State::new(3);
        let move_right = Move {
            direction: 'R',
            count: 4,
        };
        state.process_move(move_right);

        assert_eq!(state.rope[0], (4, 0));
        assert_eq!(state.rope[1], (3, 0));
        assert_eq!(state.rope[2], (2, 0));
        assert_eq!(state.tail_visited.len(), 3);
    }
}
