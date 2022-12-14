use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
struct State {
    grid: Vec<Vec<char>>,
    position: (usize, usize),
    end_position: (usize, usize),
}

#[derive(Debug)]
struct Move {
    position: (usize, usize),
    number_of_moves: usize,
}

impl State {
    fn get_possible_moves(&self, (x, y): (usize, usize)) -> Vec<(usize, usize)> {
        let mut result = Vec::new();
        let curr = self.grid[x][y];

        // Check up
        if x != 0
            && (self.grid[x - 1][y] as u8 == curr as u8 + 1
                || self.grid[x - 1][y] as u8 <= curr as u8)
        {
            result.push((x - 1, y));
        }
        // Check down
        if x + 1 < self.grid.len()
            && (self.grid[x + 1][y] as u8 == curr as u8 + 1
                || self.grid[x + 1][y] as u8 <= curr as u8)
        {
            result.push((x + 1, y));
        }
        // Check left
        if y != 0
            && (self.grid[x][y - 1] as u8 == curr as u8 + 1
                || self.grid[x][y - 1] as u8 <= curr as u8)
        {
            result.push((x, y - 1));
        }
        // Check right
        if y + 1 < self.grid[x].len()
            && (self.grid[x][y + 1] as u8 == curr as u8 + 1
                || self.grid[x][y + 1] as u8 <= curr as u8)
        {
            result.push((x, y + 1));
        }
        result
    }

    fn get_lowest_positions(&self) -> Vec<(usize, usize)> {
        let mut result = Vec::new();
        for r in 0..self.grid.len() {
            for c in 0..self.grid.len() {
                if self.grid[r][c] == 'a' {
                    result.push((r, c));
                }
            }
        }

        result
    }
}

fn find_path(state: State, mut pos: VecDeque<Move>) -> usize {
    let mut main: HashSet<(usize, usize)> = HashSet::new();
    let mut result = 0;

    while !pos.is_empty() {
        let item = pos.pop_front().unwrap();

        if item.position == state.end_position {
            result = item.number_of_moves;
            break;
        }

        let possible_moves = state.get_possible_moves(item.position);

        for possible_move in possible_moves {
            if main.insert(possible_move) {
                pos.push_back(Move {
                    position: possible_move,
                    number_of_moves: item.number_of_moves + 1,
                });
            }
        }
    }

    result
}

fn part1(state: State) -> usize {
    let queue = VecDeque::from([Move {
        position: state.position,
        number_of_moves: 0,
    }]);
    find_path(state, queue)
}

fn part2(state: State) -> usize {
    let positions = state
        .get_lowest_positions()
        .iter()
        .map(|p| Move {
            position: p.to_owned(),
            number_of_moves: 0,
        })
        .collect::<Vec<Move>>();
    let queue = VecDeque::from(positions);
    find_path(state, queue)
}

fn parse_data(input: &str) -> State {
    let mut grid = Vec::new();
    let mut start_pos = (0, 0);
    let mut end_pos = (0, 0);
    for (row, line) in input.lines().enumerate() {
        let mut curr_row = Vec::new();
        for (col, c) in line.chars().enumerate() {
            if c == 'S' {
                start_pos = (row, col);
                curr_row.push('a');
                continue;
            } else if c == 'E' {
                end_pos = (row, col);
                curr_row.push('z');
                continue;
            }
            curr_row.push(c);
        }
        grid.push(curr_row);
    }
    State {
        grid,
        position: start_pos,
        end_position: end_pos,
    }
}

pub fn run(input: &str) -> (String, String) {
    let state = parse_data(input);
    (part1(state.clone()).to_string(), part2(state).to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input: &str = include_str!("../../inputs/test_day12.txt");
        let state = parse_data(input);
        assert_eq!(part1(state), 31);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day12.txt");
        let state = parse_data(input);
        assert_eq!(part2(state), 29);
    }
}
