use std::collections::HashSet;

#[derive(Debug, Clone)]
struct State {
    grid: HashSet<(usize, usize)>,
    sand: usize,
    bottom_rock: usize,
}

impl State {
    fn drop_sand_pt1(&mut self) {
        let mut sand = (500, 0);

        loop {
            if sand.1 > self.bottom_rock {
                break;
            } else if !self.grid.contains(&(sand.0, sand.1 + 1)) {
                sand = (sand.0, sand.1 + 1);
            } else if !self.grid.contains(&(sand.0 - 1, sand.1 + 1)) {
                sand = (sand.0 - 1, sand.1 + 1);
            } else if !self.grid.contains(&(sand.0 + 1, sand.1 + 1)) {
                sand = (sand.0 + 1, sand.1 + 1);
            } else {
                self.grid.insert(sand);
                self.sand += 1;
                sand = (500, 0);
            }
        }
    }

    fn drop_sand_pt2(&mut self) {
        let mut sand = (500, 0);

        loop {
            if self.grid.contains(&(500, 0)) {
                break;
            }
            if sand.1 > self.bottom_rock {
                self.grid.insert(sand);
                self.sand += 1;
                sand = (500, 0);
            } else if !self.grid.contains(&(sand.0, sand.1 + 1)) {
                sand = (sand.0, sand.1 + 1);
            } else if !self.grid.contains(&(sand.0 - 1, sand.1 + 1)) {
                sand = (sand.0 - 1, sand.1 + 1);
            } else if !self.grid.contains(&(sand.0 + 1, sand.1 + 1)) {
                sand = (sand.0 + 1, sand.1 + 1);
            } else {
                self.grid.insert(sand);
                self.sand += 1;
                sand = (500, 0);
            }
        }
    }
}

fn part1(mut state: State) -> usize {
    state.drop_sand_pt1();
    state.sand
}

fn part2(mut state: State) -> usize {
    state.drop_sand_pt2();
    state.sand
}

fn parse_data(input: &str) -> State {
    let mut bottom_rock = 0;
    let mut grid = Vec::new();

    for line in input.lines() {
        let split = line
            .split(" -> ")
            .map(parse_point)
            .collect::<Vec<(usize, usize)>>();

        for pair in split.windows(2) {
            if pair[0].1 > bottom_rock {
                bottom_rock = pair[0].1;
            }
            if pair[1].1 > bottom_rock {
                bottom_rock = pair[1].1;
            }

            let mut points = generate_points(pair[0], pair[1]);
            grid.append(&mut points);
        }
    }

    State {
        grid: HashSet::from_iter(grid),
        sand: 0,
        bottom_rock,
    }
}

fn generate_points(
    (p1_x, p1_y): (usize, usize),
    (p2_x, p2_y): (usize, usize),
) -> Vec<(usize, usize)> {
    let mut result = Vec::new();

    if p1_x != p2_x {
        if p1_x > p2_x {
            for i in p2_x..p1_x + 1 {
                result.push((i, p1_y));
            }
        } else {
            for i in p1_x..p2_x + 1 {
                result.push((i, p1_y))
            }
        }
    } else if p1_y != p2_y {
        if p1_y > p2_y {
            for i in p2_y..p1_y + 1 {
                result.push((p1_x, i));
            }
        } else {
            for i in p1_y..p2_y + 1 {
                result.push((p1_x, i))
            }
        }
    }

    result
}

fn parse_point(input: &str) -> (usize, usize) {
    let split = input.split(',').collect::<Vec<&str>>();
    (
        split[0].parse::<usize>().unwrap(),
        split[1].parse::<usize>().unwrap(),
    )
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
        let input: &str = include_str!("../../inputs/test_day14.txt");
        let state = parse_data(input);

        assert_eq!(part1(state), 24);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day14.txt");
        let state = parse_data(input);

        assert_eq!(part2(state), 93);
    }

    #[test]
    fn parse_data_works() {
        let input: &str = include_str!("../../inputs/test_day14.txt");
        let state = parse_data(input);

        assert_eq!(state.grid.len(), 20);
        assert_eq!(state.bottom_rock, 9);
    }
}
