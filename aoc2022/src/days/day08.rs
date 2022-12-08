use std::collections::HashSet;

use took::Timer;

fn get_visible(grid: &Vec<Vec<u8>>, roots: Vec<(usize, usize)>) -> HashSet<(usize, usize)> {
    let mut found = HashSet::new();
    let col_length = grid[0].len();
    let row_length = grid.len();

    for (r, c) in roots {
        let curr = grid[r][c];
        found.insert((r, c));

        if c == 0 {
            let mut largest = 0;
            for i in 1..(col_length - 1) {
                if grid[r][i] > curr && grid[r][i] > largest {
                    found.insert((r, i));
                }
                largest = if largest > grid[r][i] {
                    largest
                } else {
                    grid[r][i]
                };
            }
        } else if c == col_length - 1 {
            let mut largest = 0;
            for i in (1..(col_length - 1)).rev() {
                if grid[r][i] > curr && grid[r][i] > largest {
                    found.insert((r, i));
                }
                largest = if largest > grid[r][i] {
                    largest
                } else {
                    grid[r][i]
                };
            }
        } else if r == 0 {
            let mut largest = 0;
            for i in 1..(row_length - 1) {
                if grid[i][c] > curr && grid[i][c] > largest {
                    found.insert((i, c));
                }
                largest = if largest > grid[i][c] {
                    largest
                } else {
                    grid[i][c]
                };
            }
        } else if r == row_length - 1 {
            let mut largest = 0;
            for i in (1..(row_length - 1)).rev() {
                if grid[i][c] > curr && grid[i][c] > largest {
                    found.insert((i, c));
                }
                largest = if largest > grid[i][c] {
                    largest
                } else {
                    grid[i][c]
                };
            }
        }
    }

    found
}

fn get_scenic_score(grid: &Vec<Vec<u8>>, (r, c): (usize, usize)) -> usize {
    let curr = grid[r][c];

    // Right
    let mut trees_to_the_right = 0;
    for i in (c + 1)..grid[0].len() {
        trees_to_the_right += 1;
        if grid[r][i] >= curr {
            break;
        }
    }

    // Left
    let mut trees_to_the_left = 0;
    for i in (0..c).rev() {
        trees_to_the_left += 1;
        if grid[r][i] >= curr {
            break;
        }
    }

    // Up
    let mut trees_up = 0;
    for i in (0..r).rev() {
        trees_up += 1;
        if grid[i][c] >= curr {
            break;
        }
    }

    // Down
    let mut trees_down = 0;
    for i in (r + 1)..grid.len() {
        trees_down += 1;
        if grid[i][c] >= curr {
            break;
        }
    }

    trees_to_the_right * trees_to_the_left * trees_up * trees_down
}

fn part1(grid: &Vec<Vec<u8>>) -> usize {
    let rows = grid.len();
    let columns = grid[0].len();
    let mut roots: Vec<(usize, usize)> = Vec::new();

    for i in 0..columns {
        roots.push((0, i));
        roots.push((rows - 1, i));
    }

    for i in 1..(rows - 1) {
        roots.push((i, 0));
        roots.push((i, columns - 1));
    }

    get_visible(grid, roots).len()
}

fn part2(grid: &Vec<Vec<u8>>) -> usize {
    let mut max_scenic_score = 0;
    for r in 0..grid.len() {
        for c in 0..grid[0].len() {
            let scenic_score = get_scenic_score(grid, (r, c));
            if scenic_score > max_scenic_score {
                max_scenic_score = scenic_score;
            }
        }
    }

    max_scenic_score
}

fn parse_data(input: &str) -> Vec<Vec<u8>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .collect::<Vec<u8>>()
        })
        .collect::<Vec<Vec<u8>>>()
}

pub fn run(input: &str) {
    println!("==== DAY 8 ====");
    let data = parse_data(input);

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
        let input: &str = include_str!("../../inputs/test_day8.txt");
        let data = parse_data(input);

        assert_eq!(part1(&data), 21);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day8.txt");
        let data = parse_data(input);

        assert_eq!(part2(&data), 8);
    }
}
