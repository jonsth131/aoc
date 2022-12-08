use std::collections::{HashMap, HashSet};
use took::Timer;

#[derive(Debug)]
struct FileSystem {
    dirs: HashSet<String>,
    files: HashMap<String, usize>,
}

impl FileSystem {
    fn new() -> Self {
        FileSystem {
            dirs: HashSet::new(),
            files: HashMap::new(),
        }
    }

    fn add_file(&mut self, path: &str, file_name: &str, size: usize) {
        self.files.insert(path.to_owned() + file_name, size);
    }

    fn add_dir(&mut self, path: &str) {
        self.dirs.insert(path.to_owned());
    }

    fn total_dir_size(&self, dir_name: &str) -> usize {
        let mut total_size = 0;
        for (path, size) in &self.files {
            if path.starts_with(dir_name) {
                total_size += size;
            }
        }

        total_size
    }

    fn get_total_sizes_with_at_most(&self, size: usize) -> usize {
        let mut total_size = 0;
        for dir in &self.dirs {
            let dir_size = self.total_dir_size(dir);
            if dir_size <= size {
                total_size += dir_size;
            }
        }

        total_size
    }

    fn get_total_sizes_with_at_least(&self, size: usize) -> Vec<usize> {
        let mut sizes = Vec::new();
        for dir in &self.dirs {
            let dir_size = self.total_dir_size(dir);
            if dir_size >= size {
                sizes.push(dir_size);
            }
        }

        sizes
    }

    fn get_size_to_delete(&self, total_disk_size: usize, needed_free_space: usize) -> usize {
        let current_unused_space = total_disk_size - self.total_dir_size("/");
        let mut sizes =
            self.get_total_sizes_with_at_least(needed_free_space - current_unused_space);
        sizes.sort();
        sizes[0]
    }
}

fn part1(fs: &FileSystem) -> usize {
    fs.get_total_sizes_with_at_most(100000)
}

fn part2(fs: &FileSystem) -> usize {
    fs.get_size_to_delete(70000000, 30000000)
}

fn parse_data(input: &str) -> FileSystem {
    let mut fs = FileSystem::new();
    let mut cwd = vec![];
    let mut listing: bool = false;

    for line in input.lines() {
        if line.starts_with('$') {
            listing = false;
        }

        if listing {
            let mut list_entry = line.split(' ');
            let first = list_entry.next().unwrap();
            let path = cwd.join("/");
            if first != "dir" {
                let size = first.parse::<usize>().unwrap();
                fs.add_file(&path, list_entry.next().unwrap(), size)
            }
        }

        if line.starts_with("$ cd") {
            let dir = line.split(' ').last().unwrap();
            if dir == ".." {
                cwd.pop();
            } else {
                cwd.push(dir);
            }

            fs.add_dir(&cwd.join("/"));
        } else if line.starts_with("$ ls") {
            listing = true
        }
    }

    fs
}

pub fn run(input: &str) {
    println!("==== DAY 7 ====");
    let fs = parse_data(input);

    let p1_timer = Timer::new();
    let p1 = part1(&fs);
    println!(
        "Part 1 answer: {}, time: {:?}",
        p1,
        p1_timer.took().into_std()
    );

    let p2_timer = Timer::new();
    let p2 = part2(&fs);
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
        let input: &str = include_str!("../../inputs/test_day7.txt");
        let fs = parse_data(input);

        assert_eq!(part1(&fs), 95437);
    }

    #[test]
    fn test_part_two() {
        let input: &str = include_str!("../../inputs/test_day7.txt");
        let fs = parse_data(input);

        assert_eq!(part2(&fs), 24933642);
    }
}
