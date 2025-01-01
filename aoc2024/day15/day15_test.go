package day15

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	input := util.ReadTestInput(15)
	expected := "10092"
	if actual := part1(parseInput(input)); actual != expected {
		t.Errorf("expected %v but got %v", expected, actual)
	}
}

func TestPart2(t *testing.T) {
	input := util.ReadTestInput(15)
	expected := "9021"
	if actual := part2(parseInput(input)); actual != expected {
		t.Errorf("expected %v but got %v", expected, actual)
	}
}

func TestSmallInput(t *testing.T) {
	input := "#######\n#...#.#\n#.....#\n#..OO@#\n#..O..#\n#.....#\n#######\n\n<vv<<^^<<^^"
	expected := "618"

	if actual := part2(parseInput(input)); actual != expected {
		t.Errorf("expected %v but got %v", expected, actual)
	}
}

func TestCustomInput(t *testing.T) {
	input := "#########\n#.......#\n#..OO...#\n#.OOO...#\n#..O....#\n#...@...#\n#########\n\n>>^^<vv<<^"
	expected := "1235"

	if actual := part2(parseInput(input)); actual != expected {
		t.Errorf("expected %v but got %v", expected, actual)
	}
}
