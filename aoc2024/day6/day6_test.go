package day6

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	expected := "41"
	input := util.ReadTestInput(6)
	grid := util.ParseGrid(input)
	result := part1(grid)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}

func TestPart2(t *testing.T) {
	expected := "6"
	input := util.ReadTestInput(6)
	grid := util.ParseGrid(input)
	result := part2(grid)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}
