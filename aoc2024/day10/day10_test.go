package day10

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	input := util.ReadTestInput(10)
	grid := util.ParseGrid(input)
	expected := "36"
	actual := part1(grid)

	if actual != expected {
		t.Errorf("Expected %s but was %s", expected, actual)
	}
}

func TestPart2(t *testing.T) {
	input := util.ReadTestInput(10)
	grid := util.ParseGrid(input)
	expected := "81"
	actual := part2(grid)

	if actual != expected {
		t.Errorf("Expected %s but was %s", expected, actual)
	}
}
