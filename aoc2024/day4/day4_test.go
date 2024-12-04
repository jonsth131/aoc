package day4

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	expected := "18"
	data := util.ReadTestInput(4)
	grid := util.ParseGrid(data)

	result := part1(grid)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}

func TestPart2(t *testing.T) {
	expected := "9"
	data := util.ReadTestInput(4)
	grid := util.ParseGrid(data)

	result := part2(grid)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}
