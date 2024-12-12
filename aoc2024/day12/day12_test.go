package day12

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	input := util.ReadTestInput(12)
	grid := util.ParseGrid(input)
	expected := "1930"

	if actual := part1(grid); actual != expected {
		t.Errorf("expected %q, actual %q", expected, actual)
	}
}

func TestPart2(t *testing.T) {
	input := util.ReadTestInput(12)
	grid := util.ParseGrid(input)
	expected := "1206"

	if actual := part2(grid); actual != expected {
		t.Errorf("expected %q, actual %q", expected, actual)
	}
}
