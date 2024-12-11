package day11

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	input := util.ReadTestInput(11)
	stones := util.ParseLineOfInts(input, " ")
	expected := "55312"

	if actual := part1(stones); actual != expected {
		t.Errorf("expected %s, but got %s", expected, actual)
	}
}

func TestPart2(t *testing.T) {
	input := util.ReadTestInput(11)
	stones := util.ParseLineOfInts(input, " ")
	expected := "65601038650482"

	if actual := part2(stones); actual != expected {
		t.Errorf("expected %s, but got %s", expected, actual)
	}
}
