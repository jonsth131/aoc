package day9

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	input := util.ReadTestInput(9)
	expected := "1928"
	actual := part1(input)

	if actual != expected {
		t.Errorf("Expected %v but was %v", expected, actual)
	}
}

func TestPart2(t *testing.T) {
	input := util.ReadTestInput(9)
	expected := "2858"
	actual := part2(input)

	if actual != expected {
		t.Errorf("Expected %v but was %v", expected, actual)
	}
}
