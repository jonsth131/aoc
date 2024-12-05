package day5

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	expected := "143"
	input := util.ReadTestInput(5)
	rules, pages := parseInput(input)
	validated := validate(rules, pages)
	result := part1(validated.valid)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}

func TestPart2(t *testing.T) {
	expected := "123"
	input := util.ReadTestInput(5)
	rules, pages := parseInput(input)
	validated := validate(rules, pages)
	result := part2(rules, validated.invalid)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}
