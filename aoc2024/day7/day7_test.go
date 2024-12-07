package day7

import (
        "testing"

        "github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
        expected := "3749"
        input := util.ReadTestInput(7)
        equations := parse(input)
        result := part1(equations)

        if result != expected {
                t.Errorf("Expected %s but got %s", expected, result)
        }
}

func TestPart2(t *testing.T) {
        expected := "11387"
        input := util.ReadTestInput(7)
        equations := parse(input)
        result := part2(equations)

        if result != expected {
                t.Errorf("Expected %s but got %s", expected, result)
        }
}

