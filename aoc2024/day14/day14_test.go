package day14

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	input := util.ReadTestInput(14)
	expected := 12
	parsed := parse(input)
	positions := getPositions(parsed, 100, 11, 7)

	if actual := getTotal(positions, 11, 7); actual != expected {
		t.Errorf("expected %v but got %v", expected, actual)
	}
}
