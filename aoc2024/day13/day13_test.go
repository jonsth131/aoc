package day13

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	input := util.ReadTestInput(13)
	expected := "480"

	if actual := part1(parse(input)); actual != expected {
		t.Errorf("expected %v but got %v", expected, actual)
	}
}
