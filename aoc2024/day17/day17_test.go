package day17

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	input := util.ReadTestInput(17)
	expected := "4,6,3,5,6,3,5,2,1,0"
	if actual := part1(parse(input)); actual != expected {
		t.Errorf("expected %v but got %v", expected, actual)
	}
}

func TestPart2Execute(t *testing.T) {
	program := Program{Cpu{117440, 0, 0, 0}, []Opcode{0, 3, 5, 4, 3, 0}, []int{}, true}
	expected := []int{0, 3, 5, 4, 3, 0}
	execute(&program)
	if !matchSlices(program.output, expected) {
		t.Errorf("expected %v but got %v", expected, program.output)
	}
}
