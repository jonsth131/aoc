package day3

import (
	"testing"
)

func TestPart1(t *testing.T) {
	expected := "161"
	data := "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

	result := part1(data)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}

func TestPart2(t *testing.T) {
	expected := "48"
	data := "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

	result := part2(data)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}
