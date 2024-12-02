package day1

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestPart1(t *testing.T) {
	expected := "11"
	data := util.ReadTestInput(1)
	arr1, arr2 := parse(data)

	result := part1(arr1, arr2)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}

func TestPart2(t *testing.T) {
	expected := "31"
	data := util.ReadTestInput(1)
	arr1, arr2 := parse(data)

	result := part2(arr1, arr2)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}

func TestDay1(t *testing.T) {
	expected := "11, 31"
	data := util.ReadTestInput(1)
	result := Run(data)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}
