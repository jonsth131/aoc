package day2

import (
	"testing"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func TestGetSafeReportsShouldFail(t *testing.T) {
	expected := 0
	data := "68 70 71 69 72 74 71"
	arr := util.ParseLineOfInts(data)

	result := getSafeReports([][]int{arr}, true)

	if result != expected {
		t.Errorf("Expected %v but got %v", expected, result)
	}
}

func TestGetSafeReportsShouldPass(t *testing.T) {
	expected := 1
	data := "68 70 71 69 72 74 75"
	arr := util.ParseLineOfInts(data)

	result := getSafeReports([][]int{arr}, true)

	if result != expected {
		t.Errorf("Expected %v but got %v", expected, result)
	}
}

func TestPart1(t *testing.T) {
	expected := "2"
	data := util.ReadTestInput(2)
	arr := util.ParseLinesOfInts(data)

	result := part1(arr)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}

func TestPart2(t *testing.T) {
	expected := "4"
	data := util.ReadTestInput(2)
	arr := util.ParseLinesOfInts(data)

	result := part2(arr)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}

func TestDay1(t *testing.T) {
	expected := "2, 4"
	data := util.ReadTestInput(2)
	result := Run(data)

	if result != expected {
		t.Errorf("Expected %s but got %s", expected, result)
	}
}
