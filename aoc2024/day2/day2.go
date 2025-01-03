package day2

import (
	"strconv"

	"github.com/jonsth131/aoc/aoc2024/util"
)

const (
	None       = 0
	Decreasing = 1
	Increasing = 2
)

func part1(input [][]int) string {
	res := getSafeReports(input, false)
	return strconv.Itoa(res)
}

func part2(input [][]int) string {
	res := getSafeReports(input, true)
	return strconv.Itoa(res)
}

func Run(input string) string {
	arr := util.ParseLinesOfInts(input, " ")
	return part1(arr) + ", " + part2(arr)
}

func getSafeReports(input [][]int, canSkip bool) int {
	res := 0
	for _, line := range input {
		if isReportSafe(line) {
			res++
		} else if canSkip {
			for i := 0; i < len(line); i++ {
				if isReportSafe(deleteIndex(line, i)) {
					res++
					break
				}
			}
		}
	}

	return res
}

func isReportSafe(report []int) bool {
	state := None
	for i := 0; i < len(report)-1; i++ {
		first, second := report[i], report[i+1]

		if i == 0 {
			state = getPairState(first, second)
		}

		pairState := getPairState(first, second)

		if state == None || state != pairState {
			return false
		}

		diff := util.GetDiff(first, second)

		if diff <= 0 || diff > 3 {
			return false
		}
	}

	return true
}

func deleteIndex(s []int, index int) []int {
	report := make([]int, len(s))
	copy(report, s)

	return append(report[:index], report[index+1:]...)
}

func getPairState(first, second int) int {
	if first < second {
		return Increasing
	} else if first > second {
		return Decreasing
	} else {
		return None
	}
}
