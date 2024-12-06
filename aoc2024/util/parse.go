package util

import (
	"strconv"
	"strings"
)

func ParseLinesOfInts(input string, separator string) [][]int {
	res := make([][]int, 0)
	for _, line := range strings.Split(input, "\n") {
		if line == "" {
			continue
		}
		res = append(res, ParseLineOfInts(line, separator))
	}
	return res
}

func ParseLineOfInts(line string, separator string) []int {
	res := make([]int, 0)
	for _, c := range strings.Split(line, separator) {
		val, err := strconv.Atoi(c)
		if err != nil {
			continue
		}
		res = append(res, val)
	}
	return res
}

func ParseGrid(input string) Grid {
	res := make(Grid, 0)
	for _, line := range strings.Split(input, "\n") {
		if line == "" {
			continue
		}
		res = append(res, ParseLineOfBytes(line))
	}
	return res
}

func ParseLineOfBytes(line string) []byte {
	res := make([]byte, 0)
	for _, c := range line {
		res = append(res, byte(c))
	}
	return res
}
