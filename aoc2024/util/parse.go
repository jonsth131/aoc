package util

import (
	"strconv"
	"strings"
)

func ParseLinesOfInts(input string) [][]int {
	res := make([][]int, 0)
	for _, line := range strings.Split(input, "\n") {
		if line == "" {
			continue
		}
		res = append(res, ParseLineOfInts(line))
	}
	return res
}

func ParseLineOfInts(line string) []int {
	res := make([]int, 0)
	for _, c := range strings.Split(line, " ") {
		val, err := strconv.Atoi(c)
		if err != nil {
			continue
		}
		res = append(res, val)
	}
	return res
}

func ParseGrid(input string) [][]byte {
	res := make([][]byte, 0)
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
