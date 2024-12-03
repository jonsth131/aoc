package day3

import (
	"regexp"
	"strconv"
)

func part1(input string) string {
	re := regexp.MustCompile(`mul\((\d{1,3}),(\d{1,3})\)`)
	matches := re.FindAllStringSubmatch(input, -1)

	res := 0
	for _, match := range matches {
		res += multiplyMatches(match)
	}
	return strconv.Itoa(res)
}

func part2(input string) string {
	res := 0
	enabled := true
	re := regexp.MustCompile(`don't\(\)|do\(\)|mul\((\d{1,3}),(\d{1,3})\)`)
	for _, match := range re.FindAllStringSubmatch(input, -1) {
		switch match[0] {
		case "don't()":
			enabled = false
		case "do()":
			enabled = true
		default:
			if enabled {
				res += multiplyMatches(match)
			}
		}
	}
	return strconv.Itoa(res)
}

func multiplyMatches(match []string) int {
	m1, err := strconv.Atoi(match[1])
	if err != nil {
		panic(err)
	}
	m2, err := strconv.Atoi(match[2])
	if err != nil {
		panic(err)
	}

	return m1 * m2
}

func Run(input string) string {
	return part1(input) + ", " + part2(input)
}
