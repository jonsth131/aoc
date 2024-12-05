package day5

import (
	"slices"
	"strconv"
	"strings"

	"github.com/jonsth131/aoc/aoc2024/util"
)

type Validated struct {
	valid   [][]int
	invalid [][]int
}

func part1(pages [][]int) string {
	return strconv.Itoa(getSumMiddlePages(pages))
}

func part2(rules map[int][]int, pages [][]int) string {
	for _, page := range pages {
		sort(rules, page)
	}
	return strconv.Itoa(getSumMiddlePages(pages))
}

func getSumMiddlePages(pages [][]int) int {
	res := 0
	for _, page := range pages {
		res += page[len(page)/2]
	}
	return res
}

func sort(rules map[int][]int, page []int) {
	for i := len(page) - 1; i >= 0; i-- {
		if !checkRule(rules, page[i:]) {
			page[i], page[i+1] = page[i+1], page[i]
			sort(rules, page)
			break
		}
	}
}

func validate(rules map[int][]int, pages [][]int) Validated {
	validPages := make([][]int, 0)
	invalidPages := make([][]int, 0)
	for _, page := range pages {
		valid := false
		for i := range page {
			if checkRule(rules, page[i:]) {
				valid = true
			} else {
				valid = false
				break
			}
		}

		if valid {
			validPages = append(validPages, page)
		} else {
			invalidPages = append(invalidPages, page)
		}
	}

	return Validated{validPages, invalidPages}
}

func checkRule(rules map[int][]int, pages []int) bool {
	page := pages[0]
	rule := rules[page]
	for _, p := range pages[1:] {
		if !slices.Contains(rule, p) {
			return false
		}
	}
	return true
}

func parseInput(input string) (map[int][]int, [][]int) {
	parts := strings.Split(input, "\n\n")
	rules := make(map[int][]int)
	for _, rule := range strings.Split(parts[0], "\n") {
		parts := strings.Split(rule, "|")
		first, err := strconv.Atoi(parts[0])
		if err != nil {
			panic(err)
		}
		second, err := strconv.Atoi(parts[1])
		if err != nil {
			panic(err)
		}
		rules[first] = append(rules[first], second)
	}
	pages := util.ParseLinesOfInts(parts[1], ",")
	return rules, pages
}

func Run(input string) string {
	rules, pages := parseInput(input)
	validated := validate(rules, pages)
	return part1(validated.valid) + ", " + part2(rules, validated.invalid)
}
