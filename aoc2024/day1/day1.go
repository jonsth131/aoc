package day1

import (
	"math"
	"sort"
	"strconv"
	"strings"
)

func parse(input string) ([]int, []int) {
	arr1 := []int{}
	arr2 := []int{}

	lines := strings.Split(input, "\n")

	for _, line := range lines {
		values := strings.Split(line, " ")
		first, err := strconv.Atoi(values[0])
		if err != nil {
			panic(err)
		}
		second, err := strconv.Atoi(values[len(values)-1])
		if err != nil {
			panic(err)
		}
		arr1 = append(arr1, first)
		arr2 = append(arr2, second)
	}
	sort.Ints(arr1)
	sort.Ints(arr2)

	return arr1, arr2
}

func part1(arr1 []int, arr2 []int) string {
	res := 0

	for i := range arr1 {
		res += int(math.Abs(float64(arr1[i] - arr2[i])))
	}

	return strconv.Itoa(res)
}

func part2(arr1 []int, arr2 []int) string {
	res := 0
	times := make(map[int]int)

	for i := range arr2 {
		times[arr2[i]]++
	}

	for i := range arr1 {
		res += times[arr1[i]] * arr1[i]
	}

	return strconv.Itoa(res)
}

func Day1(input string) string {
	arr1, arr2 := parse(input)
	return part1(arr1, arr2) + ", " + part2(arr1, arr2)
}
