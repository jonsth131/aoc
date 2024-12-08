package day7

import (
	"fmt"
	"strconv"
	"strings"
)

type Equation struct {
	sum     int
	numbers []int
}

type Operator int

const (
	Add Operator = iota
	Multiply
	Concatenate
)

func part1(equations []Equation) string {
	res := 0
	operators := []Operator{Add, Multiply}
	for _, eq := range equations {
		if canCalculate(eq, operators) {
			res += eq.sum
		}
	}
	return fmt.Sprintf("%d", res)
}

func part2(equations []Equation) string {
	res := 0
	operators := []Operator{Add, Multiply, Concatenate}
	for _, eq := range equations {
		if canCalculate(eq, operators) {
			res += eq.sum
		}
	}
	return fmt.Sprintf("%d", res)
}

func canCalculate(eq Equation, operators []Operator) bool {
	perms := permutations(operators, len(eq.numbers)-1)
	for _, perm := range perms {
		sum := eq.numbers[0]
		for i, op := range perm {
			if op == Add {
				sum += eq.numbers[i+1]
			} else if op == Multiply {
				sum *= eq.numbers[i+1]
			} else if op == Concatenate {
                for n := eq.numbers[i+1]; n > 0; n /= 10 {
                    sum *= 10
                }
                sum += eq.numbers[i+1]
			} else {
				panic("Invalid operator")
			}
		}
		if sum == eq.sum {
			return true
		}
	}
	return false
}

func parse(input string) []Equation {
	res := make([]Equation, 0)
	for _, line := range strings.Split(input, "\n") {
		if line == "" {
			continue
		}
		res = append(res, parseLine(line))
	}
	return res
}

func parseLine(line string) Equation {
	res := Equation{0, make([]int, 0)}
	s := strings.Split(line, ":")
	for _, c := range strings.Split(s[1], " ") {
		val, err := strconv.Atoi(c)
		if err != nil {
			continue
		}
		res.numbers = append(res.numbers, val)
	}
	res.sum, _ = strconv.Atoi(s[0])
	return res
}

func permute[T any](arr []T, length int, current []T, result *[][]T) {
	if len(current) == length {
		permutation := make([]T, len(current))
		copy(permutation, current)
		*result = append(*result, permutation)
		return
	}

	for i := 0; i < len(arr); i++ {
		permute(arr, length, append(current, arr[i]), result)
	}
}

func permutations[T any](arr []T, length int) [][]T {
	var result [][]T
	permute(arr, length, []T{}, &result)
	return result
}

func Run(input string) string {
	equations := parse(input)
	return part1(equations) + ", " + part2(equations)
}
