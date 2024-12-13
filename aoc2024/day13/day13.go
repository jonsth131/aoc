package day13

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/jonsth131/aoc/aoc2024/util"
)

type PuzzleInput struct {
	ButtonA util.Point
	ButtonB util.Point
	Prize   util.Point
}

func part1(input []PuzzleInput) string {
	tokens := 0
	for _, pi := range input {
		tokens += calculate(pi, 0)
	}
	return fmt.Sprintf("%d", tokens)
}

func part2(input []PuzzleInput) string {
	tokens := 0
	for _, pi := range input {
		tokens += calculate(pi, 10000000000000)
	}
	return fmt.Sprintf("%d", tokens)
}

func calculate(input PuzzleInput, offset int) int {
	input.Prize.X += offset
	input.Prize.Y += offset
	D := input.ButtonA.X*input.ButtonB.Y - input.ButtonB.X*input.ButtonA.Y
	Dx := input.Prize.X*input.ButtonB.Y - input.ButtonB.X*input.Prize.Y
	Dy := input.ButtonA.X*input.Prize.Y - input.Prize.X*input.ButtonA.Y
	if D != 0 && Dx == (Dx/D)*D && Dy == (Dy/D)*D {
		return (Dx/D)*3 + (Dy / D)
	}
	return 0
}

func parse(input string) []PuzzleInput {
	res := []PuzzleInput{}
	for _, block := range strings.Split(input, "\n\n") {
		lines := strings.Split(block, "\n")
		a := parsePoint(lines[0], "+")
		b := parsePoint(lines[1], "+")
		prize := parsePoint(lines[2], "=")
		res = append(res, PuzzleInput{ButtonA: a, ButtonB: b, Prize: prize})
	}
	return res
}

func parsePoint(line string, sep string) util.Point {
	button := strings.Split(line, ":")[1]
	buttons := strings.Split(button, ",")
	x, _ := strconv.Atoi(strings.Split(buttons[0], sep)[1])
	y, _ := strconv.Atoi(strings.Split(buttons[1], sep)[1])
	return util.Point{X: x, Y: y}
}

func Run(input string) string {
	parsed := parse(input)
	return part1(parsed) + ", " + part2(parsed)
}
