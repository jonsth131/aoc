package day10

import (
	"fmt"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func part1(grid util.Grid) string {
	startingPositions := grid.FindAll('0')
	res := 0
	for _, pos := range startingPositions {
		p1, _ := walk(grid, pos, '9')
		res += p1
	}

	return fmt.Sprintf("%d", res)
}

func part2(grid util.Grid) string {
	startingPositions := grid.FindAll('0')
	res := 0
	for _, pos := range startingPositions {
		_, p2 := walk(grid, pos, '9')
		res += p2
	}

	return fmt.Sprintf("%d", res)
}

func walk(grid util.Grid, next util.Point, target byte) (int, int) {
	queue := []util.Point{next}
	reachable := map[util.Point]bool{}
	total := 0

	for len(queue) > 0 {
		next = queue[0]
		queue = queue[1:]
		currentValue := grid[next.Y][next.X]
		if currentValue == target {
			reachable[next] = true
			total++
			continue
		}

		for d := range []util.Direction{util.Up, util.Down, util.Left, util.Right} {
			nextPoint := next.Add(util.Direction(d))
			if !grid.InBounds(nextPoint) {
				continue
			}
			nextValue := grid[nextPoint.Y][nextPoint.X]
			if nextValue == currentValue+1 {
				queue = append(queue, nextPoint)
			}
		}
	}

	return len(reachable), total
}

func Run(input string) string {
	grid := util.ParseGrid(input)
	return part1(grid) + ", " + part2(grid)
}
