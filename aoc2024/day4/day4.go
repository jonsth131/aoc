package day4

import (
	"strconv"

	"github.com/jonsth131/aoc/aoc2024/util"
)

type Direction int

const (
	Up Direction = iota
	Down
	Left
	Right
	Upleft
	Upright
	Downleft
	Downright
)

func part1(input string) string {
	target := "XMAS"
	grid := util.ParseGrid(input)

	res := 0
	for y, row := range grid {
		for x := range row {
			res += getAdjacentTargets(grid, x, y, target)
		}
	}

	return strconv.Itoa(res)
}

func getAdjacentTargets(grid [][]byte, x, y int, target string) int {
	count := 0
	for _, dir := range []Direction{Up, Down, Left, Right, Upleft, Upright, Downleft, Downright} {
		if getAdjacentTarget(grid, x, y, target, dir) {
			count++
		}
	}
	return count
}

func getAdjacentTarget(grid [][]byte, x, y int, target string, dir Direction) bool {
	targetLen := len(target)
	height := len(grid)
	width := len(grid[0])

	res := ""
	switch dir {
	case Up:
		if y-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y-i][x])
			}
			return res == target
		}
	case Down:
		if y+targetLen <= height {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y+i][x])
			}
			return res == target
		}
	case Left:
		if x-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y][x-i])
			}
			return res == target
		}
	case Right:
		if x+targetLen <= width {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y][x+i])
			}
			return res == target
		}
	case Upleft:
		if x-targetLen+1 >= 0 && y-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y-i][x-i])
			}
			return res == target
		}
	case Upright:
		if x+targetLen <= width && y-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y-i][x+i])
			}
			return res == target
		}
	case Downleft:
		if x-targetLen+1 >= 0 && y+targetLen <= height {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y+i][x-i])
			}
			return res == target
		}
	case Downright:
		if x+targetLen <= width && y+targetLen <= height {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y+i][x+i])
			}
			return res == target
		}
	}

	return false
}

func part2(input string) string {
	grid := util.ParseGrid(input)

	res := 0
	for y, row := range grid {
		for x := range row {
			if grid[y][x] == 'A' {
				if isMatch(grid, x, y) {
					res++
				}
			}
		}
	}

	return strconv.Itoa(res)
}

func isMatch(grid [][]byte, x, y int) bool {
	height := len(grid)
	width := len(grid[0])

	if y-1 >= 0 && y+1 < height && x-1 >= 0 && x+1 < width {
		leftRightMatch := (grid[y-1][x-1] == 'M' && grid[y+1][x+1] == 'S') || (grid[y-1][x-1] == 'S' && grid[y+1][x+1] == 'M')
		rightLeftMatch := (grid[y+1][x-1] == 'M' && grid[y-1][x+1] == 'S') || (grid[y+1][x-1] == 'S' && grid[y-1][x+1] == 'M')
		return leftRightMatch && rightLeftMatch
	}
	return false
}

func Run(input string) string {
	return part1(input) + ", " + part2(input)
}
