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
	DiagUpLeft
	DiagUpRight
	DiagDownLeft
	DiagDownRight
)

func part1(input string) string {
	target := "XMAS"
	grid := util.ParseGrid(input)

	res := 0
	for y, row := range grid {
		for x := range row {
			for _, dir := range []Direction{Up, Down, Left, Right, DiagUpLeft, DiagUpRight, DiagDownLeft, DiagDownRight} {
				if getAdjacentTarget(grid, x, y, target, dir) {
					res++
				}
			}
		}
	}

	return strconv.Itoa(res)
}

func part2(input string) string {
	target := "MAS"
	targetLen := len(target)
	grid := util.ParseGrid(input)

	res := 0
	for y, row := range grid {
		for x := range row {
			if (getAdjacentTarget(grid, x, y, target, DiagDownRight) || getAdjacentTarget(grid, x+targetLen-1, y+targetLen-1, target, DiagUpLeft)) &&
				(getAdjacentTarget(grid, x+targetLen-1, y, target, DiagDownLeft) || getAdjacentTarget(grid, x, y+targetLen-1, target, DiagUpRight)) {
				res++
			}
		}
	}

	return strconv.Itoa(res)
}

func getAdjacentTarget(grid [][]byte, x, y int, target string, dir Direction) bool {
	targetLen := len(target)
	height := len(grid)
	width := len(grid[0])

	if x < 0 || x >= width || y < 0 || y >= height {
		return false
	}

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
	case DiagUpLeft:
		if x-targetLen+1 >= 0 && y-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y-i][x-i])
			}
			return res == target
		}
	case DiagUpRight:
		if x+targetLen <= width && y-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y-i][x+i])
			}
			return res == target
		}
	case DiagDownLeft:
		if x-targetLen+1 >= 0 && y+targetLen <= height {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y+i][x-i])
			}
			return res == target
		}
	case DiagDownRight:
		if x+targetLen <= width && y+targetLen <= height {
			for i := 0; i < targetLen; i++ {
				res += string(grid[y+i][x+i])
			}
			return res == target
		}
	}

	return false
}
func Run(input string) string {
	return part1(input) + ", " + part2(input)
}
