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

func part1(grid util.Grid) string {
	target := "XMAS"

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

func part2(grid util.Grid) string {
	target := "MAS"
	targetLen := len(target)

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

func getAdjacentTarget(grid util.Grid, x, y int, target string, dir Direction) bool {
	targetLen := len(target)
	height := len(grid)
	width := len(grid[0])

	if x < 0 || x >= width || y < 0 || y >= height {
		return false
	}

	switch dir {
	case Up:
		if y-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				if grid[y-i][x] != target[i] {
					return false
				}
			}
			return true
		}
	case Down:
		if y+targetLen <= height {
			for i := 0; i < targetLen; i++ {
				if grid[y+i][x] != target[i] {
					return false
				}
			}
			return true
		}
	case Left:
		if x-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				if grid[y][x-i] != target[i] {
					return false
				}
			}
			return true
		}
	case Right:
		if x+targetLen <= width {
			for i := 0; i < targetLen; i++ {
				if grid[y][x+i] != target[i] {
					return false
				}
			}
			return true
		}
	case DiagUpLeft:
		if x-targetLen+1 >= 0 && y-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				if grid[y-i][x-i] != target[i] {
					return false
				}
			}
			return true
		}
	case DiagUpRight:
		if x+targetLen <= width && y-targetLen+1 >= 0 {
			for i := 0; i < targetLen; i++ {
				if grid[y-i][x+i] != target[i] {
					return false
				}
			}
			return true
		}
	case DiagDownLeft:
		if x-targetLen+1 >= 0 && y+targetLen <= height {
			for i := 0; i < targetLen; i++ {
				if grid[y+i][x-i] != target[i] {
					return false
				}
			}
			return true
		}
	case DiagDownRight:
		if x+targetLen <= width && y+targetLen <= height {
			for i := 0; i < targetLen; i++ {
				if grid[y+i][x+i] != target[i] {
					return false
				}
			}
			return true
		}
	}

	return false
}

func Run(input string) string {
	grid := util.ParseGrid(input)
	return part1(grid) + ", " + part2(grid)
}
