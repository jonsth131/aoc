package day15

import (
	"fmt"
	"slices"
	"strings"

	"github.com/jonsth131/aoc/aoc2024/util"
)

type puzzle struct {
	warehouse util.Grid
	moves     string
}

type expandedBox struct {
	left  util.Point
	right util.Point
}

type boxMove struct {
	old expandedBox
	new expandedBox
}

func part1(data puzzle) string {
	for _, move := range data.moves {
		moveRobot(data.warehouse, move)
	}

	boxes := data.warehouse.FindAll('O')

	sum := 0
	for _, box := range boxes {
		sum += box.X + (box.Y * 100)
	}

	return fmt.Sprintf("%d", sum)
}

func part2(data puzzle) string {
	data.warehouse = expand(data.warehouse)
	for _, move := range data.moves {
		moveRobotExpanded(data.warehouse, move)
	}

	boxes := data.warehouse.FindAll('[')

	sum := 0
	for _, box := range boxes {
		sum += box.X + (box.Y * 100)
	}

	return fmt.Sprintf("%d", sum)
}

func expand(grid util.Grid) util.Grid {
	newGrid := make(util.Grid, len(grid))

	for y, row := range grid {
		newRow := make(util.GridRow, len(row)*2)
		for x, cell := range row {
			if cell == '@' {
				newRow[x*2] = cell
				newRow[x*2+1] = '.'
			} else if cell == 'O' {
				newRow[x*2] = '['
				newRow[x*2+1] = ']'
			} else {
				newRow[x*2] = cell
				newRow[x*2+1] = cell
			}
		}
		newGrid[y] = newRow
	}

	return newGrid
}

func moveRobotExpanded(grid util.Grid, move rune) {
	start := grid.FindPoint('@')
	switch move {
	case '^':
		moveVerticalExpanded(grid, *start, util.Up)
	case 'v':
		moveVerticalExpanded(grid, *start, util.Down)
	case '<':
		moveLeftExpanded(grid, *start)
	case '>':
		moveRightExpanded(grid, *start)
	}
}

func moveVerticalExpanded(grid util.Grid, start util.Point, dir util.Direction) {
	nextY := start.Add(dir).Y
	if grid[nextY][start.X] == '#' {
		return
	} else if grid[nextY][start.X] == '.' {
		grid[start.Y][start.X], grid[nextY][start.X] = grid[nextY][start.X], grid[start.Y][start.X]
		return
	}
	box := expandedBox{left: start.Add(dir), right: start.Add(dir)}

	if grid[nextY][start.X] == '[' {
		box.right = box.right.Add(util.Right)
	} else if grid[nextY][start.X] == ']' {
		box.left = box.left.Add(util.Left)
	}

	getMovesExpanded(grid, box, dir)
	if grid[nextY][start.X] == '.' {
		grid[start.Y][start.X], grid[nextY][start.X] = grid[nextY][start.X], grid[start.Y][start.X]
	}
}

func getMovesExpanded(grid util.Grid, box expandedBox, dir util.Direction) bool {
	nextY := box.left.Add(dir).Y

	if grid[nextY][box.left.X] == '.' && grid[nextY][box.right.X] == '.' {
		grid[nextY][box.left.X], grid[box.left.Y][box.left.X] = grid[box.left.Y][box.left.X], grid[nextY][box.left.X]
		grid[nextY][box.right.X], grid[box.right.Y][box.right.X] = grid[box.right.Y][box.right.X], grid[nextY][box.right.X]
		return true
	} else if grid[nextY][box.left.X] == '[' && grid[nextY][box.right.X] == ']' {
		next := expandedBox{left: box.left.Add(dir), right: box.right.Add(dir)}
		res := getMovesExpanded(grid, next, dir)
		if res {
			getMovesExpanded(grid, box, dir)
		}
		return res
	} else if grid[nextY][box.left.X] == '#' || grid[nextY][box.right.X] == '#' {
		return false
	}

	if grid[nextY][box.left.X-1] == '[' && grid[nextY][box.left.X] == ']' {
		next := expandedBox{left: box.left.Add(dir).Add(util.Left), right: box.left.Add(dir)}
		res := getMovesExpanded(grid, next, dir)
		if res {
			getMovesExpanded(grid, box, dir)
		}
		return res
	}

	if grid[nextY][box.right.X] == '[' && grid[nextY][box.right.X+1] == ']' {
		next := expandedBox{left: box.right.Add(dir), right: box.right.Add(dir).Add(util.Right)}
		res := getMovesExpanded(grid, next, dir)
		if res {
			getMovesExpanded(grid, box, dir)
		}
		return res
	}

	return false
}

func moveLeftExpanded(grid util.Grid, start util.Point) {
	endX := 0
	for i := start.X; i >= 0; i-- {
		if grid[start.Y][i] == '#' {
			break
		}
		if grid[start.Y][i] == '.' {
			endX = i
			break
		}
	}

	if endX == 0 {
		return
	}

	for i := endX; i < start.X; i++ {
		grid[start.Y][i], grid[start.Y][i+1] = grid[start.Y][i+1], grid[start.Y][i]
	}
}

func moveRightExpanded(grid util.Grid, start util.Point) {
	endX := 0
	for i := start.X; i < len(grid[0]); i++ {
		if grid[start.Y][i] == '#' {
			break
		}
		if grid[start.Y][i] == '.' {
			endX = i
			break
		}
	}

	if endX == 0 {
		return
	}

	for i := endX; i > start.X; i-- {
		grid[start.Y][i], grid[start.Y][i-1] = grid[start.Y][i-1], grid[start.Y][i]
	}
}

func moveRobot(grid util.Grid, move rune) {
	start := grid.FindPoint('@')
	switch move {
	case '^':
		moveDirection(grid, *start, util.Up)
	case 'v':
		moveDirection(grid, *start, util.Down)
	case '<':
		moveDirection(grid, *start, util.Left)
	case '>':
		moveDirection(grid, *start, util.Right)
	}
}

func moveDirection(grid util.Grid, start util.Point, dir util.Direction) {
	next := start.Add(dir)
	points := make([]util.Point, 0)
	for {
		if grid[next.Y][next.X] == '#' {
			points = make([]util.Point, 0)
			break
		}
		if grid[next.Y][next.X] == '.' {
			points = append(points, next)
			break
		}
		points = append(points, next)
		next = next.Add(dir)
	}
	movePoint(grid, start, points)
}

func movePoint(grid util.Grid, start util.Point, points []util.Point) {
	slices.Reverse(points)
	for _, p := range points {
		grid[p.Y][p.X], grid[start.Y][start.X] = grid[start.Y][start.X], grid[p.Y][p.X]
		start = p
	}
}

func parseInput(input string) puzzle {
	parts := strings.Split(input, "\n\n")
	grid := util.ParseGrid(parts[0])
	return puzzle{warehouse: grid, moves: parts[1]}
}

func Run(input string) string {
	data := parseInput(input)
	return part1(data) + ", " + part2(data)
}
