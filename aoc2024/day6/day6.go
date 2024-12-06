package day6

import (
	"fmt"

	"github.com/jonsth131/aoc/aoc2024/util"
)

type Direction int

const (
	Up Direction = iota
	Down
	Left
	Right
)

type Line struct {
	start util.Point
	end   util.Point
}

func part1(grid util.Grid) string {
	visited := visit(grid)
	return fmt.Sprintf("%d", len(visited))
}

func part2(grid util.Grid) string {
	visited := visit(grid)
	res := 0
	for p := range visited {
		if grid[p.Y][p.X] == '.' {
			grid[p.Y][p.X] = '#'
			if isLoop(grid) {
				res++
			}
			grid[p.Y][p.X] = '.'
		}
	}
	return fmt.Sprintf("%d", res)
}

func visit(grid util.Grid) map[util.Point]bool {
	visited := make(map[util.Point]bool)
	position := grid.FindPoint('^')
	visited[*position] = true
	dir := Up
	for isEnd(grid, position, dir) == false {
		if canMove(grid, position, dir) {
			position = move(position, dir)
			visited[*position] = true
		} else {
			dir = getNextDirection(dir)
		}
	}
	return visited
}

func isLoop(grid util.Grid) bool {
	visited := make(map[Line]bool)
	start := grid.FindPoint('^')
	dir := Up
	for {
		end := getLineEnd(grid, start, dir)
		if isEnd(grid, end, dir) {
			return false
		}
		if visited[Line{*start, *end}] {
			return true
		}
		visited[Line{*start, *end}] = true
		start = end
		dir = getNextDirection(dir)
	}
}

func getLineEnd(grid util.Grid, p *util.Point, dir Direction) *util.Point {
	for canMove(grid, p, dir) {
		p = move(p, dir)
	}
	return p
}

func isEnd(grid util.Grid, p *util.Point, dir Direction) bool {
	next := move(p, dir)
	if next.Y < 0 || next.Y >= len(grid) {
		return true
	}
	if next.X < 0 || next.X >= len(grid[next.Y]) {
		return true
	}
	return false
}

func canMove(grid util.Grid, p *util.Point, dir Direction) bool {
	next := move(p, dir)
	if next.Y < 0 || next.Y >= len(grid) {
		return false
	}
	if next.X < 0 || next.X >= len(grid[next.Y]) {
		return false
	}
	return grid[next.Y][next.X] != '#'
}

func move(p *util.Point, dir Direction) *util.Point {
	switch dir {
	case Up:
		return &util.Point{X: p.X, Y: p.Y - 1}
	case Down:
		return &util.Point{X: p.X, Y: p.Y + 1}
	case Left:
		return &util.Point{X: p.X - 1, Y: p.Y}
	case Right:
		return &util.Point{X: p.X + 1, Y: p.Y}
	}
	return p
}

func getNextDirection(dir Direction) Direction {
	switch dir {
	case Up:
		return Right
	case Down:
		return Left
	case Left:
		return Up
	case Right:
		return Down
	}
	return dir
}

func Run(input string) string {
	grid := util.ParseGrid(input)
	return part1(grid) + ", " + part2(grid)
}
