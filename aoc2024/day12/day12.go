package day12

import (
	"fmt"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func part1(grid util.Grid) string {
	totalPrice := 0
	totalVisited := make(map[util.Point]bool)

	for {
		p := findNonVisited(grid, totalVisited)
		if p == (util.Point{X: -1, Y: -1}) {
			break
		}
		visited := floodFill(grid, p)
		for p := range visited {
			totalVisited[p] = true
		}
		totalPrice += calculatePrice(visited)
	}

	return fmt.Sprintf("%v", totalPrice)
}

func part2(grid util.Grid) string {
	totalPrice := 0
	totalVisited := make(map[util.Point]bool)

	for {
		p := findNonVisited(grid, totalVisited)
		if p == (util.Point{X: -1, Y: -1}) {
			break
		}
		visited := floodFill(grid, p)
		for p := range visited {
			totalVisited[p] = true
		}
		totalPrice += calculateUpdatedPrice(visited)
	}

	return fmt.Sprintf("%v", totalPrice)
}

func floodFill(grid util.Grid, p util.Point) map[util.Point]bool {
	visited := make(map[util.Point]bool)

	q := []util.Point{p}

	for len(q) > 0 {
		p := q[0]
		q = q[1:]
		if visited[p] {
			continue
		}
		visited[p] = true
		current := grid.Get(p)

		for _, d := range []util.Direction{util.Up, util.Down, util.Left, util.Right} {
			neighbor := p.Add(d)
			if !grid.InBounds(neighbor) {
				continue
			}

			val := grid.Get(neighbor)
			if val == current {
				q = append(q, neighbor)
			}
		}
	}

	return visited
}

func findNonVisited(grid util.Grid, visited map[util.Point]bool) util.Point {
	for y, row := range grid {
		for x := range row {
			p := util.Point{X: x, Y: y}
			if !visited[p] {
				return p
			}
		}
	}
	return util.Point{X: -1, Y: -1}
}

func calculatePrice(visited map[util.Point]bool) int {
	area := 0
	perimeter := 0
	for p := range visited {
		area++
		for _, d := range []util.Direction{util.Up, util.Down, util.Left, util.Right} {
			neighbor := p.Add(d)
			if !visited[neighbor] {
				perimeter++
			}
		}
	}
	return area * perimeter
}

func calculateUpdatedPrice(visited map[util.Point]bool) int {
	area := 0
	totalCorners := 0

	for p := range visited {
		area++
		if !visited[p.Add(util.Up)] && !visited[p.Add(util.Right)] {
			totalCorners++
		}
		if !visited[p.Add(util.Up)] && !visited[p.Add(util.Left)] {
			totalCorners++
		}
		if !visited[p.Add(util.Down)] && !visited[p.Add(util.Right)] {
			totalCorners++
		}
		if !visited[p.Add(util.Down)] && !visited[p.Add(util.Left)] {
			totalCorners++
		}
		if !visited[p.Add(util.DiagonalUpLeft)] && visited[p.Add(util.Up)] && visited[p.Add(util.Left)] {
			totalCorners++
		}
		if !visited[p.Add(util.DiagonalUpRight)] && visited[p.Add(util.Up)] && visited[p.Add(util.Right)] {
			totalCorners++
		}
		if !visited[p.Add(util.DiagonalDownLeft)] && visited[p.Add(util.Down)] && visited[p.Add(util.Left)] {
			totalCorners++
		}
		if !visited[p.Add(util.DiagonalDownRight)] && visited[p.Add(util.Down)] && visited[p.Add(util.Right)] {
			totalCorners++
		}
	}

	return area * totalCorners
}

func Run(input string) string {
	grid := util.ParseGrid(input)
	return part1(grid) + ", " + part2(grid)
}
