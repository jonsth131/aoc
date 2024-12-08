package day8

import (
	"fmt"
	"github.com/jonsth131/aoc/aoc2024/util"
)

func part1(grid util.Grid) string {
	nodes := findNodes(grid)
	antinodes := make(map[util.Point]bool)
	width, height := len(grid[0]), len(grid)
	for _, points := range nodes {
		for i, point := range points {
			if i == len(points)-1 {
				break
			}
			remaining := points[i+1:]
			for _, other := range remaining {
				dx := point.X - other.X
				dy := point.Y - other.Y
				newPoint1 := util.Point{X: point.X + dx, Y: point.Y + dy}
				newPoint2 := util.Point{X: other.X - dx, Y: other.Y - dy}
				if newPoint1.X >= 0 && newPoint1.X < width && newPoint1.Y >= 0 && newPoint1.Y < height {
					antinodes[newPoint1] = true
				}
				if newPoint2.X >= 0 && newPoint2.X < width && newPoint2.Y >= 0 && newPoint2.Y < height {
					antinodes[newPoint2] = true
				}
			}
		}
	}
	return fmt.Sprintf("%d", len(antinodes))
}

func part2(grid util.Grid) string {
	nodes := findNodes(grid)
	antinodes := make(map[util.Point]bool)
	width, height := len(grid[0]), len(grid)
	for _, points := range nodes {
		for i, point := range points {
			antinodes[point] = true
			if i == len(points)-1 {
				break
			}
			remaining := points[i+1:]
			for _, other := range remaining {
				dx := point.X - other.X
				dy := point.Y - other.Y
				newPoint := point
				for {
					newPoint = util.Point{X: newPoint.X + dx, Y: newPoint.Y + dy}
					if newPoint.X < 0 || newPoint.X >= width || newPoint.Y < 0 || newPoint.Y >= height {
						break
					}
					antinodes[newPoint] = true
				}
				newPoint = other
				for {
					newPoint = util.Point{X: newPoint.X - dx, Y: newPoint.Y - dy}
					if newPoint.X < 0 || newPoint.X >= width || newPoint.Y < 0 || newPoint.Y >= height {
						break
					}
					antinodes[newPoint] = true
				}
			}
		}
	}
	return fmt.Sprintf("%d", len(antinodes))
}

func findNodes(grid util.Grid) map[byte][]util.Point {
	nodes := make(map[byte][]util.Point)
	for y, row := range grid {
		for x, c := range row {
			if c != '.' {
				nodes[c] = append(nodes[c], util.Point{X: x, Y: y})
			}
		}
	}
	return nodes
}

func Run(input string) string {
	grid := util.ParseGrid(input)
	return part1(grid) + ", " + part2(grid)
}
