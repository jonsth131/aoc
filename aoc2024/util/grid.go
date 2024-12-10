package util

type Grid []GridRow
type GridRow []byte
type Point struct {
	X int
	Y int
}

type Direction int

const (
	Up Direction = iota
	Down
	Left
	Right
)

func (grid Grid) FindPoint(target byte) *Point {
	for y, row := range grid {
		for x, c := range row {
			if c == target {
				return &Point{x, y}
			}
		}
	}
	return nil
}

func (grid Grid) FindAll(target byte) []Point {
	points := []Point{}
	for y, row := range grid {
		for x, c := range row {
			if c == target {
				points = append(points, Point{x, y})
			}
		}
	}
	return points
}

func (grid Grid) GetSize() (int, int) {
	return len(grid[0]), len(grid)
}

func (grid Grid) InBounds(p Point) bool {
	width, height := grid.GetSize()
	return p.X >= 0 && p.X < width && p.Y >= 0 && p.Y < height
}

func (p Point) Add(d Direction) Point {
	switch d {
	case Up:
		return Point{p.X, p.Y - 1}
	case Down:
		return Point{p.X, p.Y + 1}
	case Left:
		return Point{p.X - 1, p.Y}
	case Right:
		return Point{p.X + 1, p.Y}
	}
	panic("Invalid direction")
}
