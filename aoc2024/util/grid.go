package util

type Grid []GridRow
type GridRow []byte
type Point struct {
	X int
	Y int
}

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

func (grid Grid) Copy() Grid {
	newGrid := make(Grid, len(grid))
	for y, row := range grid {
		newGrid[y] = make(GridRow, len(row))
		copy(newGrid[y], row)
	}
	return newGrid
}
