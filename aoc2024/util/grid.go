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
