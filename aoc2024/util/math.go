package util

import "math"

func GetDiff(first, second int) int {
	return int(math.Abs(float64(second - first)))
}
