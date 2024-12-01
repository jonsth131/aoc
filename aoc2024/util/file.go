package util

import (
	"fmt"
	"os"
	"strings"
)

func ReadInput(day int) string {
	data, err := os.ReadFile(fmt.Sprintf("inputs/day%d.txt", day+1))
	if err != nil {
		panic(err)
	}

	return strings.TrimSuffix(string(data), "\n")
}

func ReadTestInput(day int) string {
	data, err := os.ReadFile(fmt.Sprintf("../inputs/day%d-test.txt", day))
	if err != nil {
		panic(err)
	}

	return strings.TrimSuffix(string(data), "\n")
}
