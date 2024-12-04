package main

import (
	"fmt"
	"os"
	"strconv"
	"time"

	"github.com/jonsth131/aoc/aoc2024/day1"
	"github.com/jonsth131/aoc/aoc2024/day2"
	"github.com/jonsth131/aoc/aoc2024/day3"
	"github.com/jonsth131/aoc/aoc2024/day4"
	"github.com/jonsth131/aoc/aoc2024/util"
)

type dayFunc func(input string) string

var days = []dayFunc{day1.Run, day2.Run, day3.Run, day4.Run}

func run(day int) {
	fmt.Printf("===== Day %d =====\n", day+1)
	input := util.ReadInput(day)
	dayFn := days[day]
	start := time.Now()
	result := dayFn(input)
	fmt.Printf("%s\n", result)
	elapsed := time.Since(start)
	fmt.Printf("%s\n", elapsed)
}

func main() {
	if len(os.Args) == 2 {
		day, err := strconv.Atoi(os.Args[1])
		day -= 1
		if err != nil {
			panic(err)
		}
		if day < 0 || day >= len(days) {
			panic("Invalid day")
		}
		run(day)
	} else {
		for i := range days {
			run(i)
		}
	}
}
