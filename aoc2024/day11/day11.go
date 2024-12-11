package day11

import (
	"fmt"
	"math"

	"github.com/jonsth131/aoc/aoc2024/util"
)

func part1(input []int) string {
	result := simulate(input, 25)
	return fmt.Sprintf("%d", result)
}

func part2(input []int) string {
	result := simulate(input, 75)
	return fmt.Sprintf("%d", result)
}

func simulate(input []int, blinks int) int {
	stoneCounts := make(map[int]int)
	for _, stone := range input {
		stoneCounts[stone]++
	}

	for i := 0; i < blinks; i++ {
		nextCounts := make(map[int]int)
		for stone, count := range stoneCounts {
			digits := 1
			temp := stone
			for temp >= 10 {
				temp /= 10
				digits++
			}

			if stone == 0 {
				nextCounts[1] += count
			} else if digits%2 == 0 {
				parts := digits / 2
				power := int(math.Pow10(parts))
				left := stone / power
				right := stone % power
				nextCounts[left] += count
				nextCounts[right] += count
			} else {
				nextCounts[stone*2024] += count
			}
		}
		stoneCounts = nextCounts
	}

	totalStones := 0
	for _, count := range stoneCounts {
		totalStones += count
	}

	return totalStones
}

func Run(input string) string {
	stones := util.ParseLineOfInts(input, " ")
	return part1(stones) + ", " + part2(stones)
}
