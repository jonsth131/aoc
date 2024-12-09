package day9

import (
	"fmt"
	"strings"
)

type Block struct {
	index  int
	length int
	isFile bool
}

func part1(input string) string {
	blocks := parse(input)
	left := 1
	right := len(blocks) - 1
	compressed := make([]Block, 0)
	compressed = append(compressed, blocks[0])

	r := blocks[right]
	space := blocks[left]

	for left < right {
		if space.length >= r.length {
			compressed = append(compressed, r)
			space.length -= r.length
			right -= 2
			r = blocks[right]
		} else {
			newBlock := Block{index: r.index, length: space.length, isFile: true}
			compressed = append(compressed, newBlock)
			r.length -= space.length
			left++
			if left < right {
				compressed = append(compressed, blocks[left])
				left++
				space = blocks[left]
			}
		}
	}

	compressed = append(compressed, r)
	checksum := calculateChecksum(compressed)

	return fmt.Sprintf("%v", checksum)
}

func part2(input string) string {
	blocks := parse(input)

	idx := 0

	for {
		if idx >= len(blocks) {
			break
		}

		if !blocks[idx].isFile && blocks[idx].length > 0 {
			for i := len(blocks) - 1; i > idx; i-- {
				if blocks[i].isFile {
					if blocks[idx].length >= blocks[i].length {
						blocks[idx], blocks[i] = blocks[i], blocks[idx]
						if blocks[i].length-blocks[idx].length > 0 {
							newSpace := Block{index: blocks[i].index, length: blocks[i].length - blocks[idx].length, isFile: false}
							blocks[i].length = blocks[idx].length
							blocks = append(blocks[:idx+1], append([]Block{newSpace}, blocks[idx+1:]...)...)
						}
						break
					}
				}
			}
		}

		idx++
	}

	checksum := calculateChecksum(blocks)

	return fmt.Sprintf("%v", checksum)
}

func parse(input string) []Block {
	blocks := make([]Block, 0)
	for i, c := range input {
		isFile := i%2 == 0
		idx := 0
		if isFile {
			idx = i / 2
		}
		blocks = append(blocks, Block{index: idx, length: int(c - '0'), isFile: isFile})
	}
	return blocks
}

func calculateChecksum(blocks []Block) int {
	checksum := 0
	i := 0
	for _, block := range blocks {
		for j := 0; j < block.length; j++ {
			checksum += block.index * i
			i++
		}
	}

	return checksum
}

func Run(input string) string {
	input = strings.TrimSuffix(input, "\n")
	return part1(input) + ", " + part2(input)
}
