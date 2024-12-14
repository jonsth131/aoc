package day14

import (
	"fmt"
	"image"
	"image/color"
	"image/png"
	"os"
	"strconv"
	"strings"

	"github.com/jonsth131/aoc/aoc2024/util"
)

type robot struct {
	start    util.Point
	velocity util.Point
}

func part1(robots []robot) string {
	positions := getPositions(robots, 100, 103, 101)
	total := getTotal(positions, 103, 101)
	return fmt.Sprintf("%d", total)
}

func part2(robots []robot) string {
	height := 103
	width := 101
	ticks := 7672

	positions := getPositions(robots, ticks, height, width)
	img := generateImage(positions, height, width)
	filename := fmt.Sprintf("output/image-%05d.png", ticks)
	saveImage(img, filename)

	printPositions(positions, height, width)

	return fmt.Sprintf("Image saved to %s", filename)
}

func saveImage(img image.Gray, filename string) {
	f, err := os.Create(filename)
	if err != nil {
		panic("Error creating file")
	}

	if err := png.Encode(f, &img); err != nil {
		f.Close()
		panic("Error encoding image")
	}

	if err := f.Close(); err != nil {
		panic("Error closing file")
	}
}

func generateImage(positions map[util.Point]int, height, width int) image.Gray {
	img := image.NewGray(image.Rect(0, 0, width, height))
	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			if positions[util.Point{X: x, Y: y}] > 0 {
				img.SetGray(x, y, color.Gray{Y: 255})
			} else {
				img.SetGray(x, y, color.Gray{Y: 0})
			}
		}
	}

	return *img
}

func printPositions(positions map[util.Point]int, height, width int) {
	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			if positions[util.Point{X: x, Y: y}] > 0 {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

func getPositions(robots []robot, ticks, height, width int) map[util.Point]int {
	positions := make(map[util.Point]int)
	for _, robot := range robots {
		pos := getPosition(robot, ticks, width, height)
		positions[pos]++
	}
	return positions
}

func getTotal(positions map[util.Point]int, height, width int) int {
	topLeft := 0
	topRight := 0
	bottomLeft := 0
	bottomRight := 0
	for pos, n := range positions {
		rowLength := width / 2
		colLength := height / 2

		if pos.X < rowLength && pos.Y < colLength {
			topLeft += n
		} else if pos.X > rowLength && pos.Y < colLength {
			topRight += n
		} else if pos.X < rowLength && pos.Y > colLength {
			bottomLeft += n
		} else if pos.X > rowLength && pos.Y > colLength {
			bottomRight += n
		}
	}

	return topLeft * topRight * bottomLeft * bottomRight
}

func getPosition(robot robot, ticks, height, width int) util.Point {
	newX := robot.start.X + robot.velocity.X*ticks
	newY := robot.start.Y + robot.velocity.Y*ticks
	newX = mod(newX, height)
	newY = mod(newY, width)
	return util.Point{X: newX, Y: newY}
}

func mod(a, b int) int {
	return (a%b + b) % b
}

func parse(input string) []robot {
	res := []robot{}
	for _, line := range strings.Split(input, "\n") {
		parts := strings.Split(line, " ")
		start := strings.Split(parts[0][2:], ",")
		velocity := strings.Split(parts[1][2:], ",")
		startx, _ := strconv.Atoi(start[0])
		starty, _ := strconv.Atoi(start[1])
		velocityx, _ := strconv.Atoi(velocity[0])
		velocityy, _ := strconv.Atoi(velocity[1])
		res = append(res, robot{util.Point{X: startx, Y: starty}, util.Point{X: velocityx, Y: velocityy}})
	}
	return res
}

func Run(input string) string {
	parsed := parse(input)
	return part1(parsed) + ", " + part2(parsed)
}
