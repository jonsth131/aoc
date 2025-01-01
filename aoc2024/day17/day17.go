package day17

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

type Cpu struct {
	a  int
	b  int
	c  int
	pc int
}

type Opcode int

const (
	adv Opcode = iota
	bxl
	bst
	jnz
	bxc
	out
	bdv
	cdv
)

type Combo int

const (
	zero Combo = iota
	one
	two
	three
	a
	b
	c
	reserved
)

type Program struct {
	cpu         Cpu
	opcodes     []Opcode
	output      []int
	checkOutput bool
}

func part1(input Program) string {
	execute(&input)
	return strings.Trim(strings.Join(strings.Fields(fmt.Sprint(input.output)), ","), "[]")
}

func part2() string {
	return "Not implemented"
}

func convertOpcodesToInts(opcodes []Opcode) []int {
	res := make([]int, len(opcodes))
	for i, v := range opcodes {
		res[i] = int(v)
	}
	return res
}

func matchStartOfSlice(a, b []int) bool {
	for i, v := range a {
		if b[i] != v {
			return false
		}
	}
	return true
}

func matchSlices(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	return matchStartOfSlice(a, b)
}

func execute(program *Program) {
	for {
		op := program.opcodes[program.cpu.pc]
		literal := program.opcodes[program.cpu.pc+1]
		program.cpu.pc += 2
		executeOpcode(op, int(literal), program)

		if program.cpu.pc >= len(program.opcodes) {
			break
		}
	}
}

func executeOpcode(op Opcode, literal int, program *Program) {
	switch op {
	case adv:
		program.cpu.a = calculateDivision(*program, literal)
	case bxl:
		program.cpu.b ^= literal
	case bst:
		program.cpu.b = getComboValue(Combo(literal), *program) % 8
	case jnz:
		if program.cpu.a != 0 {
			program.cpu.pc = literal
		}
	case bxc:
		program.cpu.b ^= program.cpu.c
	case out:
		value := getComboValue(Combo(literal), *program) % 8
		program.output = append(program.output, value)
		if program.checkOutput {
			outputInts := convertOpcodesToInts(program.opcodes)
			if !matchStartOfSlice(program.output, outputInts) {
				program.cpu.pc = len(program.opcodes)
			}
		}
	case bdv:
		program.cpu.b = calculateDivision(*program, literal)
	case cdv:
		program.cpu.c = calculateDivision(*program, literal)
	default:
		panic("Invalid opcode")
	}
}

func calculateDivision(program Program, literal int) int {
	numerator := float64(program.cpu.a)
	combovalue := getComboValue(Combo(literal), program)
	denominator := math.Pow(2, float64(combovalue))
	return int(numerator / denominator)
}

func getComboValue(combo Combo, program Program) int {
	switch combo {
	case zero:
		return 0
	case one:
		return 1
	case two:
		return 2
	case three:
		return 3
	case a:
		return program.cpu.a
	case b:
		return program.cpu.b
	case c:
		return program.cpu.c
	case reserved:
		panic("Reserved combo")
	default:
		panic("Invalid combo")
	}
}

func Run(input string) string {
	program := parse(input)
	return part1(program) + ", " + part2()
}

func parse(input string) Program {
	blocks := strings.Split(input, "\n\n")
	registers := strings.Split(blocks[0], "\n")
	a, _ := strconv.Atoi(strings.Split(registers[0], ": ")[1])
	b, _ := strconv.Atoi(strings.Split(registers[1], ": ")[1])
	c, _ := strconv.Atoi(strings.Split(registers[2], ": ")[1])
	opcodes := make([]Opcode, 0)
	program := strings.Split(blocks[1], ": ")[1]
	for _, code := range strings.Split(program, ",") {
		opcode, _ := strconv.Atoi(code)
		opcodes = append(opcodes, Opcode(opcode))
	}

	return Program{Cpu{a, b, c, 0}, opcodes, make([]int, 0), false}
}
