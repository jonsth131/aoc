module AoC2020.Day08

open System.Text.RegularExpressions

type Instruction = { OpCode: string; Value: int }

type Vm =
    { InstructionPointer: int
      Accumulator: int
      RanInstructions: int list }

let input =
    Utils.readInput "Day08.txt" |> Array.toList

let parseOpCode input =
    let matches =
        Regex.Match(input, "^(?<opCode>nop|jmp|acc) (?<value>[+|-]\d+)$")

    { OpCode = string matches.Groups.["opCode"].Value
      Value = int matches.Groups.["value"].Value }

let parseOpCodes (input: string list) = input |> List.map parseOpCode

let executeInstruction instruction vm =
    match instruction.OpCode with
    | "jmp" ->
        { vm with
              InstructionPointer = vm.InstructionPointer + instruction.Value
              RanInstructions = List.append [ vm.InstructionPointer ] vm.RanInstructions }
    | "acc" ->
        { vm with
              InstructionPointer = vm.InstructionPointer + 1
              RanInstructions = List.append [ vm.InstructionPointer ] vm.RanInstructions
              Accumulator = vm.Accumulator + instruction.Value }
    | "nop" ->
        { vm with
              InstructionPointer = vm.InstructionPointer + 1
              RanInstructions = List.append [ vm.InstructionPointer ] vm.RanInstructions }
    | _ -> invalidArg "OpCode" "Invalid op code"

let rec runCode vm (instructions: Instruction list) =
    if List.contains vm.InstructionPointer vm.RanInstructions then
        vm.Accumulator
    else
        let currentInstruction = instructions.[vm.InstructionPointer]
        runCode (executeInstruction currentInstruction vm) instructions

let patchInstruction (instruction: Instruction) =
    match instruction.OpCode with
    | "jmp" -> { instruction with OpCode = "nop" }
    | "nop" -> { instruction with OpCode = "jmp" }
    | _ -> instruction

let getCandidates (ranInstructions: int list) (instructions: Instruction list) =
    instructions
    |> List.indexed
    |> List.filter (fun (i, _) -> List.contains i ranInstructions)
    |> List.filter (fun (_, x) -> x.OpCode = "nop" || x.OpCode = "jmp")
    |> List.map (fun (i, _) -> i)

let rec tryPatch (vm: Vm) (instructions: Instruction list) =
    if vm.InstructionPointer = (instructions.Length)
    then (true, vm)
    elif List.contains vm.InstructionPointer vm.RanInstructions
    then (false, vm)
    else tryPatch (executeInstruction instructions.[vm.InstructionPointer] vm) instructions

let runWithPatch (vm: Vm) (instructions: Instruction list) =
    let (success, vmState) = tryPatch vm instructions
    if success then
        vmState.Accumulator
    else
        getCandidates vmState.RanInstructions instructions
        |> List.map (fun x ->
            instructions.[..x - 1]
            @ [ patchInstruction instructions.[x] ]
            @ instructions.[x + 1..])
        |> List.map (fun x ->
            tryPatch
                { InstructionPointer = 0
                  Accumulator = 0
                  RanInstructions = [] }
                x)
        |> List.filter (fun (success, vm) -> success = true)
        |> List.map (fun (_, vm) -> vm)
        |> List.head
        |> fun x -> x.Accumulator

let part1 =
    input
    |> parseOpCodes
    |> runCode
        { InstructionPointer = 0
          Accumulator = 0
          RanInstructions = [] }

let part2 =
    input
    |> parseOpCodes
    |> runWithPatch
        { InstructionPointer = 0
          Accumulator = 0
          RanInstructions = [] }
