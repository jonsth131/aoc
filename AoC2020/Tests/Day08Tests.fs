module Tests.Day08Tests

open AoC2020
open Xunit

let input =
    [ "nop +0"
      "acc +1"
      "jmp +4"
      "acc +3"
      "jmp -3"
      "acc -99"
      "acc +1"
      "jmp -4"
      "acc +6" ]

[<Fact>]
let ``Should parse opcode with negative value`` () =
    let expected: Day08.Instruction = { OpCode = "jmp"; Value = -4 }
    Assert.Equal(expected, Day08.parseOpCode "jmp -4")

[<Fact>]
let ``Should parse opcode with positive value`` () =
    let expected: Day08.Instruction = { OpCode = "jmp"; Value = 4 }
    Assert.Equal(expected, Day08.parseOpCode "jmp +4")

[<Fact>]
let ``Should run jmp instruction`` () =
    let expected: Day08.Vm =
        { InstructionPointer = 10
          Accumulator = 0
          RanInstructions = [ 0 ] }

    Assert.Equal
        (expected,
         Day08.executeInstruction
             { OpCode = "jmp"; Value = 10 }
             { InstructionPointer = 0
               Accumulator = 0
               RanInstructions = [] })

[<Fact>]
let ``Should run acc instruction`` () =
    let expected: Day08.Vm =
        { InstructionPointer = 1
          Accumulator = 10
          RanInstructions = [ 0 ] }

    Assert.Equal
        (expected,
         Day08.executeInstruction
             { OpCode = "acc"; Value = 10 }
             { InstructionPointer = 0
               Accumulator = 0
               RanInstructions = [] })

[<Fact>]
let ``Should run nop instruction`` () =
    let expected: Day08.Vm =
        { InstructionPointer = 1
          Accumulator = 0
          RanInstructions = [ 0 ] }

    Assert.Equal
        (expected,
         Day08.executeInstruction
             { OpCode = "nop"; Value = 10 }
             { InstructionPointer = 0
               Accumulator = 0
               RanInstructions = [] })

[<Fact>]
let ``Should run code`` () =
    Assert.Equal
        (5,
         Day08.runCode
             { InstructionPointer = 0
               Accumulator = 0
               RanInstructions = [] }
             (input |> Day08.parseOpCodes))

[<Fact>]
let ``Should run code with patch`` () =
    Assert.Equal
        (8,
         Day08.runWithPatch
             { InstructionPointer = 0
               Accumulator = 0
               RanInstructions = [] }
             (input |> Day08.parseOpCodes))

[<Fact>]
let ``Should get correct answer for part1`` () = Assert.Equal(2014, Day08.part1)

[<Fact>]
let ``Should get correct answer for part2`` () = Assert.Equal(2251, Day08.part2)
