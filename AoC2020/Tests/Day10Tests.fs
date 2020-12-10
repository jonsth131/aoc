module Tests.Day10Tests

open System.Collections.Generic
open AoC2020
open Xunit

let input =
    [| 28
       33
       18
       42
       31
       14
       46
       20
       48
       47
       24
       23
       49
       45
       19
       38
       39
       11
       1
       32
       25
       35
       8
       17
       7
       9
       4
       2
       34
       10
       3 |]

let smallInput =
    [| 16
       10
       15
       5
       1
       11
       7
       19
       6
       12
       4 |]

[<Theory>]
[<InlineData(1, 22)>]
[<InlineData(3, 10)>]
let ``Should count differences`` (diff, expected) =
    Assert.Equal(expected, Day10.countDifferences diff input)

[<Fact>]
let ``Can reach`` () =
    let actual = Day10.canReach 1 3
    Assert.Equal(true, actual)

[<Fact>]
let ``Should get permutations for small input`` () =
    let dict = Dictionary<int, uint64>()
    dict.Add(0, 1UL)
    let actual = smallInput |> Day10.prepareInput 22 |> Seq.toList |> Day10.countPermutations dict 0
    Assert.Equal(8UL, actual)
    
[<Fact>]
let ``Should get permutations for input`` () =
    let dict = Dictionary<int, uint64>()
    dict.Add(0, 1UL)
    let actual = input |> Day10.prepareInput 52 |> Seq.toList |> Day10.countPermutations dict 0
    Assert.Equal(19208UL, actual)

[<Fact>]
let ``Should get correct answer for part1`` () = Assert.Equal(2232, Day10.part1)

[<Fact>]
let ``Should get correct answer for part2`` () = Assert.Equal(173625106649344UL, Day10.part2)