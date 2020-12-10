module Tests.Day10Tests

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
let ``Should get correct multiple part1`` () =
    let actual = Day10.countMatches 4 [ 5; 6; 7 ]
    Assert.Equal(3, actual)

[<Fact>]
let ``Should get correct multiple part2`` () =
    let actual = Day10.countMatches 5 [ 6; 7; 10 ]
    Assert.Equal(2, actual)

[<Fact>]
let ``Should get correct answer for part1`` () = Assert.Equal(2232, Day10.part1)
