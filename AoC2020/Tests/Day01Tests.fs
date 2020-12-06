module Tests.Day01Tests

open AoC2020
open Xunit

[<Fact>]
let ``Should find sum of two values`` () =
    let input = [| 1721; 979; 366; 299; 675; 1456 |]
    Assert.Equal(514579, Day01.findTwoInArray (input |> Array.sort) 2020 0 (input.Length - 1))

[<Fact>]
let ``Should find sum of three values`` () =
    let input = [| 1721; 979; 366; 299; 675; 1456 |]
    Assert.Equal(241861950, Day01.findThreeInArray (input |> Array.sort) 2020 0 1 (input.Length - 1))

[<Fact>]
let ``Should get correct answer for part1`` () =
    Assert.Equal(974304, Day01.part1)
    
[<Fact>]
let ``Should get correct answer for part2`` () =
    Assert.Equal(236430480, Day01.part2)