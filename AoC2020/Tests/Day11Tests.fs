module Tests.Day11Tests

open AoC2020
open Xunit

let input =
    [| "L.LL.LL.LL"
       "LLLLLLL.LL"
       "L.L.L..L.."
       "LLLL.LL.LL"
       "L.LL.LL.LL"
       "L.LLLLL.LL"
       "..L.L....."
       "LLLLLLLLLL"
       "L.LLLLLL.L"
       "L.LLLLL.LL" |]

[<Fact>]
let ``Should get answer for test input`` () =
    let actual = Day11.updateUntilNoChanges (Day11.createModel input) |> Day11.countOccupied
    Assert.Equal(37, actual)

[<Fact>]
let ``Should get correct answer for part1`` () = Assert.Equal(2249, Day11.part1)
