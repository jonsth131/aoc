module Tests.Day03Tests

open AoC2020
open Xunit

let input: string[] =
    [| "..##......."; "#...#...#.."; ".#....#..#."; "..#.#...#.#"; ".#...##..#."; "..#.##....."; ".#.#.#....#"; ".#........#"; "#.##...#..."; "#...##....#"; ".#..#...#.#" |]

[<Theory>]
[<InlineData("..##.......", 0, false)>]
[<InlineData("..##.......", 1, false)>]
[<InlineData("..##.......", 2, true)>]
[<InlineData("..##.......", 3, true)>]
[<InlineData("..##.......", 4, false)>]
[<InlineData("..##.......", 11, false)>]
[<InlineData("..##.......", 12, false)>]
[<InlineData("..##.......", 13, true)>]
[<InlineData("..##.......", 14, true)>]
let ``Should check hit`` (row, pos, expected) =
    Assert.Equal(expected, Day03.checkHit pos row)

[<Theory>]
[<InlineData(1, 1, 2)>]
[<InlineData(3, 1, 7)>]
[<InlineData(5, 1, 3)>]
[<InlineData(7, 1, 4)>]
[<InlineData(1, 2, 2)>]
let ``Should calculate hits`` (right, down, expected) =
    Assert.Equal(expected, Day03.checkHits right down input)

[<Theory>]
[<InlineData(0, 1, true)>]
[<InlineData(1, 1, true)>]
[<InlineData(0, 2, true)>]
[<InlineData(1, 2, false)>]
[<InlineData(2, 2, true)>]
let ``Should get down step`` (currRow, down, expected) =
    Assert.Equal(expected, Day03.checkDownStep currRow down)

[<Fact>]
let ``Should calculate all slopes`` () =
    let actual =
        (Day03.checkHits 1 1 input)
        * (Day03.checkHits 3 1 input)
        * (Day03.checkHits 5 1 input)
        * (Day03.checkHits 7 1 input)
        * (Day03.checkHits 1 2 input)
    Assert.Equal(336, actual)

[<Fact>]
let ``Should get correct answer for part1`` () =
    Assert.Equal(230, Day03.part1)
    
[<Fact>]
let ``Should get correct answer for part2`` () =
    Assert.Equal(9533698720UL, Day03.part2)