module Tests.Day06Tests

open AoC2020
open Xunit

[<Fact>]
let ``Should create set`` () =
    Assert.Equal(2, Day06.createSet "ab\r\nac" |> Seq.length)

[<Theory>]
[<InlineData("abc", 3)>]
[<InlineData("a\r\nb\r\nc", 0)>]
[<InlineData("ab\r\nac", 1)>]
[<InlineData("a\r\na\r\na\r\na", 1)>]
[<InlineData("b", 1)>]
[<InlineData("tg\r\ngt", 2)>]
let ``Should count answers`` (input, expected) =
    Assert.Equal(expected, Day06.countAnswers input)

[<Fact>]
let ``Should sum answer count`` () =
    let answers =
        [| "abc"
           "a\r\nb\r\nc"
           "ab\r\nac"
           "a\r\na\r\na\r\na"
           "b" |]

    Assert.Equal(6, Day06.sumAnswers answers)
    
[<Fact>]
let ``Should get correct answer for part1`` () =
    Assert.Equal(6532, Day06.part1)
    
[<Fact>]
let ``Should get correct answer for part2`` () =
    Assert.Equal(3427, Day06.part2)