module Tests.Day06Tests

open AoC2020
open Xunit

[<Fact>]
let ``Create set`` () =
    Assert.Equal(2, Day06.createSet "ab\r\nac" |> Seq.length)

[<Theory>]
[<InlineData("abc", 3)>]
[<InlineData("a\r\nb\r\nc", 0)>]
[<InlineData("ab\r\nac", 1)>]
[<InlineData("a\r\na\r\na\r\na", 1)>]
[<InlineData("b", 1)>]
[<InlineData("tg\r\ngt", 2)>]
let ``Count answers`` (input, expected) =
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