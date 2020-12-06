module Tests.Day02Tests

open AoC2020.Day02
open Xunit

[<Fact>]
let ``Should parse password policy`` () =
    let expected = {First = 1; Last = 3; Character = 'a'; Password = "abcde"}
    Assert.Equal(expected, parsePasswordPolicy "1-3 a: abcde")

[<Theory>]
[<InlineData("1-3 a: abcde", true)>]
[<InlineData("1-3 b: cdefg", false)>]
[<InlineData("2-9 c: ccccccccc", true)>]
let ``Should validate first password policy`` (input, expected) =
    Assert.Equal(expected, parsePasswordPolicy input |> validPasswordForPolicy1)

[<Theory>]
[<InlineData("1-3 a: abcde", true)>]
[<InlineData("1-3 b: cdefg", false)>]
[<InlineData("2-9 c: ccccccccc", false)>]
let ``Should validate second password policy`` (input, expected) =
    Assert.Equal(expected, parsePasswordPolicy input |> validPasswordForPolicy2)

[<Fact>]
let ``Should get correct answer for part1`` () =
    Assert.Equal(548, part1)
    
[<Fact>]
let ``Should get correct answer for part2`` () =
    Assert.Equal(502, part2)