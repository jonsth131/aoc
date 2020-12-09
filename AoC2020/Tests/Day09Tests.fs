module Tests.Day09Tests

open AoC2020
open Xunit

let input: uint64 list =
    [ 35UL
      20UL
      15UL
      25UL
      47UL
      40UL
      62UL
      55UL
      65UL
      95UL
      102UL
      117UL
      150UL
      182UL
      127UL
      219UL
      299UL
      277UL
      309UL
      576UL ]

[<Fact>]
let ``Should find invalid element`` () =
    Assert.Equal(127UL, Day09.findInvalidElement 5 input)

[<Fact>]
let ``Should get sums`` () =
    let expected = [ 3UL; 4UL; 5UL ]
    Assert.Equal(expected, Day09.getSums 1UL [ 2UL; 3UL; 4UL ])

[<Fact>]
let ``Should check if valid value`` () =
    Assert.Equal(true, Day09.isValidValue 5UL [ 2UL; 3UL; 4UL ])

[<Fact>]
let ``Should check if invalid value`` () =
    Assert.Equal(false, Day09.isValidValue 5UL [ 3UL; 4UL; 5UL ])

[<Fact>]
let ``Should find elements that sum up to value`` () =
    let expected = [ 15UL; 25UL; 40UL; 47UL ]
    let actual = Day09.findSumElements input 127UL
    Assert.Equal(expected.Length, actual.Length)
    Assert.Equal(expected.Head, actual.Head)
    Assert.Equal(expected.[expected.Length - 1], actual.[actual.Length - 1])

[<Fact>]
let ``Should get correct answer for part1`` () = Assert.Equal(27911108UL, Day09.part1)

[<Fact>]
let ``Should get correct answer for part2`` () = Assert.Equal(4023754UL, Day09.part2)
