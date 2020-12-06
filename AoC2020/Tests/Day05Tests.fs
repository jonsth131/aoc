module Tests.Day05Tests

open AoC2020
open Xunit

[<Fact>]
let ``Should decode boarding code`` () =
    Assert.Equal((44, 5), Day05.decode "FBFBBFFRLR")
    
[<Fact>]
let ``Should take lower part of values`` () =
    let actual = Day05.getValue 'L' [|0..19|]
    let expected = 9
    Assert.Equal(expected, actual.[actual.Length - 1])
    
[<Fact>]
let ``Should take upper part of values`` () =
    let actual = Day05.getValue 'R' [|0..19|]
    let expected = 19
    Assert.Equal(expected, actual.[actual.Length - 1])
    
[<Fact>]
let ``Should calculate values for FBFBBFF`` () =
    let actual = Day05.calculateValue "FBFBBFF" [|0..127|]
    let expected = 44
    Assert.Equal(expected, actual)
    
[<Fact>]
let ``Should calculate values for RLR`` () =
    let actual = Day05.calculateValue "RLR" [|0..7|]
    let expected = 5
    Assert.Equal(expected, actual)
    
[<Fact>]
let ``Should calculate seat id`` () =
    Assert.Equal(357, Day05.calculateSeatId (44, 5))
    
[<Fact>]
let ``Should find missing element`` () =
    let inputArray = [|100; 101; 103; 104 |]
    let expected = 102
    Assert.Equal(expected, Day05.findGap inputArray)
    
[<Fact>]
let ``Should find missing element with large array`` () =
    let inputArray = [|100; 101; 103; 104; 105; 106; 107; 108; 109; 110; 111 |]
    let expected = 102
    Assert.Equal(expected, Day05.findGap inputArray)
    
[<Fact>]
let ``Should get correct answer for part1`` () =
    Assert.Equal(883, Day05.part1)
    
[<Fact>]
let ``Should get correct answer for part2`` () =
    Assert.Equal(532, Day05.part2)