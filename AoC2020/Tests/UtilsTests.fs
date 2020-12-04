module Tests.UtilsTests

open AoC2020
open Xunit

[<Fact>]
let ``Should count occurances of a in string abcde`` () =
    Assert.Equal(1, Utils.countOccurances "abcde" 'a')

[<Fact>]
let ``Should count occurances of c in string ccccccccc`` () =
    Assert.Equal(9, Utils.countOccurances "ccccccccc" 'c')
