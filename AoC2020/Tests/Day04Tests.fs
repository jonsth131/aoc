module Tests.Day04Tests

open AoC2020
open Xunit

let input = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in" |> Utils.readBlock


[<Fact>]
let ``Should parse lines`` () =
    let actual = Day04.parseLines input
    Assert.Equal(4, actual |> Array.length)

[<Fact>]
let ``Should get number of valid passports`` () =
    Assert.Equal(2, Day04.validatePassport input Day04.naiveValidation)

[<Fact>]
let ``Should parse line to passport`` () =
    let passport =
        Day04.parseLine "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"

    Assert.Equal("gry", passport.EyeColor)
    Assert.Equal("860033327", passport.PassportId)
    Assert.Equal(2020, passport.ExpirationYear)
    Assert.Equal("#fffffd", passport.HairColor)
    Assert.Equal(1937, passport.BirthYear)
    Assert.Equal(2017, passport.IssueYear)
    Assert.Equal("183cm", passport.Height)

[<Theory>]
[<InlineData(1919, false)>]
[<InlineData(2003, false)>]
[<InlineData(1920, true)>]
[<InlineData(2002, true)>]
let ``Should validate birth year`` (input, expected) =
    Assert.Equal(expected, Day04.validBirthYear input)

[<Theory>]
[<InlineData(2009, false)>]
[<InlineData(2021, false)>]
[<InlineData(2010, true)>]
[<InlineData(2020, true)>]
let ``Should validate issue year`` (input, expected) =
    Assert.Equal(expected, Day04.validIssueYear input)

[<Theory>]
[<InlineData(2019, false)>]
[<InlineData(2031, false)>]
[<InlineData(2020, true)>]
[<InlineData(2030, true)>]
let ``Should validate expiration year`` (input, expected) =
    Assert.Equal(expected, Day04.validExpirationYear input)

[<Theory>]
[<InlineData("brown", false)>]
[<InlineData("#12345", false)>]
[<InlineData("#1234567", false)>]
[<InlineData("#12345t", false)>]
[<InlineData("#123456", true)>]
[<InlineData("#abcdef", true)>]
let ``Should validate hair color`` (input, expected) =
    Assert.Equal(expected, Day04.validHairColor input)

[<Theory>]
[<InlineData("brown", false)>]
[<InlineData("amb", true)>]
[<InlineData("blu", true)>]
[<InlineData("brn", true)>]
[<InlineData("gry", true)>]
[<InlineData("grn", true)>]
[<InlineData("hzl", true)>]
[<InlineData("oth", true)>]
let ``Should validate eye color`` (input, expected) =
    Assert.Equal(expected, Day04.validEyeColor input)

[<Theory>]
[<InlineData("test", false)>]
[<InlineData("12345678o", false)>]
[<InlineData("12345678", false)>]
[<InlineData("1234567890", false)>]
[<InlineData("123456789", true)>]
[<InlineData("000000000", true)>]
let ``Should validate passport id`` (input, expected) =
    Assert.Equal(expected, Day04.validPassportId input)

[<Theory>]
[<InlineData("test", false)>]
[<InlineData("190in", false)>]
[<InlineData("190", false)>]
[<InlineData("60in", true)>]
[<InlineData("190cm", true)>]
let ``Should validate height`` (input, expected) =
    Assert.Equal(expected, Day04.validHeight input)

[<Theory>]
[<InlineData("eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926", false)>]
[<InlineData("iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946", false)>]
[<InlineData("hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277", false)>]
[<InlineData("hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007", false)>]
[<InlineData("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f", true)>]
[<InlineData("eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm", true)>]
[<InlineData("hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022", true)>]
[<InlineData("iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719", true)>]
let ``Should validate passport with extended validation`` (input, expected) =
    let passport = Day04.parseLine input
    Assert.Equal(expected, Day04.extendedValidation passport)

[<Fact>]
let ``Should get correct answer for part1`` () =
    Assert.Equal(200, Day04.part1)
    
[<Fact>]
let ``Should get correct answer for part2`` () =
    Assert.Equal(116, Day04.part2)