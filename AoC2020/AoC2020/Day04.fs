module AoC2020.Day04

open System
open System.Text.RegularExpressions

type Passport =
    { BirthYear: int
      IssueYear: int
      ExpirationYear: int
      Height: string
      HairColor: string
      EyeColor: string
      PassportId: string }

let input = Utils.readInputAsStringBlocks "Day04.txt"

let parseLines (input: string[]) =
    input
    |> Array.map (fun x -> x.Replace(Environment.NewLine, " "))

let parseField (input: string) =
    let field = input.Split(":")
    match field.Length with
    | 2 -> field.[0], field.[1]
    | _ -> "", ""

let parseLine (input: string) =
    input.Split(" ")
    |> Array.fold (fun result (line: string) ->
        let (key, value) = parseField line
        match key with
        | "byr" -> { result with BirthYear = value |> int }
        | "iyr" -> { result with IssueYear = value |> int }
        | "eyr" ->
            { result with
                  ExpirationYear = value |> int }
        | "hgt" -> { result with Height = value }
        | "hcl" -> { result with HairColor = value }
        | "ecl" -> { result with EyeColor = value }
        | "pid" -> { result with PassportId = value }
        | _ -> (result))
           { BirthYear = 0
             IssueYear = 0
             ExpirationYear = 0
             Height = ""
             HairColor = ""
             EyeColor = ""
             PassportId = "" }

let naiveValidation (passport: Passport) =
    (passport.BirthYear <> 0)
    && (passport.IssueYear <> 0)
    && (passport.ExpirationYear <> 0)
    && (passport.Height <> "")
    && (passport.HairColor <> "")
    && (passport.EyeColor <> "")
    && (passport.PassportId <> "")

let validBirthYear year = year >= 1920 && year <= 2002

let validIssueYear year = year >= 2010 && year <= 2020

let validExpirationYear year = year >= 2020 && year <= 2030

let validHairColor color = Regex.IsMatch(color, "^[#][0-9a-f]{6}$")

let validEyeColor color =
    match color with
    | "amb"
    | "blu"
    | "brn"
    | "gry"
    | "grn"
    | "hzl"
    | "oth" -> true
    | _ -> false

let validPassportId id = Regex.IsMatch(id, "^[[0-9]{9}$")

let validHeight height =
    let matched =
        Regex.Match(height, "(?<height>[0-9]{2,3})(?<type>in|cm)")

    try
        match (int matched.Groups.["height"].Value, matched.Groups.["type"].Value) with
        | (n, "in") -> n >= 59 && n <= 76
        | (n, "cm") -> n >= 150 && n <= 193
        | _ -> false
    with :? FormatException -> false

let extendedValidation (passport: Passport) =
    naiveValidation passport
    && validBirthYear passport.BirthYear
    && validIssueYear passport.IssueYear
    && validExpirationYear passport.ExpirationYear
    && validHairColor passport.HairColor
    && validEyeColor passport.EyeColor
    && validPassportId passport.PassportId
    && validHeight passport.Height

let validatePassport input validation =
    parseLines input
    |> Array.map parseLine
    |> Array.filter validation
    |> Array.length

let part1 = validatePassport input naiveValidation

let part2 =
    validatePassport input extendedValidation
