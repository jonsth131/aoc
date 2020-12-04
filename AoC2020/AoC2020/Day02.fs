module AoC2020.Day02

open System.Text.RegularExpressions

type PasswordPolicy =
    { First: int
      Last: int
      Character: char
      Password: string }

let input = Utils.readInput "Day02.txt"

let parsePasswordPolicy input: PasswordPolicy =
    let pattern =
        "(?<first>\d+)-(?<last>\d+) (?<character>.): (?<password>.*)"

    let matched = Regex.Match(input, pattern)
    { First = int matched.Groups.["first"].Value
      Last = int matched.Groups.["last"].Value
      Character = char matched.Groups.["character"].Value
      Password = string matched.Groups.["password"].Value }

let validPasswordForPolicy1 (input: PasswordPolicy) =
    let occurances =
        Utils.countOccurances input.Password input.Character

    (occurances >= input.First)
    && (occurances <= input.Last)

let validatePasswordForPolicy1 input =
    input
    |> parsePasswordPolicy
    |> validPasswordForPolicy1

let validPasswordForPolicy2 (input: PasswordPolicy) =
    let first = input.Password.[input.First - 1]
    let last = input.Password.[input.Last - 1]
    (first <> last)
    && ((first = input.Character && last <> input.Character)
        || (first <> input.Character && last = input.Character))

let validatePasswordForPolicy2 input =
    input
    |> parsePasswordPolicy
    |> validPasswordForPolicy2

let part1 =
    input
    |> Array.filter validatePasswordForPolicy1
    |> Array.length

let part2 =
    input
    |> Array.filter validatePasswordForPolicy2
    |> Array.length
