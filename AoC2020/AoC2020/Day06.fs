module AoC2020.Day06

open System

let input = Utils.readInputAsString "Day06.txt"

let parseLines (input: string) =
    input.Trim().Split("\r\n\r\n")
    
let createSet (input: string) =
    input.Trim().Split("\r\n")
    |> Seq.map Set.ofSeq

let countAnswers (input: string) =
    input
    |> createSet
    |> Set.intersectMany
    |> Set.count

let sumAnswers (input: string[]) =
    input
    |> Seq.map countAnswers
    |> Seq.sum

let part1 =
    input
    |> parseLines
    |> Seq.map (fun x -> x.Replace(Environment.NewLine, ""))
    |> Seq.map (fun x -> Seq.distinct x |> Seq.length)
    |> Seq.sum

let part2 =
    input
    |> parseLines
    |> sumAnswers