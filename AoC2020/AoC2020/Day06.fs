module AoC2020.Day06

open System

let input = Utils.readInputAsStringBlocks "Day06.txt"
    
let createSet (input: string) =
    input
    |> Utils.splitToArray
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
    |> Seq.map (fun x -> x.Replace(Environment.NewLine, ""))
    |> Seq.map (fun x -> Seq.distinct x |> Seq.length)
    |> Seq.sum

let part2 =
    input
    |> sumAnswers