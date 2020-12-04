module AoC2020.Utils

open System.IO

let readInput fileName = File.ReadAllLines("Inputs/" + fileName)

let readInputAsInts fileName = readInput fileName |> Array.map int

let readInputAsString fileName = File.ReadAllText("Inputs/" + fileName)

let countOccurances input toMatch =
    input
    |> Seq.filter (fun x' -> x' = toMatch)
    |> Seq.length
