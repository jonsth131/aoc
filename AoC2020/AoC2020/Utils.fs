module AoC2020.Utils

open System.IO

let readInput fileName = File.ReadAllLines(Path.Combine("Inputs", fileName))

let readInputAsInts fileName = readInput fileName |> Array.map int

let readInputAsString fileName = File.ReadAllText(Path.Combine("Inputs", fileName))

let countOccurances input toMatch =
    input
    |> Seq.filter (fun x' -> x' = toMatch)
    |> Seq.length
