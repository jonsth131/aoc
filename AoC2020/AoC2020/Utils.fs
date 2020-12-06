module AoC2020.Utils

open System
open System.IO

let countOccurances input toMatch =
    input
    |> Seq.filter (fun x' -> x' = toMatch)
    |> Seq.length

let readBlock (input: string) =
    input.Trim().Split(Environment.NewLine + Environment.NewLine)
    
let splitToArray (input: string) =
    input.Trim().Split(Environment.NewLine)
    
let readInput fileName = File.ReadAllLines(Path.Combine("Inputs", fileName))

let readInputAsInts fileName = readInput fileName |> Array.map int

let readInputAsString fileName = File.ReadAllText(Path.Combine("Inputs", fileName))

let readInputAsStringBlocks filename = filename |> readInputAsString |> readBlock