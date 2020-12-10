module AoC2020.Day10

let input = Utils.readInputAsInts "Day10.txt"

let prepareInput max input =
    input |> Seq.append [ 0; max ] |> Seq.sort

let countDifferences diff input =
    input
    |> prepareInput ((input |> Seq.max) + 3)
    |> Seq.pairwise
    |> Seq.filter (fun (first, second) -> second = first + diff)
    |> Seq.length

let countMatches toMatch input =
    input
    |> Seq.filter (fun x ->
        toMatch
        + 1 = x
        || toMatch + 2 = x
        || toMatch + 3 = x)
    |> Seq.length

let rec count (result: Map<int, int>) (input: int list) =
    if input.Length = 1 then
        result |> Map.add input.Head 1
    else
        let matches = countMatches input.Head input.Tail
        count (result |> Map.add input.Head (matches)) input.Tail

let part1 =
    (input |> countDifferences 1)
    * (input |> countDifferences 3)

let part2 = 2
