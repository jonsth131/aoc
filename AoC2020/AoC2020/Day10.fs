module AoC2020.Day10

open System.Collections.Generic

let input = Utils.readInputAsInts "Day10.txt"

let prepareInput max input =
    input |> Seq.append [ 0; max ] |> Seq.sort

let countDifferences diff input =
    input
    |> prepareInput ((input |> Seq.max) + 3)
    |> Seq.pairwise
    |> Seq.filter (fun (first, second) -> second = first + diff)
    |> Seq.length

let canReach first second = first - second <= 3

let updateDict first second (dict: Dictionary<int, uint64>) =
    if dict.ContainsKey(first)
    then dict.[first] <- dict.[second] + dict.[first]
    else dict.Add(first, dict.[second])

let rec countPermutations (dict: Dictionary<int, uint64>) index (input: int list) =
    if index = input.Length then
        dict.[input.[index - 1]]
    else
        if (index - 1 >= 0)
           && canReach input.[index] input.[index - 1] then
            updateDict input.[index] input.[index - 1] dict
        if (index - 2 >= 0)
           && canReach input.[index] input.[index - 2] then
            updateDict input.[index] input.[index - 2] dict
        if (index - 3 >= 0)
           && canReach input.[index] input.[index - 3] then
            updateDict input.[index] input.[index - 3] dict
        countPermutations dict (index + 1) input

let part1 =
    (input |> countDifferences 1)
    * (input |> countDifferences 3)

let part2 =
    let dict = Dictionary<int, uint64>()
    dict.Add(0, 1UL)

    input
    |> prepareInput 165
    |> Seq.toList
    |> countPermutations dict 0
