module AoC2020.Day09

let input =
    Utils.readInputAsUint64 "Day09.txt"
    |> Array.toList

let getSums (value: uint64) (preamble: uint64 list) = preamble |> Seq.map (fun x -> x + value)

let isValidValue (value: uint64) (preamble: uint64 list) =
    preamble
    |> Seq.map (fun value ->
        getSums
            value
            (preamble
             |> List.filter (fun current -> current <> value)))
    |> Seq.concat
    |> Seq.contains value

let findInvalidElement (preambleLength: int) (input: uint64 list) =
    input
    |> Seq.indexed
    |> Seq.fold (fun result (i, x) ->
        if i < preambleLength then result
        elif isValidValue x (input.[i - preambleLength - 1..i - 1]) then result
        else x) 0UL

let rec checkValue (value: uint64) first last (input: uint64 list) =
    let totalSum = input.[first..last] |> List.sum
    if totalSum = value then input.[first..last]
    elif totalSum > value then checkValue value (first + 1) (first + 2) input
    elif totalSum < value then checkValue value first (last + 1) input
    else []

let findSumElements (input: uint64 list) (value: uint64) =
    input |> checkValue value 0 1 |> List.sort

let part1 = input |> findInvalidElement 25

let part2 =
    let foundElements = part1 |> findSumElements input
    foundElements.Head
    + foundElements.[foundElements.Length - 1]
