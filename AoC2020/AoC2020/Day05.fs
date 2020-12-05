module AoC2020.Day05

let input = Utils.readInput "Day05.txt"

let getValue code (values: int []) =
    match code with
    | 'F'
    | 'L' -> values.[..(values.Length / 2) - 1]
    | 'B'
    | 'R' -> values.[values.Length / 2..]
    | _ -> values

let rec calculateValue (code: string) (values: int []) =
    if values.Length = 1 then
        values.[0]
    else
        let newValues = getValue code.[0] values
        calculateValue code.[1..] newValues

let decode (code: string) =
    (calculateValue code.[0..6] [| 0 .. 127 |]), (calculateValue code.[7..9] [| 0 .. 7 |])

let calculateSeatId (column, row) = column * 8 + row

let rec findGap (arr: int []) =
    if arr.Length > 2 then
        let middle = arr.Length / 2
        if arr.[middle] = middle + arr.[0] then findGap arr.[middle..] else findGap arr.[..middle]
    else
        arr.[0] + 1

let part1 =
    input
    |> Array.map decode
    |> Array.map calculateSeatId
    |> Array.max

let part2 =
    input
    |> Array.map decode
    |> Array.map calculateSeatId
    |> Array.sort
    |> findGap
