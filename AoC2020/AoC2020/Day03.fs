module AoC2020.Day03

let input = Utils.readInput "Day03.txt"

let checkHit pos (row: string) = row.[pos % row.Length] = '#'

let checkDownStep currRow down = currRow % down = 0

let checkHits right down input =
    input
    |> Array.indexed
    |> Array.filter (fun (i, _) -> checkDownStep i down)
    |> Array.filter (fun (i, row) -> checkHit (right * (i / down)) row)
    |> Array.length

let part1 = checkHits 3 1 input

let part2 =
    let a = checkHits 1 1 input |> uint64
    let b = checkHits 3 1 input |> uint64
    let c = checkHits 5 1 input |> uint64
    let d = checkHits 7 1 input |> uint64
    let e = checkHits 1 2 input |> uint64

    a * b * c * d * e
