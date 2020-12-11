module AoC2020.Day11

type Cell =
    | Occupied
    | Free
    | Floor

let input = Utils.readInput "Day11.txt"

let parseChar char =
    match char with
    | '.' -> Floor
    | 'L' -> Free
    | '#' -> Occupied
    | _ -> invalidArg "char" "Invalid character"

let createModel input =
    [ for (y, row) in input |> Seq.indexed do
        for (x, col) in row |> Seq.indexed -> (x, y), parseChar col ]
    |> Map.ofSeq

let stuff = createModel input

let getNeighbours (x, y) model =
    let neighbourPoints = [
        (x - 1, y - 1)
        (x, y - 1)
        (x + 1, y - 1)
        (x - 1, y)
        (x + 1, y)
        (x - 1, y + 1)
        (x, y + 1)
        (x + 1, y + 1)
    ]
    
    neighbourPoints |> Seq.choose (fun x -> model |> Map.tryFind x)

let getUpdatedCell model (xy, cell) =
    let occupied = getNeighbours xy model
                   |> Seq.filter (fun x -> x = Occupied)
                   |> Seq.length

    match cell, occupied with
    | Free, 0 -> (xy, Occupied)
    | Occupied, o when o >= 4 -> (xy, Free)
    | cell, _ -> (xy, cell)
    
let update model =
    model
    |> Map.toSeq
    |> Seq.map (getUpdatedCell model)
    |> Map.ofSeq

let rec updateUntilNoChanges model =
    let newModel = update model
    if newModel = model then model else updateUntilNoChanges newModel
    
let countOccupied model =
    model
    |> Map.toSeq
    |> Seq.filter (fun (_, cell) -> cell = Occupied)
    |> Seq.length
    
let part1 =
    input
    |> createModel
    |> updateUntilNoChanges
    |> countOccupied

let part2 = 2
