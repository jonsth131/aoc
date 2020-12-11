module AoC2020.Day11

type Cell =
    | Occupied
    | Free
    | Floor

type Seats =
    { Rows: int
      Columns: int
      Cells: Cell list }

let input = Utils.readInput "Day11.txt"

let createRow (row: string) =
    row
    |> Seq.fold (fun (result: Cell list) character ->
        match character with
        | '.' -> result @ [ Floor ]
        | 'L' -> result @ [ Free ]
        | '#' -> result @ [ Occupied ]
        | _ -> invalidArg "Character" "Invalid") []

let createModel (input: string array) =
    let columns = input.[0].Length
    let rows = input.Length

    input
    |> Array.fold (fun result row ->
        { result with
              Cells = result.Cells @ createRow row })
           { Rows = rows
             Columns = columns
             Cells = [] }

let toXy index seats =
    (index % (seats.Columns), index / (seats.Columns))

let getCellAt x y seats =
    if x < 0
       || y < 0
       || x > seats.Columns - 1
       || y > seats.Rows - 1 then
        None
    else
        let index = y * seats.Columns + x
        if index < 0 then None else Some seats.Cells.[index]

let getNeighbours index seats =
    let (x, y) = toXy index seats
    [ (getCellAt (x - 1) (y - 1) seats)
      (getCellAt x (y - 1) seats)
      (getCellAt (x + 1) (y - 1) seats)
      (getCellAt (x - 1) (y) seats)
      (getCellAt (x + 1) (y) seats)
      (getCellAt (x - 1) (y + 1) seats)
      (getCellAt x (y + 1) seats)
      (getCellAt (x + 1) (y + 1) seats) ]

let getUpdatedCell index seats =
    let currentCell = seats.Cells.[index]

    let rule1 =
        currentCell = Free
        && (getNeighbours index seats
           |> Seq.filter (fun x -> x.IsSome && x.Value = Occupied)
           |> Seq.length = 0)

    let rule2 =
        currentCell = Occupied
        && (getNeighbours index seats
           |> Seq.filter (fun x -> x.IsSome && x.Value = Occupied)
           |> Seq.length
           >= 4)

    if rule2 then Free
    elif rule1 then Occupied
    else currentCell

let updateSeats seats =
    seats.Cells
    |> List.indexed
    |> List.fold (fun result (index, _) ->
        { result with
              Cells = result.Cells @ [ getUpdatedCell index seats ] })
           { Rows = seats.Rows
             Columns = seats.Columns
             Cells = [] }

let countSeats cell input =
    input.Cells
    |> Seq.filter (fun x -> x = cell)
    |> Seq.length

let rec updateUntilNoChanges seats =
    let newSeats = updateSeats seats
    if Utils.compare seats.Cells newSeats.Cells then seats else updateUntilNoChanges newSeats

let part1 =
    input
    |> createModel
    |> updateUntilNoChanges
    |> countSeats Occupied

let part2 = 2
