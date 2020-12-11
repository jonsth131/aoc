module Tests.Day11Tests

open AoC2020
open Xunit

let input =
    [| "L.LL.LL.LL"
       "LLLLLLL.LL"
       "L.L.L..L.."
       "LLLL.LL.LL"
       "L.LL.LL.LL"
       "L.LLLLL.LL"
       "..L.L....."
       "LLLLLLLLLL"
       "L.LLLLLL.L"
       "L.LLLLL.LL" |]

[<Fact>]
let ``Should create seats model`` () =
    let actual = Day11.createModel input
    let floorCount = actual.Cells |> Seq.filter (fun x -> x = Day11.Floor) |> Seq.length
    let freeCount = actual.Cells |> Seq.filter (fun x -> x = Day11.Free) |> Seq.length
    Assert.Equal(10, actual.Columns)
    Assert.Equal(10, actual.Rows)
    Assert.Equal(100, actual.Cells.Length)
    Assert.Equal(29, floorCount)
    Assert.Equal(71, freeCount)

[<Fact>]
let ``Should create row`` () =
    let actual = Day11.createRow "L.LL.LL.LL"

    let expected =
        [| Day11.Free
           Day11.Floor
           Day11.Free
           Day11.Free
           Day11.Floor
           Day11.Free
           Day11.Free
           Day11.Floor
           Day11.Free
           Day11.Free |]

    Assert.Equal(expected, actual)

[<Fact>]
let ``Should get x and y`` () =
    let actual = Day11.toXy 23 {Columns = 10; Rows = 10; Cells = []}
    Assert.Equal((3, 2), actual)

[<Fact>]
let ``Should get x and y for index 0`` () =
    let actual = Day11.toXy 0 {Columns = 10; Rows = 10; Cells = []}
    Assert.Equal((0, 0), actual)

[<Fact>]
let ``Should get x and y for index in non rectangular`` () =
    let actual = Day11.toXy 94 {Columns = 94; Rows = 93; Cells = []}
    Assert.Equal((0, 1), actual)

[<Fact>]
let ``Should get x and y for index in non rectangular last element`` () =
    let actual = Day11.toXy 8741 {Columns = 94; Rows = 93; Cells = []}
    Assert.Equal((93, 92), actual)

[<Fact>]
let ``Should get x and y for index 9`` () =
    let actual = Day11.toXy 9 {Columns = 10; Rows = 10; Cells = []}
    Assert.Equal((9, 0), actual)

[<Fact>]
let ``Should get x and y for index 7`` () =
    let actual = Day11.toXy 7 {Columns = 5; Rows = 5; Cells = []}
    Assert.Equal((2, 1), actual)

[<Fact>]
let ``Should get x and y for index 1`` () =
    let actual = Day11.toXy 1 {Columns = 5; Rows = 5; Cells = []}
    Assert.Equal((1, 0), actual)

[<Fact>]
let ``Should get cell at 0 0`` () =
    let actual = Day11.getCellAt 0 0 (Day11.createModel input)
    Assert.Equal(Day11.Free, actual.Value)

[<Fact>]
let ``Should get cell at 9 0`` () =
    let actual = Day11.getCellAt 9 0 (Day11.createModel input)
    Assert.Equal(Day11.Free, actual.Value)

[<Fact>]
let ``Should get cell at 1 0`` () =
    let actual = Day11.getCellAt 1 0 (Day11.createModel input)
    Assert.Equal(Day11.Floor, actual.Value)

[<Fact>]
let ``Should get cell at 7 1`` () =
    let actual = Day11.getCellAt 7 1 (Day11.createModel input)
    Assert.Equal(Day11.Floor, actual.Value)

[<Fact>]
let ``Should get cell at -1 1`` () =
    let actual = Day11.getCellAt -1 1 (Day11.createModel input)
    Assert.Equal(true, actual.IsNone)

[<Fact>]
let ``Should get neighbours for first index`` () =
    let actual = Day11.getNeighbours 0 (Day11.createModel input) |> Seq.filter (fun x -> x.IsSome) |> Seq.length
    Assert.Equal(3, actual)

[<Fact>]
let ``Should get neighbours for last index on first row`` () =
    let actual = Day11.getNeighbours 9 (Day11.createModel input) |> Seq.filter (fun x -> x.IsSome) |> Seq.length
    Assert.Equal(3, actual)

[<Fact>]
let ``Should get full neighbours`` () =
    let actual = Day11.getNeighbours 12 (Day11.createModel input) |> Seq.filter (fun x -> x.IsSome) |> Seq.length
    Assert.Equal(8, actual)

[<Fact>]
let ``Should update seats`` () =
    let actual = Day11.updateSeats (Day11.createModel input)
    Assert.Equal(0, actual.Cells |> Seq.filter (fun x -> x = Day11.Free) |> Seq.length)

[<Fact>]
let ``Should update seats twice`` () =
    let actual = Day11.updateSeats (Day11.createModel input) |> Day11.updateSeats
    Assert.Equal(51, actual.Cells |> Seq.filter (fun x -> x = Day11.Free) |> Seq.length)

[<Fact>]
let ``Should update until no further changes`` () =
    let actual = Day11.updateUntilNoChanges (Day11.createModel input)
    Assert.Equal(37, Day11.countSeats Day11.Occupied actual)

[<Fact>]
let ``Should create corerct model`` () =
    let actual = Utils.readInput "Day11.txt" |> Day11.createModel
    Assert.Equal(8742, actual.Cells.Length)

[<Fact>]
let ``Should get last cell`` () =
    let model = Utils.readInput "Day11.txt" |> Day11.createModel
    let actual = Day11.getCellAt 93 92 model
    Assert.Equal(Day11.Free, actual.Value)

[<Fact>]
let ``Should get neighbours for last cell`` () =
    let model = Utils.readInput "Day11.txt" |> Day11.createModel
    let actual = Day11.getNeighbours (model.Cells.Length - 1) model |> Seq.filter (fun x -> x.IsSome && x.Value = Day11.Free) |> Seq.length
    Assert.Equal(3, actual)
