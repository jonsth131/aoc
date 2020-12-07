module AoC2020.Day07

open System.Text.RegularExpressions

type Bag = { Name: string; Count: int }

let input = Utils.readInput "Day07.txt"

let parseRules (input: string []) =
    let parseName (input: string) =
        Regex.Match(input, "^(?<bagName>\w+ \w+)").Groups.["bagName"].Value

    let parseContainedBags (input: string) =
        Regex.Matches(input, "(?<containedBagCount>\d) (?<containedBag>\w+ \w+)")
        |> Seq.map (fun x ->
            { Name = string x.Groups.["containedBag"].Value
              Count = int x.Groups.["containedBagCount"].Value })
        |> Seq.toList

    let parseRule (input: string) =
        (parseName input), (parseContainedBags input)

    input
    |> Seq.map parseRule
    |> Seq.fold (fun (result: Map<string, Bag list>) (name, bags) -> result.Add(name, bags)) Map.empty

let getBagNames (bagName: string) (rules: Map<string, Bag list>) =
    let bagNameExistsInList (bagName: string) (bags: Bag list) =
        bags
        |> Seq.filter (fun x -> x.Name = bagName)
        |> Seq.length > 0

    let canHoldBag (bags: Map<string, Bag list>) (bagName: string) =
        bags
        |> Map.filter (fun _ bags -> bagNameExistsInList bagName bags)
        |> Map.fold (fun keys key _ -> key :: keys) []

    canHoldBag rules bagName

let rec getBags (bagNames: string list) (rules: Map<string, Bag list>) =
    let bags =
        bagNames
        |> List.map (fun x -> getBagNames x rules)
        |> List.concat
        |> List.distinct

    let allFoundNames = bags @ bagNames |> List.distinct
    if bagNames.Length = allFoundNames.Length
    then bagNames
    else getBags (bags @ bagNames |> List.distinct) rules

let getCount (input: string) (bags: Map<string, Bag list>) =
    bags
    |> getBags (getBagNames input bags)
    |> Seq.length

let getTotalBagCount (input: string) (rules: Map<string, Bag list>) =
    let rec getSums (color: string) (multiplier: int) =
        seq {
            let bags = rules.[color]
            yield! (bags |> Seq.map (fun x -> x.Count * multiplier))
            for bag in bags do
                yield! getSums bag.Name (bag.Count * multiplier)
        }

    getSums input 1 |> Seq.sum

let part1 =
    input |> parseRules |> getCount "shiny gold"

let part2 =
    input
    |> parseRules
    |> getTotalBagCount "shiny gold"
